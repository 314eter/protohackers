-module(job_centre_server).

-behaviour(gen_server).

-export([start_link/0, put/3, get/2, delete/1, abort/1]).
-export([init/1, handle_info/2, handle_call/3, handle_cast/2]).

-record(job, {pri :: non_neg_integer(), id :: integer(), queue :: binary(), task :: term()}).
-type job() :: #job{}.

-type state() :: #{
    queues := #{binary() => gb_sets:set(job())},
    queued := #{integer() => job()},
    clients := #{pid() => sets:set(integer())},
    assigned := #{integer() => {pid(), job()}},
    waiting := #{binary() => [{gen_server:from(), [binary()]}]},
    monitors := #{pid() => reference()}
}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec put(binary(), term(), non_neg_integer()) -> integer().
put(Name, Task, Pri) ->
    gen_server:call(?MODULE, {put, Name, Task, Pri}).

-spec get([binary()], boolean()) -> {integer(), term(), non_neg_integer(), binary()} | 'no-job'.
get(Names, Wait) ->
    case gen_server:call(?MODULE, {get, Names, Wait}, infinity) of
        'no-job' -> 'no-job';
        #job{id = Id, task = Task, pri = Pri, queue = Queue} -> {Id, Task, Pri, Queue}
    end.

-spec delete(integer()) -> ok | 'no-job'.
delete(Id) ->
    gen_server:call(?MODULE, {delete, Id}).

-spec abort(integer()) -> ok | 'no-job'.
abort(Id) ->
    gen_server:call(?MODULE, {abort, Id}).

-spec init([]) -> {ok, state()}.
init([]) ->
    State = #{
        queues => #{},
        queued => #{},
        clients => #{},
        assigned => #{},
        waiting => #{},
        monitors => #{}
    },
    {ok, State}.

-spec handle_info({'DOWN', reference(), process, pid(), _}, state()) -> {noreply, state()}.
handle_info(
    {'DOWN', Monitor, process, Pid, _},
    OldState = #{clients := OldClients, monitors := OldMonitors}
) ->
    {Ids, Clients} = maps:take(Pid, OldClients),
    {Monitor, Monitors} = maps:take(Pid, OldMonitors),
    {noreply,
        sets:fold(
            fun(Id, State = #{assigned := OldAssigned}) ->
                {{Pid, Job}, Assigned} = maps:take(Id, OldAssigned),
                queue(Job, State#{assigned := Assigned})
            end,
            OldState#{clients := Clients, monitors := Monitors},
            Ids
        )}.

-spec handle_call
    ({put, binary(), term(), non_neg_integer()}, gen_server:from(), state()) ->
        {reply, integer(), state()};
    ({get, [binary()], boolean()}, gen_server:from(), state()) ->
        {reply, job() | 'no-job', state()};
    ({delete, integer()}, gen_server:from(), state()) ->
        {reply, job() | 'no-job', state()};
    ({abort, integer()}, gen_server:from(), state()) ->
        {reply, job() | 'no-job', state()}.
handle_call({put, Name, Task, Pri}, _, State) ->
    Id = erlang:unique_integer([monotonic, positive]),
    Job = #job{pri = Pri, id = Id, queue = Name, task = Task},
    {reply, Id, queue(Job, State)};
handle_call({get, Names, Wait}, From = {Pid, _}, State = #{queues := Queues}) ->
    case get_first_job(Names, Queues) of
        'no-job' when not Wait ->
            {reply, 'no-job', State};
        'no-job' ->
            {noreply, wait({From, Names}, State)};
        Job ->
            {reply, Job, assign(Pid, Job, dequeue(Job, State))}
    end;
handle_call({delete, Id}, _, State = #{queued := Queued, assigned := Assigned}) ->
    case maps:get(Id, Queued, 'no-job') of
        'no-job' ->
            case maps:get(Id, Assigned, 'no-job') of
                'no-job' -> {reply, 'no-job', State};
                {Pid, Job} -> {reply, ok, unassign(Pid, Job, State)}
            end;
        Job ->
            {reply, ok, dequeue(Job, State)}
    end;
handle_call({abort, Id}, {Pid, _}, State = #{assigned := Assigned}) ->
    case maps:get(Id, Assigned, 'no-job') of
        {Pid, Job} -> {reply, ok, queue(Job, unassign(Pid, Job, State))};
        _ -> {reply, 'no-job', State}
    end.

handle_cast(_, State) ->
    {noreply, State}.

-spec queue(job(), state()) -> state().
queue(
    Job = #job{id = Id, queue = Name},
    State = #{queues := Queues, queued := Queued, waiting := Waiting}
) ->
    case maps:get(Name, Waiting, []) of
        [Wait = {From = {Pid, _}, _} | _] ->
            gen_server:reply(From, Job),
            assign(Pid, Job, unwait(Wait, State));
        [] ->
            Queue = maps:get(Name, Queues, gb_sets:new()),
            State#{
                queues := Queues#{Name => gb_sets:add(Job, Queue)},
                queued := Queued#{Id => Job}
            }
    end.

-spec dequeue(job(), state()) -> state().
dequeue(Job = #job{id = Id, queue = Name}, State = #{queues := Queues, queued := Queued}) ->
    Queue = gb_sets:delete(Job, maps:get(Name, Queues)),
    State#{
        queues :=
            case gb_sets:is_empty(Queue) of
                true -> maps:remove(Name, Queues);
                false -> Queues#{Name => Queue}
            end,
        queued := maps:remove(Id, Queued)
    }.

-spec assign(pid(), job(), state()) -> state().
assign(
    Pid,
    Job = #job{id = Id},
    State = #{clients := Clients, assigned := Assigned, monitors := OldMonitors}
) ->
    Monitors =
        case maps:get(Pid, OldMonitors, undefined) of
            undefined -> OldMonitors#{Pid => erlang:monitor(process, Pid)};
            _ -> OldMonitors
        end,
    State#{
        clients := Clients#{Pid => sets:add_element(Id, maps:get(Pid, Clients, sets:new()))},
        assigned := Assigned#{Id => {Pid, Job}},
        monitors := Monitors
    }.

-spec unassign(pid(), job(), state()) -> state().
unassign(
    Pid, #job{id = Id}, State = #{clients := Clients, assigned := Assigned, monitors := OldMonitors}
) ->
    Monitors =
        case maps:get(Pid, OldMonitors, undefined) of
            undefined ->
                OldMonitors;
            Monitor ->
                erlang:demonitor(Monitor, [flush]),
                maps:remove(Pid, OldMonitors)
        end,
    State#{
        clients := Clients#{Pid => sets:del_element(Id, maps:get(Pid, Clients))},
        assigned := maps:remove(Id, Assigned),
        monitors := Monitors
    }.

-spec wait({gen_server:from(), [binary()]}, state()) -> state().
wait({From, Names}, State = #{waiting := OldWaiting}) ->
    Waiting = lists:foldl(
        fun(Name, Waiting) ->
            Waits = maps:get(Name, Waiting, []),
            Waiting#{Name => [{From, Names} | Waits]}
        end,
        OldWaiting,
        Names
    ),
    State#{waiting := Waiting}.

-spec unwait({gen_server:from(), [binary()]}, state()) -> state().
unwait(Wait = {_, Names}, State = #{waiting := OldWaiting}) ->
    Waiting = lists:foldl(
        fun(Name, Waiting) ->
            Waits = maps:get(Name, Waiting),
            Waiting#{Name => lists:delete(Wait, Waits)}
        end,
        OldWaiting,
        Names
    ),
    State#{waiting := Waiting}.

-spec get_first_job([binary()], #{binary() => gb_sets:set(job())}) -> any().
get_first_job(Names, Queues) ->
    lists:foldl(
        fun(Name, Job) ->
            Queue = maps:get(Name, Queues, gb_sets:new()),
            case gb_sets:is_empty(Queue) of
                true -> Job;
                false -> max(Job, gb_sets:largest(Queue))
            end
        end,
        'no-job',
        Names
    ).
