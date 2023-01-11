-module(job_centre_protocol).

-behaviour(ranch_protocol).
-behaviour(gen_server).

-export([start_link/3]).
-export([init/1, handle_continue/2, handle_info/2, handle_call/3, handle_cast/2]).

start_link(Ref, Transport, []) ->
    gen_server:start_link(?MODULE, {Ref, Transport}, []).

init({Ref, Transport}) ->
    State = #{
        transport => Transport,
        socket => undefined,
        buffer => []
    },
    {ok, State, {continue, {handshake, Ref}}}.

handle_continue({handshake, Ref}, State = #{transport := Transport}) ->
    {ok, Socket} = ranch:handshake(Ref),
    ok = Transport:setopts(Socket, [{active, once}]),
    {noreply, State#{socket := Socket}}.

handle_info(Info, State = #{transport := Transport, socket := Socket}) ->
    {OK, Closed, _, _} = Transport:messages(),
    case Info of
        {OK, Socket, Packet} ->
            ok = Transport:setopts(Socket, [{active, once}]),
            process_buffer(binary:split(Packet, <<"\n">>, [global]), State);
        {Closed, Socket} ->
            ok = Transport:close(Socket),
            {stop, normal, State}
    end.

handle_call(_, _, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

process_buffer([Rest], State = #{buffer := Buffer}) ->
    {noreply, State#{buffer := [Buffer, Rest]}};
process_buffer(
    [Line | Lines], State = #{transport := Transport, socket := Socket, buffer := Buffer}
) ->
    Response =
        try jiffy:decode([Buffer, Line], [return_maps]) of
            Request -> process_request(Request)
        catch
            error:_ -> #{status => error, error => <<"invalid JSON">>}
        end,
    Transport:send(Socket, [jiffy:encode(Response), $\n]),
    process_buffer(Lines, State#{buffer := []}).

process_request(#{
    <<"request">> := <<"put">>, <<"queue">> := Queue, <<"job">> := Job, <<"pri">> := Pri
}) when is_binary(Queue), is_integer(Pri), Pri >= 0 ->
    Id = job_centre_server:put(Queue, Job, Pri),
    #{status => ok, id => Id};
process_request(
    Request = #{<<"request">> := <<"get">>, <<"queues">> := Queues}
) when is_list(Queues) ->
    Wait = maps:get(<<"wait">>, Request, false),
    case lists:all(fun is_binary/1, Queues) andalso is_boolean(Wait) of
        true ->
            case job_centre_server:get(Queues, Wait) of
                {Id, Job, Pri, Queue} ->
                    #{status => ok, id => Id, job => Job, pri => Pri, queue => Queue};
                'no-job' ->
                    #{status => 'no-job'}
            end;
        false ->
            #{status => error, error => <<"invalid request">>}
    end;
process_request(
    #{<<"request">> := <<"delete">>, <<"id">> := Id}
) when is_integer(Id) ->
    #{status => job_centre_server:delete(Id)};
process_request(
    #{<<"request">> := <<"abort">>, <<"id">> := Id}
) when is_integer(Id) ->
    #{status => job_centre_server:abort(Id)};
process_request(_) ->
    #{status => error, error => <<"invalid request">>}.
