-module(insecure_sockets_layer_application).

-behaviour(gen_server).

-export([start_link/1, process/2]).
-export([init/1, handle_call/3, handle_cast/2]).

start_link(Sender) ->
    gen_server:start_link(?MODULE, Sender, []).

process(Sender, Packet) ->
    gen_server:cast(Sender, Packet).

init(Sender) ->
    State = #{sender => Sender, buffer => <<>>},
    {ok, State}.

handle_call(_, _, State) ->
    {noreply, State}.

handle_cast(Packet, State = #{buffer := Buffer}) ->
    handle_buffer(State#{buffer := <<Buffer/binary, Packet/binary>>}).

handle_buffer(State = #{sender := Sender, buffer := Buffer}) ->
    case binary:split(Buffer, <<"\n">>) of
        [_] ->
            {noreply, State};
        [Line, Rest] ->
            Toys = binary:split(Line, <<",">>, [global]),
            {_, Toy} = lists:max(lists:map(fun toy_copies/1, Toys)),
            insecure_sockets_layer_sender:send(Sender, <<Toy/binary, $\n>>),
            handle_buffer(State#{buffer := Rest})
    end.

toy_copies(Toy) ->
    [Count | _] = binary:split(Toy, <<"x">>),
    {binary_to_integer(Count), Toy}.
