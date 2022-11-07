-module(unusual_database_program_server).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_info/2, handle_call/3, handle_cast/2]).

start_link(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Port, []).

init(Port) ->
    {ok, _Socket} = gen_udp:open(Port, [binary, {active, once}]),
    {ok, #{}}.

handle_info({udp, Socket, Host, Port, Packet}, Store) ->
    ok = inet:setopts(Socket, [{active, once}]),
    case binary:split(Packet, <<"=">>) of
        [<<"version">>] ->
            gen_udp:send(Socket, Host, Port, <<"version=1.0">>),
            {noreply, Store};
        [Key] ->
            Value = maps:get(Key, Store, <<"">>),
            gen_udp:send(Socket, Host, Port, [Key, $=, Value]),
            {noreply, Store};
        [Key, Value] ->
            {noreply, Store#{Key => Value}}
    end.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.
