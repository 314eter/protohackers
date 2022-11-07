-module(smoke_test_protocol).

-behaviour(ranch_protocol).

-export([start_link/3]).
-export([init/2]).

start_link(Ref, Transport, []) ->
    Pid = spawn_link(?MODULE, init, [Ref, Transport]),
    {ok, Pid}.

init(Ref, Transport) ->
    {ok, Socket} = ranch:handshake(Ref),
    loop(Transport, Socket).

loop(Transport, Socket) ->
    case Transport:recv(Socket, 0, infinity) of
        {ok, Packet} ->
            Transport:send(Socket, Packet),
            loop(Transport, Socket);
        {error, _} ->
            Transport:close(Socket)
    end.
