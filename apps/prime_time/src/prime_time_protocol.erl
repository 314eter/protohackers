-module(prime_time_protocol).

-behaviour(ranch_protocol).

-export([start_link/3]).
-export([init/2]).

start_link(Ref, Transport, []) ->
    Pid = spawn_link(?MODULE, init, [Ref, Transport]),
    {ok, Pid}.

init(Ref, Transport) ->
    {ok, Socket} = ranch:handshake(Ref),
    accept(Transport, Socket).

accept(Transport, Socket) ->
    accept(Transport, Socket, []).

accept(Transport, Socket, Received) ->
    case Transport:recv(Socket, 0, infinity) of
        {ok, Packet} ->
            [FirstPacket | Packets] = binary:split(Packet, <<"\n">>, [global]),
            accept(Transport, Socket, [Received, FirstPacket], Packets);
        {error, closed} ->
            Transport:close(Socket)
    end.

accept(Transport, Socket, Received, []) ->
    accept(Transport, Socket, Received);
accept(Transport, Socket, Received, [Packet | Packets]) ->
    case catch jiffy:decode(Received, [return_maps]) of
        #{<<"method">> := <<"isPrime">>, <<"number">> := Number} when is_number(Number) ->
            Response = jiffy:encode(#{
                <<"method">> => <<"isPrime">>, <<"prime">> => is_prime(Number)
            }),
            Transport:send(Socket, [Response, $\n]),
            accept(Transport, Socket, Packet, Packets);
        _ ->
            Transport:send(Socket, <<"error">>),
            Transport:close(Socket)
    end.

is_prime(N) when is_integer(N), N > 1 ->
    is_prime(N, 2, trunc(math:sqrt(N)) + 1);
is_prime(_) ->
    false.

is_prime(_, Max, Max) ->
    true;
is_prime(N, Divider, Max) ->
    case N rem Divider of
        0 -> false;
        _ -> is_prime(N, Divider + 1, Max)
    end.
