-module(means_to_an_end_protocol).

-behaviour(ranch_protocol).

-export([start_link/3]).
-export([init/2]).

start_link(Ref, Transport, []) ->
    Pid = spawn_link(?MODULE, init, [Ref, Transport]),
    {ok, Pid}.

init(Ref, Transport) ->
    {ok, Socket} = ranch:handshake(Ref),
    loop(Transport, Socket, gb_trees:empty()).

loop(Transport, Socket, Prices) ->
    case Transport:recv(Socket, 9, infinity) of
        {ok, <<$I:8, Timestamp:4/signed-big-unit:8, Price:4/signed-big-unit:8>>} ->
            loop(Transport, Socket, insert(Timestamp, Price, Prices));
        {ok, <<$Q:8, MinTime:4/signed-big-unit:8, MaxTime:4/signed-big-unit:8>>} ->
            Mean = query(MinTime, MaxTime, Prices),
            Transport:send(Socket, <<Mean:4/signed-big-unit:8>>),
            loop(Transport, Socket, Prices);
        _ ->
            Transport:close(Socket)
    end.

insert(Timestamp, Price, Prices) ->
    gb_trees:insert(Timestamp, Price, Prices).

query(MinTime, MaxTime, Prices) ->
    Iter = gb_trees:iterator_from(MinTime, Prices),
    case summarize(Iter, MaxTime) of
        {0, 0} -> 0;
        {Sum, Count} -> trunc(Sum / Count)
    end.

summarize(Iter, MaxTime) ->
    case gb_trees:next(Iter) of
        {Timestamp, Price, NextIter} when Timestamp =< MaxTime ->
            {Sum, Count} = summarize(NextIter, MaxTime),
            {Sum + Price, Count + 1};
        _ ->
            {0, 0}
    end.
