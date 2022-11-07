-module(mob_in_the_middle_protocol).

-behaviour(ranch_protocol).
-behaviour(gen_server).

-export([start_link/3]).
-export([init/1, handle_continue/2, handle_info/2, handle_call/3, handle_cast/2]).

start_link(Ref, Transport, {UpstreamAddress, UpstreamPort}) ->
    gen_server:start_link(?MODULE, {Ref, Transport, UpstreamAddress, UpstreamPort}, []).

init({Ref, Transport, UpstreamAddress, UpstreamPort}) ->
    State = #{
        transport => Transport,
        socket => nil,
        upstream_socket => nil,
        buffer => <<>>,
        upstream_buffer => <<>>
    },
    {ok, State, {continue, {Ref, UpstreamAddress, UpstreamPort}}}.

handle_continue({Ref, UpstreamAddress, UpstreamPort}, State = #{transport := Transport}) ->
    {ok, Socket} = ranch:handshake(Ref),
    ok = Transport:setopts(Socket, [{active, once}]),
    {ok, UpstreamSocket} = gen_tcp:connect(UpstreamAddress, UpstreamPort, [binary, {active, once}]),
    {noreply, State#{socket := Socket, upstream_socket := UpstreamSocket}}.

handle_info(
    Info,
    State = #{
        transport := Transport,
        socket := Socket,
        upstream_socket := UpstreamSocket,
        buffer := Buffer,
        upstream_buffer := UpstreamBuffer
    }
) ->
    {OK, Closed, Error, _Passive} = Transport:messages(),
    case Info of
        {OK, Socket, Packet} ->
            ok = Transport:setopts(Socket, [{active, once}]),
            {Data, NewBuffer} = process_buffer(<<Buffer/binary, Packet/binary>>),
            gen_tcp:send(UpstreamSocket, Data),
            {noreply, State#{buffer := NewBuffer}};
        {tcp, UpstreamSocket, Packet} ->
            ok = inet:setopts(UpstreamSocket, [{active, once}]),
            {Data, NewUpstreamBuffer} = process_buffer(<<UpstreamBuffer/binary, Packet/binary>>),
            Transport:send(Socket, Data),
            {noreply, State#{upstream_buffer := NewUpstreamBuffer}};
        {Message, _} when Message =:= Closed; Message =:= tcp_closed ->
            Transport:close(Socket),
            gen_tcp:close(UpstreamSocket),
            {stop, normal, State};
        {Message, _, Reason} when Message =:= Error; Message =:= tcp_error ->
            Transport:close(Socket),
            gen_tcp:close(UpstreamSocket),
            {stop, {Message, Reason}, State}
    end.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

process_buffer(Buffer) ->
    process_lines(binary:split(Buffer, <<"\n">>, [global])).

process_lines([Line]) ->
    {[], Line};
process_lines([Line | Lines]) ->
    {Data, Buffer} = process_lines(Lines),
    {[rewrite(Line), $\n | Data], Buffer}.

rewrite(Line) ->
    re:replace(
        Line,
        "(?<=^| )(7[A-Za-z0-9]{25,34})(?= |$)",
        <<"7YWHMfk9JZe0LM0g1ZauHuiSxhI">>,
        [global]
    ).
