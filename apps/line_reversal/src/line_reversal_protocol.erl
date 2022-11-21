-module(line_reversal_protocol).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_info/2, handle_call/3, handle_cast/2]).

start_link(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Port, []).

init(Port) ->
    {ok, Socket} = gen_udp:open(Port, [binary, {active, 100}, {recbuf, 4194304}]),
    State = #{
        socket => Socket,
        sessions => #{},
        monitors => #{}
    },
    {ok, State}.

handle_info({udp_passive, Socket}, State = #{socket := Socket}) ->
    ok = inet:setopts(Socket, [{active, 100}]),
    {noreply, State};
handle_info(
    {udp, Socket, Host, Port, Packet},
    State = #{socket := Socket, sessions := Sessions}
) ->
    case line_reversal_utils:parse(Packet) of
        {connect, SessionId} ->
            case maps:find(SessionId, Sessions) of
                {ok, {_, {Host, Port}}} ->
                    send_ack(Host, Port, SessionId, 0, State);
                {ok, _} ->
                    send_close(Host, Port, SessionId, State);
                error ->
                    start_session(Host, Port, SessionId, State)
            end;
        {data, SessionId, Pos, Data} ->
            case maps:find(SessionId, Sessions) of
                {ok, {{_, _, Receiver}, {Host, Port}}} ->
                    line_reversal_session_receiver:data(Receiver, Pos, Data),
                    {noreply, State};
                _ ->
                    send_close(Host, Port, SessionId, State)
            end;
        {ack, SessionId, Length} ->
            case maps:find(SessionId, Sessions) of
                {ok, {{_, Sender, _}, {Host, Port}}} ->
                    line_reversal_session_sender:ack(Sender, Length),
                    {noreply, State};
                _ ->
                    send_close(Host, Port, SessionId, State)
            end;
        {close, SessionId} ->
            case maps:find(SessionId, Sessions) of
                {ok, {{Supervisor, _, _}, {Host, Port}}} ->
                    line_reversal_sessions_sup:terminate_session(Supervisor),
                    {noreply, State};
                _ ->
                    {noreply, State}
            end;
        error ->
            {noreply, State}
    end;
handle_info(
    {'DOWN', Ref, process, Supervisor, _},
    State = #{socket := Socket, sessions := Sessions, monitors := Monitors}
) ->
    {SessionId, OtherMonitors} = maps:take(Ref, Monitors),
    {{{Supervisor, _, _}, {Host, Port}}, OtherSessions} = maps:take(SessionId, Sessions),
    gen_udp:send(Socket, Host, Port, line_reversal_utils:serialize_close(SessionId)),
    {noreply, State#{sessions := OtherSessions, monitors := OtherMonitors}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

send_ack(Host, Port, SessionId, Length, State = #{socket := Socket}) ->
    gen_udp:send(Socket, Host, Port, line_reversal_utils:serialize_ack(SessionId, Length)),
    {noreply, State}.

send_close(Host, Port, SessionId, State = #{socket := Socket}) ->
    gen_udp:send(Socket, Host, Port, line_reversal_utils:serialize_close(SessionId)),
    {noreply, State}.

start_session(
    Host, Port, SessionId, State = #{socket := Socket, sessions := Sessions, monitors := Monitors}
) ->
    {ok, Supervisor, {Sender, Receiver}} = line_reversal_sessions_sup:start_session(
        Socket, Host, Port, SessionId
    ),
    NewSessions = Sessions#{
        SessionId => {{Supervisor, Sender, Receiver}, {Host, Port}}
    },
    NewMonitors = Monitors#{monitor(process, Supervisor) => SessionId},
    {noreply, State#{sessions := NewSessions, monitors := NewMonitors}}.
