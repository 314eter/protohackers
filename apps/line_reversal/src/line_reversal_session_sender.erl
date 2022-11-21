-module(line_reversal_session_sender).

-behaviour(gen_statem).

-export([start_link/4, data/2, ack/2]).
-export([init/1, callback_mode/0, idle/3, awaiting_ack/3]).

start_link(Socket, Host, Port, SessionId) ->
    gen_statem:start_link(?MODULE, {Socket, Host, Port, SessionId}, []).

data(Sender, Payload) ->
    gen_statem:cast(Sender, {data, Payload}).

ack(Sender, Length) ->
    gen_statem:cast(Sender, {ack, Length}).

init({Socket, Host, Port, SessionId}) ->
    Data = #{
        socket => Socket,
        host => Host,
        port => Port,
        session_id => SessionId,
        acked => 0,
        buffer => [],
        buffer_size => 0,
        session_expiry_timer => undefined
    },
    {ok, idle, Data}.

callback_mode() -> state_functions.

idle(cast, {data, Payload}, Data) ->
    send_data(Data#{buffer := Payload, buffer_size := iolist_size(Payload)});
idle(cast, {ack, Length}, #{acked := Acked}) when Length =< Acked ->
    keep_state_and_data;
idle(cast, {ack, _}, _) ->
    {stop, normal}.

awaiting_ack(state_timeout, retransmit, Data) ->
    send_data(Data);
awaiting_ack(cast, {data, Payload}, Data = #{buffer := Buffer, buffer_size := BufferSize}) ->
    send_data(Data#{buffer := [Buffer | Payload], buffer_size := BufferSize + iolist_size(Payload)});
awaiting_ack(cast, {ack, Length}, #{acked := Acked}) when Length =< Acked ->
    keep_state_and_data;
awaiting_ack(cast, {ack, Length}, #{acked := Acked, buffer_size := BufferSize}) when
    Length > Acked + BufferSize
->
    {stop, normal};
awaiting_ack(
    cast,
    {ack, Length},
    Data = #{
        acked := Acked,
        buffer := Buffer,
        buffer_size := BufferSize,
        session_expiry_timer := Timer
    }
) ->
    Truncate = Length - Acked,
    send_data(Data#{
        acked := Length,
        buffer := line_reversal_utils:iolist_skip(Truncate, Buffer),
        buffer_size := BufferSize - Truncate,
        session_expiry_timer := cancel_session_expiry_timer(Timer)
    });
awaiting_ack(info, {timeout, _, session_expired}, _) ->
    {stop, normal}.

send_data(Data = #{buffer_size := 0}) ->
    {next_state, idle, Data};
send_data(
    Data = #{
        socket := Socket,
        host := Host,
        port := Port,
        session_id := SessionId,
        acked := Acked,
        buffer := Buffer,
        session_expiry_timer := Timer
    }
) ->
    Escaped = line_reversal_utils:escape(Buffer),
    Packet = line_reversal_utils:serialize_data(SessionId, Acked, Escaped),
    gen_udp:send(Socket, Host, Port, Packet),
    NewData = Data#{session_expiry_timer := update_session_expiry_timer(Timer)},
    {ok, RetransmissionTimeout} = application:get_env(retransmission_timeout),
    {next_state, awaiting_ack, NewData, {state_timeout, RetransmissionTimeout, retransmit}}.

cancel_session_expiry_timer(Timer) when is_reference(Timer) ->
    erlang:cancel_timer(Timer),
    undefined;
cancel_session_expiry_timer(undefined) ->
    undefined.

update_session_expiry_timer(Timer) when is_reference(Timer) ->
    Timer;
update_session_expiry_timer(undefined) ->
    {ok, SessionExpiryTimeout} = application:get_env(session_expiry_timeout),
    erlang:start_timer(SessionExpiryTimeout, self(), session_expired).
