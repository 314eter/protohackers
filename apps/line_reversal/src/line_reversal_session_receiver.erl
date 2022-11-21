-module(line_reversal_session_receiver).

-behaviour(gen_server).

-export([start_link/5, data/3]).
-export([init/1, handle_continue/2, handle_call/3, handle_cast/2]).

start_link(Server, Socket, Host, Port, SessionId) ->
    gen_server:start_link(?MODULE, {Server, Socket, Host, Port, SessionId}, []).

data(Receiver, Pos, Data) ->
    gen_server:cast(Receiver, {data, Pos, Data}).

init({Server, Socket, Host, Port, SessionId}) ->
    State = #{
        server => Server,
        socket => Socket,
        host => Host,
        port => Port,
        session_id => SessionId,
        acked => 0
    },
    {ok, State, {continue, ack}}.

handle_continue(ack, State) ->
    send_ack(State).

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({data, Pos, _}, State = #{acked := Acked}) when Pos > Acked ->
    send_ack(State);
handle_cast({data, Pos, Escaped}, State = #{server := Server, acked := Acked}) ->
    case line_reversal_utils:unescape(Escaped, Acked - Pos) of
        error ->
            {noreply, State};
        <<>> ->
            send_ack(State);
        Data ->
            line_reversal_session_server:receive_data(Server, Data),
            send_ack(State#{acked := Acked + byte_size(Data)})
    end.

send_ack(
    State = #{
        socket := Socket,
        host := Host,
        port := Port,
        session_id := SessionId,
        acked := Acked
    }
) ->
    gen_udp:send(Socket, Host, Port, line_reversal_utils:serialize_ack(SessionId, Acked)),
    {noreply, State}.
