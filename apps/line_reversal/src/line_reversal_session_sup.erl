-module(line_reversal_session_sup).

-behaviour(supervisor).

-export([start_link/4]).
-export([init/1]).

start_link(Socket, Host, Port, SessionId) ->
    {ok, Supervisor} = supervisor:start_link(?MODULE, []),
    {ok, Sender} = start_sender(Supervisor, Socket, Host, Port, SessionId),
    {ok, Server} = start_server(Supervisor, Sender),
    {ok, Receiver} = start_receiver(Supervisor, Server, Socket, Host, Port, SessionId),
    {ok, Supervisor, {Sender, Receiver}}.

start_sender(Supervisor, Socket, Host, Port, SessionId) ->
    ChildSpec = #{
        id => line_reversal_session_sender,
        start => {line_reversal_session_sender, start_link, [Socket, Host, Port, SessionId]},
        restart => transient,
        significant => true
    },
    supervisor:start_child(Supervisor, ChildSpec).

start_server(Supervisor, Sender) ->
    ChildSpec = #{
        id => line_reversal_session_server,
        start => {line_reversal_session_server, start_link, [Sender]},
        restart => transient,
        significant => true
    },
    supervisor:start_child(Supervisor, ChildSpec).

start_receiver(Supervisor, Server, Socket, Host, Port, SessionId) ->
    ChildSpec = #{
        id => line_reversal_session_receiver,
        start => {line_reversal_session_receiver, start_link, [Server, Socket, Host, Port, SessionId]},
        restart => transient,
        significant => true
    },
    supervisor:start_child(Supervisor, ChildSpec).

init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 0,
        auto_shutdown => any_significant
    },
    {ok, {SupFlags, []}}.
