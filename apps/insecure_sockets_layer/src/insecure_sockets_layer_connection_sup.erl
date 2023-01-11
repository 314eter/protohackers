-module(insecure_sockets_layer_connection_sup).

-behaviour(ranch_protocol).
-behaviour(supervisor).

-export([start_link/3]).
-export([init/1]).
-export([start_sender/4, start_application/2]).

start_link(Ref, Transport, []) ->
    {ok, Supervisor} = supervisor:start_link(?MODULE, []),
    {ok, Receiver} = start_receiver(Supervisor, Ref, Transport),
    {ok, Supervisor, Receiver}.

start_receiver(Supervisor, Ref, Transport) ->
    ChildSpec = #{
        id => insecure_sockets_layer_receiver,
        start => {insecure_sockets_layer_receiver, start_link, [Supervisor, Ref, Transport]},
        restart => transient,
        significant => true
    },
    supervisor:start_child(Supervisor, ChildSpec).

start_sender(Supervisor, Transport, Socket, Cipher) ->
    ChildSpec = #{
        id => insecure_sockets_layer_sender,
        start => {insecure_sockets_layer_sender, start_link, [Transport, Socket, Cipher]}
    },
    supervisor:start_child(Supervisor, ChildSpec).

start_application(Supervisor, Sender) ->
    ChildSpec = #{
        id => insecure_sockets_layer_application,
        start => {insecure_sockets_layer_application, start_link, [Sender]}
    },
    supervisor:start_child(Supervisor, ChildSpec).

init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 0,
        auto_shutdown => any_significant
    },
    {ok, {SupFlags, []}}.
