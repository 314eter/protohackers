-module(insecure_sockets_layer_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, Port} = application:get_env(port),
    ranch:start_listener(
        insecure_sockets_layer,
        ranch_tcp,
        #{connection_type => supervisor, socket_opts => [{port, Port}]},
        insecure_sockets_layer_connection_sup,
        []
    ),
    insecure_sockets_layer_sup:start_link().

stop(_State) ->
    ok.
