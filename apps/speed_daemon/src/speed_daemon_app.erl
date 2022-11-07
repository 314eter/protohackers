-module(speed_daemon_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, Port} = application:get_env(port),
    {ok, Supervisor} = speed_daemon_sup:start_link(),
    ranch:start_listener(
        speed_daemon,
        ranch_tcp,
        [{port, Port}],
        speed_daemon_protocol,
        []
    ),
    {ok, Supervisor}.

stop(_State) ->
    ok.
