-module(smoke_test_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, Port} = application:get_env(port),
    ranch:start_listener(
        smoke_test,
        ranch_tcp,
        [{port, Port}],
        smoke_test_protocol,
        []
    ),
    smoke_test_sup:start_link().

stop(_State) ->
    ok.
