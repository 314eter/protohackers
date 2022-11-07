-module(means_to_an_end_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, Port} = application:get_env(port),
    ranch:start_listener(
        means_to_an_end,
        ranch_tcp,
        [{port, Port}],
        means_to_an_end_protocol,
        []
    ),
    means_to_an_end_sup:start_link().

stop(_State) ->
    ok.
