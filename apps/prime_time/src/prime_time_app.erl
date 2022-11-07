-module(prime_time_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, Port} = application:get_env(port),
    ranch:start_listener(
        prime_time,
        ranch_tcp,
        [{port, Port}],
        prime_time_protocol,
        []
    ),
    prime_time_sup:start_link().


stop(_State) ->
    ok.
