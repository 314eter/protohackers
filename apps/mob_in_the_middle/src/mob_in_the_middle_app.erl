-module(mob_in_the_middle_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, Port} = application:get_env(port),
    {ok, UpstreamAddress} = application:get_env(upstream_address),
    {ok, UpstreamPort} = application:get_env(upstream_port),
    ranch:start_listener(
        mob_in_the_middle,
        ranch_tcp,
        [{port, Port}],
        mob_in_the_middle_protocol,
        {UpstreamAddress, UpstreamPort}
    ),
    mob_in_the_middle_sup:start_link().

stop(_State) ->
    ok.
