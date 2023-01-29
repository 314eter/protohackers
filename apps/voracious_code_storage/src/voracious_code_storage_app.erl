-module(voracious_code_storage_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, Port} = application:get_env(port),
    {ok, Supervisor} = voracious_code_storage_sup:start_link(),
    ranch:start_listener(
        voracious_code_storage,
        ranch_tcp,
        [{port, Port}],
        voracious_code_storage_protocol,
        []
    ),
    {ok, Supervisor}.


stop(_State) ->
    ok.
