-module(job_centre_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, Port} = application:get_env(port),
    {ok, Supervisor} = job_centre_sup:start_link(),
    ranch:start_listener(
        job_centre,
        ranch_tcp,
        [{port, Port}],
        job_centre_protocol,
        []
    ),
    {ok, Supervisor}.


stop(_State) ->
    ok.
