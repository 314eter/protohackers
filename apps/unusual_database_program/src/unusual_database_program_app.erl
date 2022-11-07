-module(unusual_database_program_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, Port} = application:get_env(port),
    unusual_database_program_sup:start_link(Port).

stop(_State) ->
    ok.
