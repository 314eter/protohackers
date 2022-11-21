-module(line_reversal_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, Port} = application:get_env(port),
    line_reversal_sup:start_link(Port).

stop(_State) ->
    ok.
