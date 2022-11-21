-module(line_reversal_sup).

-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Port) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Port).

init(Port) ->
    SupFlags = #{
        strategy => rest_for_one
    },
    ChildSpecs = [
        #{
            id => line_reversal_protocol,
            start => {line_reversal_protocol, start_link, [Port]}
        },
        #{
            id => line_reversal_sessions_sup,
            start => {line_reversal_sessions_sup, start_link, []},
            type => supervisor
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
