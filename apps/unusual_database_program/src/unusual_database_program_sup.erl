-module(unusual_database_program_sup).

-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Port) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Port).

init(Port) ->
    SupFlags = #{
        intensity => 0,
        period => 1
    },
    ChildSpecs = [#{
        id => unusual_database_program_server,
        start => {unusual_database_program_server, start_link, [Port]}
    }],
    {ok, {SupFlags, ChildSpecs}}.
