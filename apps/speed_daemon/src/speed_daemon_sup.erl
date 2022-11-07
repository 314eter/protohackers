-module(speed_daemon_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        intensity => 0,
        period => 1
    },
    ChildSpecs = [#{
        id => speed_daemon_server,
        start => {speed_daemon_server, start_link, []}
    }],
    {ok, {SupFlags, ChildSpecs}}.
