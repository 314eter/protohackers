-module(budget_chat_sup).

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
        id => budget_chat_server,
        start => {budget_chat_server, start_link, []}
    }],
    {ok, {SupFlags, ChildSpecs}}.
