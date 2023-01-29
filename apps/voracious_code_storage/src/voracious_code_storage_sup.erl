-module(voracious_code_storage_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ChildSpecs = [
        #{
            id => voracious_code_storage_server,
            start => {voracious_code_storage_server, start_link, []}
        }
    ],
    {ok, {#{}, ChildSpecs}}.
