-module(job_centre_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ChildSpecs = [
        #{
            id => job_centre_server,
            start => {job_centre_server, start_link, []}
        }
    ],
    {ok, {#{}, ChildSpecs}}.
