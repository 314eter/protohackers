-module(line_reversal_sessions_sup).

-behaviour(supervisor).

-export([start_link/0, start_session/4, terminate_session/1]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_session(Socket, Host, Port, SessionId) ->
    supervisor:start_child(?MODULE, [Socket, Host, Port, SessionId]).

terminate_session(Pid) ->
    supervisor:terminate_child(?MODULE, Pid).

init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one
    },
    ChildSpecs = [#{
        id => line_reversal_session_sup,
        start => {line_reversal_session_sup, start_link, []},
        restart => temporary,
        type => supervisor
    }],
    {ok, {SupFlags, ChildSpecs}}.
