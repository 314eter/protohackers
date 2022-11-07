-module(budget_chat_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, Port} = application:get_env(port),
    {ok, Supervisor} = budget_chat_sup:start_link(),
    ranch:start_listener(
        budget_chat,
        ranch_tcp,
        [{port, Port}],
        budget_chat_client,
        []
    ),
    {ok, Supervisor}.


stop(_State) ->
    ok.
