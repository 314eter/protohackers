-module(budget_chat_server).

-behaviour(gen_server).

-export([start_link/0, join/2, leave/1, message/2]).
-export([init/1, handle_call/3, handle_cast/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

join(Client, Name) ->
    gen_server:call(?MODULE, {join, Client, Name}).

leave(Client) ->
    gen_server:cast(?MODULE, {leave, Client}).

message(Client, Message) ->
    gen_server:cast(?MODULE, {message, Client, Message}).

init([]) ->
    {ok, #{}}.

handle_call({join, Client, Name}, _From, Clients) ->
    case re:run(Name, "^[A-Za-z0-9]+$", [{capture, none}]) of
        match ->
            broadcast_join(Clients, Name),
            {reply, {ok, maps:values(Clients)}, Clients#{Client => Name}};
        nomatch ->
            {reply, illegal_name, Clients}
    end.

handle_cast({leave, Client}, OldClients) ->
    {Name, Clients} = maps:take(Client, OldClients),
    broadcast_leave(Clients, Name),
    {noreply, Clients};
handle_cast({message, Client, Message}, Clients) ->
    broadcast_message(Clients, Client, Message),
    {noreply, Clients}.

broadcast_join(Clients, Name) ->
    maps:foreach(
        fun(Client, _Name) -> budget_chat_client:notify_join(Client, Name) end,
        Clients
    ).

broadcast_leave(Clients, Name) ->
    maps:foreach(
        fun(Client, _Name) -> budget_chat_client:notify_leave(Client, Name) end,
        Clients
    ).

broadcast_message(Clients, From, Message) ->
    Name = maps:get(From, Clients),
    maps:foreach(
        fun
            (Client, _Name) when Client =:= From -> ok;
            (Client, _Name) -> budget_chat_client:message(Client, Name, Message)
        end,
        Clients
    ).
