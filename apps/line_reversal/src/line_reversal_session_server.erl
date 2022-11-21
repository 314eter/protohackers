-module(line_reversal_session_server).

-behaviour(gen_server).

-export([start_link/1, receive_data/2]).
-export([init/1, handle_call/3, handle_cast/2]).

start_link(Sender) ->
    gen_server:start_link(?MODULE, Sender, []).

receive_data(Server, Data) ->
    gen_server:cast(Server, {data, Data}).

init(Sender) ->
    {ok, #{sender => Sender, buffer => <<>>}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({data, Data}, State) ->
    process_data(binary:split(Data, <<"\n">>, [global]), State).

process_data([Line], State = #{buffer := Buffer}) ->
    {noreply, State#{buffer := <<Buffer/binary, Line/binary>>}};
process_data([Line | Lines], State = #{sender := Sender, buffer := <<>>}) ->
    line_reversal_session_sender:data(Sender, [reverse(Line), $\n]),
    process_data(Lines, State);
process_data([Line | Lines], State = #{sender := Sender, buffer := Buffer}) ->
    line_reversal_session_sender:data(Sender, [reverse(Line), reverse(Buffer), $\n]),
    process_data(Lines, State#{buffer := <<>>}).

reverse(Line) ->
    reverse(Line, []).

reverse(<<>>, Reversed) ->
    Reversed;
reverse(<<C, Line/binary>>, Reversed) ->
    reverse(Line, [C | Reversed]).
