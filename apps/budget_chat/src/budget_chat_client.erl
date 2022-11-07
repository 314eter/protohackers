-module(budget_chat_client).

-behaviour(ranch_protocol).
-behaviour(gen_statem).

-export([start_link/3, notify_join/2, notify_leave/2, message/3]).
-export([init/1, callback_mode/0]).
-export([pending/3, joined/3]).

start_link(Ref, Transport, []) ->
    gen_statem:start_link(?MODULE, {Ref, Transport}, []).

notify_join(Client, Name) ->
    gen_statem:cast(Client, {join, Name}).

notify_leave(Client, Name) ->
    gen_statem:cast(Client, {leave, Name}).

message(Client, Name, Message) ->
    gen_statem:cast(Client, {message, Name, Message}).

init({Ref, Transport}) ->
    Data = #{
        transport => Transport,
        socket => Ref,
        buffer => <<>>
    },
    {ok, pending, Data}.

callback_mode() -> [state_functions, state_enter].

pending(enter, pending, Data = #{transport := Transport, socket := Ref}) ->
    {ok, Socket} = ranch:handshake(Ref),
    ok = Transport:send(Socket, <<"Welcome to budgetchat! What shall I call you?\n">>),
    ok = Transport:setopts(Socket, [{active, once}]),
    {keep_state, Data#{socket := Socket}};
pending(info, Info, Data = #{transport := Transport, buffer := Buffer}) ->
    {OK, Closed, Error, _Passive} = Transport:messages(),
    case Info of
        {OK, Socket, Packet} ->
            NewBuffer = <<Buffer/binary, Packet/binary>>,
            case binary:split(NewBuffer, <<"\n">>) of
                [_] ->
                    ok = Transport:setopts(Socket, [{active, once}]),
                    {keep_state, Data#{buffer := NewBuffer}};
                [Line, Rest] ->
                    case budget_chat_server:join(self(), Line) of
                        {ok, Users} ->
                            Message = ["* The room contains: ", lists:join(", ", Users), $\n],
                            Transport:send(Socket, Message),
                            {next_state, joined, Data#{buffer := Rest}};
                        illegal_name ->
                            Transport:close(Socket),
                            {stop, normal}
                    end
            end;
        {Closed, Socket} ->
            Transport:close(Socket),
            {stop, normal};
        {Error, Socket, Reason} ->
            Transport:close(Socket),
            {stop, {Error, Reason}}
    end.

joined(enter, pending, Data = #{transport := Transport, socket := Socket, buffer := Buffer}) ->
    NewBuffer = process_message_buffer(Buffer),
    ok = Transport:setopts(Socket, [{active, once}]),
    {keep_state, Data#{buffer := NewBuffer}};
joined(info, Info, Data = #{transport := Transport, buffer := Buffer}) ->
    {OK, Closed, Error, _Passive} = Transport:messages(),
    case Info of
        {OK, Socket, Packet} ->
            NewBuffer = process_message_buffer(<<Buffer/binary, Packet/binary>>),
            ok = Transport:setopts(Socket, [{active, once}]),
            {keep_state, Data#{buffer := NewBuffer}};
        {Closed, Socket} ->
            budget_chat_server:leave(self()),
            Transport:close(Socket),
            {stop, normal};
        {Error, Socket, Reason} ->
            budget_chat_server:leave(self()),
            Transport:close(Socket),
            {stop, {Error, Reason}}
    end;
joined(cast, {join, Name}, #{transport := Transport, socket := Socket}) ->
    Transport:send(Socket, ["* ", Name, " has entered the room\n"]),
    keep_state_and_data;
joined(cast, {leave, Name}, #{transport := Transport, socket := Socket}) ->
    Transport:send(Socket, ["* ", Name, " has left the room\n"]),
    keep_state_and_data;
joined(cast, {message, Name, Message}, #{transport := Transport, socket := Socket}) ->
    Transport:send(Socket, ["[", Name, "] ", Message, $\n]),
    keep_state_and_data.

process_message_buffer(Buffer) ->
    process_message_lines(binary:split(Buffer, <<"\n">>, [global])).

process_message_lines([Line]) ->
    Line;
process_message_lines([Line | Lines]) ->
    budget_chat_server:message(self(), Line),
    process_message_lines(Lines).
