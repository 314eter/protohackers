-module(voracious_code_storage_protocol).

-behaviour(ranch_protocol).
-behaviour(gen_statem).

-export([start_link/3]).
-export([init/1, callback_mode/0]).
-export([ready/3, put/3]).

start_link(Ref, Transport, []) ->
    gen_statem:start_link(?MODULE, {Ref, Transport}, []).

init({Ref, Transport}) ->
    {ok, FilePattern} = re:compile(<<"^(/[A-Za-z0-9_.-]+)+$">>),
    {ok, DirPattern} = re:compile(<<"^((?:/[A-Za-z0-9_.-]+)*)/?$">>),
    Data = #{
        transport => Transport,
        socket => undefined,
        buffer => <<>>,
        file_pattern => FilePattern,
        dir_pattern => DirPattern,
        put_name => undefined,
        put_length => undefined
    },
    {ok, ready, Data, {next_event, internal, {handshake, Ref}}}.

callback_mode() -> state_functions.

ready(internal, {handshake, Ref}, Data = #{transport := Transport}) ->
    {ok, Socket} = ranch:handshake(Ref),
    ok = Transport:setopts(Socket, [{active, once}]),
    Transport:send(Socket, <<"READY\n">>),
    {keep_state, Data#{socket := Socket}};
ready(info, Info, Data = #{transport := Transport, socket := Socket, buffer := Buffer}) ->
    {OK, Closed, _, _} = Transport:messages(),
    case Info of
        {OK, Socket, Packet} ->
            ok = Transport:setopts(Socket, [{active, once}]),
            process_buffer(Data#{buffer := <<Buffer/binary, Packet/binary>>});
        {Closed, Socket} ->
            {stop, normal}
    end.

put(info, Info, Data = #{transport := Transport, socket := Socket, buffer := Buffer}) ->
    {OK, Closed, _, _} = Transport:messages(),
    case Info of
        {OK, Socket, Packet} ->
            ok = Transport:setopts(Socket, [{active, once}]),
            handle_put(Data#{buffer := <<Buffer/binary, Packet/binary>>});
        {Closed, Socket} ->
            {stop, normal}
    end.

process_buffer(Data = #{buffer := Buffer}) ->
    case binary:split(Buffer, <<"\n">>) of
        [_] -> {next_state, ready, Data};
        [Line, Rest] -> process_line(Line, Data#{buffer := Rest})
    end.

process_line(Line, Data) when is_binary(Line) ->
    process_line(binary:split(Line, <<" ">>, [global, trim]), Data);
process_line([], Data) ->
    process_line([<<>>], Data);
process_line([Command | Args], Data) ->
    handle_command(string:casefold(binary_to_list(Command)), Args, Data).

handle_command("help", _, Data) ->
    reply(<<"OK usage: HELP|GET|PUT|LIST\nREADY\n">>, Data);
handle_command("get", [Name], Data = #{file_pattern := FilePattern}) ->
    case re:run(Name, FilePattern, [{capture, none}]) of
        match -> handle_get(Name, current, Data);
        nomatch -> reply(<<"ERR illegal file name\n">>, Data)
    end;
handle_command("get", [Name, Rev], Data = #{file_pattern := FilePattern}) ->
    case re:run(Name, FilePattern, [{capture, none}]) of
        match -> handle_get(Name, Rev, Data);
        nomatch -> reply(<<"ERR illegal file name\n">>, Data)
    end;
handle_command("get", _, Data) ->
    reply(<<"ERR usage: GET file [revision]\nREADY\n">>, Data);
handle_command("put", [Name, LengthBin], Data = #{file_pattern := FilePattern}) ->
    case re:run(Name, FilePattern, [{capture, none}]) of
        match ->
            Length =
                try
                    binary_to_integer(LengthBin)
                catch
                    error:badarg -> 0
                end,
            handle_put(Data#{put_name := Name, put_length := Length});
        nomatch ->
            reply(<<"ERR illegal file name\n">>, Data)
    end;
handle_command("put", _, Data) ->
    reply(<<"ERR usage: PUT file length newline data\nREADY\n">>, Data);
handle_command("list", [Name], Data = #{dir_pattern := DirPattern}) ->
    case re:run(Name, DirPattern, [{capture, all_but_first, binary}]) of
        {match, [Dir]} -> handle_list(Dir, Data);
        nomatch -> reply(<<"ERR illegal dir name\n">>, Data)
    end;
handle_command("list", _, Data) ->
    reply(<<"ERR usage: LIST dir\nREADY\n">>, Data);
handle_command(Command, _, #{transport := Transport, socket := Socket}) ->
    Transport:send(Socket, [<<"ERR illegal method: ">>, string:uppercase(Command), $\n]),
    {stop, normal}.

handle_get(Name, Rev, Data) ->
    case voracious_code_storage_server:get(Name, Rev) of
        {ok, Content} ->
            Length = integer_to_binary(byte_size(Content)),
            reply([<<"OK ">>, Length, $\n, Content, <<"READY\n">>], Data);
        {error, Error} ->
            reply([<<"ERR ">>, Error, <<"\nREADY\n">>], Data)
    end.

handle_put(Data = #{buffer := Buffer, put_name := Name, put_length := Length}) when
    byte_size(Buffer) >= Length
->
    <<Content:Length/binary, Rest/binary>> = Buffer,
    case unicode:characters_to_binary(Content) of
        Content ->
            Rev = voracious_code_storage_server:put(Name, Content),
            reply([<<"OK r">>, integer_to_binary(Rev), <<"\nREADY\n">>], Data#{buffer := Rest});
        _ ->
            reply(<<"ERR text file only\nREADY\n">>, Data#{buffer := Rest})
    end;
handle_put(Data) ->
    {next_state, put, Data}.

handle_list(Name, Data) ->
    List = voracious_code_storage_server:list(Name),
    Length = integer_to_binary(length(List)),
    Listing = lists:map(fun serialize_item/1, List),
    reply([<<"OK ">>, Length, $\n, Listing, <<"READY\n">>], Data).

serialize_item({file, File, Rev}) ->
    [File, <<" r">>, integer_to_binary(Rev), $\n];
serialize_item({dir, Dir}) ->
    [Dir, <<"/ DIR\n">>].

reply(Reply, Data = #{transport := Transport, socket := Socket}) ->
    Transport:send(Socket, Reply),
    process_buffer(Data).
