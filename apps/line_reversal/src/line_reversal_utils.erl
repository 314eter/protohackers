-module(line_reversal_utils).

-feature(maybe_expr, enable).

-export([parse/1, serialize_ack/2, serialize_close/1, serialize_data/3]).
-export([unescape/2, escape/1]).
-export([iolist_skip/2]).

parse(Packet) when byte_size(Packet) >= 1000 ->
    error;
parse(<<"/connect/", Rest/binary>>) ->
    parse_connect(Rest);
parse(<<"/data/", Rest/binary>>) ->
    parse_data(Rest);
parse(<<"/ack/", Rest/binary>>) ->
    parse_ack(Rest);
parse(<<"/close/", Rest/binary>>) ->
    parse_close(Rest);
parse(_) ->
    error.

parse_connect(Data) ->
    maybe
        {SessionId, <<>>} ?= parse_integer(Data),
        {connect, SessionId}
    end.

parse_data(Data) ->
    maybe
        {SessionId, PosData} ?= parse_integer(Data),
        {Pos, PayloadData} ?= parse_integer(PosData),
        {ok, Payload} ?= parse_payload(PayloadData),
        {data, SessionId, Pos, Payload}
    end.

parse_ack(Data) ->
    maybe
        {SessionId, LengthData} ?= parse_integer(Data),
        {Length, <<>>} ?= parse_integer(LengthData),
        {ack, SessionId, Length}
    end.

parse_close(Data) ->
    maybe
        {SessionId, <<>>} ?= parse_integer(Data),
        {close, SessionId}
    end.

parse_integer(Data) ->
    case binary:split(Data, <<"/">>) of
        [IntegerBin, Rest] ->
            try binary_to_integer(IntegerBin) of
                Integer when Integer >= 2147483648 -> error;
                Integer -> {Integer, Rest}
            catch
                error:badarg -> error
            end;
        _ ->
            error
    end.

parse_payload(Data) ->
    case binary:last(Data) of
        $/ -> {ok, binary_part(Data, 0, byte_size(Data) - 1)};
        _ -> error
    end.

serialize_ack(SessionId, Length) ->
    [<<"/ack/">>, integer_to_binary(SessionId), $/, integer_to_binary(Length), $/].

serialize_close(SessionId) ->
    [<<"/close/">>, integer_to_binary(SessionId), $/].

serialize_data(SessionId, Pos, Data) ->
    [<<"/data/">>, integer_to_binary(SessionId), $/, integer_to_binary(Pos), $/, Data, $/].

unescape(Data, Skip) ->
    unescape(Data, Skip, <<>>).

unescape(Data, Skip, Buffer) ->
    case binary:match(Data, [<<"\\">>, <<"/">>]) of
        nomatch ->
            case Data of
                <<_:Skip/binary, Escaped/binary>> -> <<Buffer/binary, Escaped/binary>>;
                _ -> Buffer
            end;
        {Pos, 1} ->
            case Data of
                <<Part:Pos/binary, $\\, Char, Rest/binary>> ->
                    case Part of
                        <<_:Skip/binary, Escaped/binary>> ->
                            unescape(Rest, 0, <<Buffer/binary, Escaped/binary, Char>>);
                        _ ->
                            unescape(Rest, Skip - Pos - 1, Buffer)
                    end;
                _ ->
                    error
            end
    end.

escape(Data) ->
    element(1, escape(Data, 970)).

escape(_, 0) ->
    {[], 0};
escape(Buffer, Limit) when is_binary(Buffer) ->
    BufferLength = byte_size(Buffer),
    case binary:match(Buffer, [<<"/">>, <<"\\">>]) of
        nomatch when Limit < BufferLength ->
            {binary_part(Buffer, 0, Limit), 0};
        nomatch ->
            {Buffer, Limit - BufferLength};
        {Pos, 1} when Limit =< Pos ->
            {binary_part(Buffer, 0, Limit), 0};
        {Pos, 1} when Limit == Pos + 1 ->
            {binary_part(Buffer, 0, Pos), 0};
        {Pos, 1} ->
            {Escaped, RestLimit} = escape(
                binary_part(Buffer, Pos + 1, BufferLength - Pos - 1), Limit - Pos - 2
            ),
            {[binary_part(Buffer, 0, Pos), $\\, binary:at(Buffer, Pos) | Escaped], RestLimit}
    end;
escape([], Limit) ->
    {[], Limit};
escape([Char | _], 1) when Char =:= $\\; Char =:= $/ ->
    {[], 0};
escape([Char | Buffer], Limit) when Char =:= $\\; Char =:= $/ ->
    {Escaped, RestLimit} = escape(Buffer, Limit - 2),
    {[$\\, Char | Escaped], RestLimit};
escape([Char | Buffer], Limit) when is_integer(Char) ->
    {Escaped, RestLimit} = escape(Buffer, Limit - 1),
    {[Char | Escaped], RestLimit};
escape([Part | Buffer], Limit) ->
    case escape(Part, Limit) of
        {EscapedPart, 0} ->
            {EscapedPart, 0};
        {EscapedPart, PartLimit} ->
            {EscapedBuffer, RestLimit} = escape(Buffer, PartLimit),
            {[EscapedPart | EscapedBuffer], RestLimit}
    end.

iolist_skip(0, Buffer) ->
    Buffer;
iolist_skip(Truncate, Buffer) when is_binary(Buffer), byte_size(Buffer) < Truncate ->
    binary_part(Buffer, Truncate, byte_size(Buffer) - Truncate);
iolist_skip(Truncate, Buffer) when is_binary(Buffer), byte_size(Buffer) =:= Truncate ->
    [];
iolist_skip(Truncate, Buffer) when is_binary(Buffer) ->
    Truncate - byte_size(Buffer);
iolist_skip(Truncate, []) ->
    Truncate;
iolist_skip(Truncate, [Character | Buffer]) when is_integer(Character) ->
    iolist_skip(Truncate - 1, Buffer);
iolist_skip(Truncate, [Part | Buffer]) ->
    case iolist_skip(Truncate, Part) of
        Left when is_integer(Left) ->
            iolist_skip(Left, Buffer);
        [] ->
            Buffer;
        Truncated ->
            [Truncated | Buffer]
    end.
