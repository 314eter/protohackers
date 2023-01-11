-module(insecure_sockets_layer_receiver).

-behaviour(gen_statem).

-export([start_link/3]).
-export([init/1, callback_mode/0]).
-export([receiving_cipher/3, receiving_data/3]).

start_link(Supervisor, Ref, Transport) ->
    gen_statem:start_link(?MODULE, {Supervisor, Ref, Transport}, []).

init({Supervisor, Ref, Transport}) ->
    Data = #{
        supervisor => Supervisor,
        transport => Transport,
        socket => undefined,
        cipher => [],
        buffer => <<>>
    },
    {ok, receiving_cipher, Data, {next_event, internal, {handshake, Ref}}}.

callback_mode() -> state_functions.

receiving_cipher(internal, {handshake, Ref}, Data = #{transport := Transport}) ->
    {ok, Socket} = ranch:handshake(Ref),
    ok = Transport:setopts(Socket, [{active, once}]),
    {keep_state, Data#{socket := Socket}};
receiving_cipher(
    info,
    Info,
    Data = #{transport := Transport, socket := Socket, buffer := Buffer}
) ->
    {OK, Closed, _, _} = Transport:messages(),
    case Info of
        {OK, Socket, Packet} ->
            ok = Transport:setopts(Socket, [{active, once}]),
            process_cipher(Data#{buffer := <<Buffer/binary, Packet/binary>>});
        {Closed, Socket} ->
            {stop, normal}
    end.

receiving_data(info, Info, Data = #{transport := Transport, socket := Socket}) ->
    {OK, Closed, _, _} = Transport:messages(),
    case Info of
        {OK, Socket, Packet} ->
            ok = Transport:setopts(Socket, [{active, once}]),
            process(Packet, Data);
        {Closed, Socket} ->
            {stop, normal}
    end.

process_cipher(#{buffer := <<>>}) ->
    keep_state_and_data;
process_cipher(#{
    supervisor := Supervisor,
    transport := Transport,
    socket := Socket,
    cipher := Cipher,
    buffer := <<0, Buffer/binary>>
}) ->
    case canonical_cipher(lists:reverse(Cipher)) of
        [] ->
            {stop, normal};
        Canonical ->
            {ok, Sender} = insecure_sockets_layer_connection_sup:start_sender(
                Supervisor, Transport, Socket, Canonical
            ),
            {ok, Application} = insecure_sockets_layer_connection_sup:start_application(
                Supervisor, Sender
            ),
            Data = #{
                transport => Transport,
                socket => Socket,
                cipher => lists:reverse(Canonical),
                pos => 0,
                application => Application
            },
            process(Buffer, Data)
    end;
process_cipher(Data = #{cipher := Cipher, buffer := <<1, Buffer/binary>>}) ->
    process_cipher(Data#{cipher := [reversebits | Cipher], buffer := Buffer});
process_cipher(#{buffer := <<2>>}) ->
    keep_state_and_data;
process_cipher(Data = #{cipher := Cipher, buffer := <<2, N, Buffer/binary>>}) ->
    process_cipher(Data#{cipher := [{xor_, N} | Cipher], buffer := Buffer});
process_cipher(Data = #{cipher := Cipher, buffer := <<3, Buffer/binary>>}) ->
    process_cipher(Data#{cipher := [xorpos | Cipher], buffer := Buffer});
process_cipher(#{buffer := <<4>>}) ->
    keep_state_and_data;
process_cipher(Data = #{cipher := Cipher, buffer := <<4, N, Buffer/binary>>}) ->
    process_cipher(Data#{cipher := [{add, N} | Cipher], buffer := Buffer});
process_cipher(Data = #{cipher := Cipher, buffer := <<5, Buffer/binary>>}) ->
    process_cipher(Data#{cipher := [addpos | Cipher], buffer := Buffer}).

canonical_cipher(Cipher) ->
    canonical_cipher([], Cipher).

canonical_cipher(Canonical, []) ->
    test_cipher(Canonical, 0, 0);
canonical_cipher([reversebits | Canonical], [reversebits | Cipher]) ->
    canonical_cipher(Canonical, Cipher);
canonical_cipher([{xor_, N} | Canonical], [reversebits | Cipher]) ->
    canonical_cipher(Canonical, [reversebits, {xor_, reverse(N)} | Cipher]);
canonical_cipher([{xor_, M} | Canonical], [{xor_, N} | Cipher]) ->
    canonical_cipher(Canonical, [{xor_, M bxor N} | Cipher]);
canonical_cipher(Canonical, [{xor_, 0} | Cipher]) ->
    canonical_cipher(Canonical, Cipher);
canonical_cipher([xorpos | Canonical], [xorpos | Cipher]) ->
    canonical_cipher(Canonical, Cipher);
canonical_cipher([xorpos | Canonical], [{xor_, N} | Cipher]) ->
    canonical_cipher(Canonical, [{xor_, N}, xorpos | Cipher]);
canonical_cipher([{add, M} | Canonical], [{add, N} | Cipher]) ->
    canonical_cipher(Canonical, [{add, (M + N) band 16#ff} | Cipher]);
canonical_cipher(Canonical, [{add, 0} | Cipher]) ->
    canonical_cipher(Canonical, Cipher);
canonical_cipher([addpos | Canonical], [{add, N} | Cipher]) ->
    canonical_cipher(Canonical, [{add, N}, addpos | Cipher]);
canonical_cipher(Canonical, [Operation | Cipher]) ->
    canonical_cipher([Operation | Canonical], Cipher).

test_cipher(_, 256, 0) ->
    [];
test_cipher(Cipher, Pos, 256) ->
    test_cipher(Cipher, Pos + 1, 0);
test_cipher(Cipher, Pos, Byte) ->
    case decode_byte(Cipher, Pos, Byte) of
        Byte -> test_cipher(Cipher, Pos, Byte + 1);
        _ -> lists:reverse(Cipher)
    end.

process(<<>>, Data) ->
    {next_state, receiving_data, Data};
process(Packet, Data = #{cipher := Cipher, pos := Pos, application := Application}) ->
    {Plain, NewPos} = decode(<<>>, Cipher, Pos, Packet),
    insecure_sockets_layer_application:process(Application, Plain),
    {next_state, receiving_data, Data#{pos := NewPos}}.

decode(Plain, _, Pos, <<>>) ->
    {Plain, Pos};
decode(Plain, Cipher, Pos, <<Byte, Packet/binary>>) ->
    PlainByte = decode_byte(Cipher, Pos, Byte),
    decode(<<Plain/binary, PlainByte>>, Cipher, (Pos + 1) band 16#ff, Packet).

decode_byte([], _, Byte) ->
    Byte;
decode_byte([reversebits | Cipher], Pos, Byte) ->
    decode_byte(Cipher, Pos, reverse(Byte));
decode_byte([{xor_, N} | Cipher], Pos, Byte) ->
    decode_byte(Cipher, Pos, Byte bxor N);
decode_byte([xorpos | Cipher], Pos, Byte) ->
    decode_byte(Cipher, Pos, Byte bxor Pos);
decode_byte([{add, N} | Cipher], Pos, Byte) ->
    decode_byte(Cipher, Pos, (Byte - N) band 16#ff);
decode_byte([addpos | Cipher], Pos, Byte) ->
    decode_byte(Cipher, Pos, (Byte - Pos) band 16#ff).

reverse(Byte) ->
    (Byte * 16#80200802 band 16#0884422110 * 16#0101010101 bsr 32) band 16#ff.
