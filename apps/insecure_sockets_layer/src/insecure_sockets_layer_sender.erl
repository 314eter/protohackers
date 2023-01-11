-module(insecure_sockets_layer_sender).

-behaviour(gen_server).

-export([start_link/3, send/2]).
-export([init/1, handle_call/3, handle_cast/2]).

start_link(Transport, Socket, Cipher) ->
    gen_server:start_link(?MODULE, {Transport, Socket, Cipher}, []).

send(Sender, Packet) ->
    gen_server:cast(Sender, Packet).

init({Transport, Socket, Cipher}) ->
    State = #{transport => Transport, socket => Socket, cipher => Cipher, pos => 0},
    {ok, State}.

handle_call(_, _, State) ->
    {noreply, State}.

handle_cast(
    Packet, State = #{transport := Transport, socket := Socket, cipher := Cipher, pos := Pos}
) ->
    {Encoded, NewPos} = encode(<<>>, Cipher, Pos, Packet),
    Transport:send(Socket, Encoded),
    {noreply, State#{pos := NewPos}}.

encode(Encoded, _, Pos, <<>>) ->
    {Encoded, Pos};
encode(Encoded, Cipher, Pos, <<Byte, Packet/binary>>) ->
    EncodedByte = encode_byte(Cipher, Pos, Byte),
    encode(<<Encoded/binary, EncodedByte>>, Cipher, (Pos + 1) band 16#ff, Packet).

encode_byte([], _, Byte) ->
    Byte;
encode_byte([reversebits | Cipher], Pos, Byte) ->
    Reversed = Byte * 16#80200802 band 16#0884422110 * 16#0101010101 bsr 32,
    encode_byte(Cipher, Pos, Reversed band 16#ff);
encode_byte([{xor_, N} | Cipher], Pos, Byte) ->
    encode_byte(Cipher, Pos, Byte bxor N);
encode_byte([xorpos | Cipher], Pos, Byte) ->
    encode_byte(Cipher, Pos, Byte bxor Pos);
encode_byte([{add, N} | Cipher], Pos, Byte) ->
    encode_byte(Cipher, Pos, (Byte + N) band 16#ff);
encode_byte([addpos | Cipher], Pos, Byte) ->
    encode_byte(Cipher, Pos, (Byte + Pos) band 16#ff).
