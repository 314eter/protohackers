-module(speed_daemon_protocol).

-behaviour(ranch_protocol).
-behaviour(gen_statem).

-export([start_link/3, send_ticket/2]).
-export([init/1, callback_mode/0]).
-export([pending/3, camera/3, dispatcher/3]).

-define(HANDLE_COMMON,
    ?FUNCTION_NAME(info, Info, Data = #{transport := Transport, buffer := Buffer}) ->
        {OK, Closed, Error, _Passive} = Transport:messages(),
        case Info of
            {OK, Socket, Packet} ->
                ok = Transport:setopts(Socket, [{active, once}]),
                ?FUNCTION_NAME(internal, <<Buffer/binary, Packet/binary>>, Data#{buffer := <<>>});
            {Closed, Socket} ->
                Transport:close(Socket),
                {stop, normal};
            {Error, Socket, Reason} ->
                Transport:close(Socket),
                {stop, {Error, Reason}}
        end;
    ?FUNCTION_NAME(internal, <<>>, _Data) ->
        keep_state_and_data;
    ?FUNCTION_NAME(internal, Buffer = <<16#40, Rest/binary>>, Data) when byte_size(Rest) < 4 ->
        {keep_state, Data#{buffer := Buffer}};
    ?FUNCTION_NAME(internal, <<16#40, Interval:4/unit:8, Rest/binary>>, _Data) ->
        Timeout = case Interval of 0 -> infinity; _ -> 100 * Interval end,
        Actions = [{{timeout, heartbeat}, Timeout, Timeout}, {next_event, internal, Rest}],
        {keep_state_and_data, Actions};
    ?FUNCTION_NAME(internal, _, #{transport := Transport, socket := Socket}) ->
        Transport:send(Socket, <<16#10, 12, "invalid data">>),
        Transport:close(Socket),
        {stop, normal};
    ?FUNCTION_NAME({timeout, heartbeat}, Timeout, #{transport := Transport, socket := Socket}) ->
        Transport:send(Socket, <<16#41>>),
        {keep_state_and_data, {{timeout, heartbeat}, Timeout, Timeout}}
).

start_link(Ref, Transport, []) ->
    gen_statem:start_link(?MODULE, {Ref, Transport}, []).

send_ticket(Dispatcher, Ticket) ->
    gen_statem:cast(Dispatcher, Ticket).

init({Ref, Transport}) ->
    Data = #{
        transport => Transport,
        socket => nil,
        buffer => <<>>
    },
    {ok, pending, Data, {next_event, internal, {handshake, Ref}}}.

callback_mode() -> state_functions.

pending(internal, {handshake, Ref}, Data = #{transport := Transport}) ->
    {ok, Socket} = ranch:handshake(Ref),
    ok = Transport:setopts(Socket, [{active, once}]),
    {keep_state, Data#{socket := Socket}};
pending(internal, Buffer = <<16#80, Rest/binary>>, Data) when byte_size(Rest) < 6 ->
    {keep_state, Data#{buffer := Buffer}};
pending(internal, <<16#80, Road:2/unit:8, Mile:2/unit:8, Limit:2/unit:8, Rest/binary>>, Data) ->
    speed_daemon_server:set_road_limit(Road, Limit),
    {next_state, camera, Data#{road => Road, mile => Mile}, {next_event, internal, Rest}};
pending(internal, Buffer = <<16#81, Rest/binary>>, Data) when byte_size(Rest) < 1 ->
    {keep_state, Data#{buffer := Buffer}};
pending(internal, Buffer = <<16#81, NumRoads:1/unit:8, Rest/binary>>, Data) when
    byte_size(Rest) < 2 * NumRoads
->
    {keep_state, Data#{buffer := Buffer}};
pending(internal, <<16#81, NumRoads:1/unit:8, Roads:(2 * NumRoads)/binary, Rest/binary>>, Data) ->
    Tickets = speed_daemon_server:register_dispatcher(self(), parse_roads(Roads)),
    lists:foreach(fun(Ticket) -> send_ticket(self(), Ticket) end, Tickets),
    {next_state, dispatcher, Data, {next_event, internal, Rest}};
?HANDLE_COMMON.

parse_roads(<<>>) ->
    [];
parse_roads(<<Road:2/unit:8, Roads/binary>>) ->
    [Road | parse_roads(Roads)].

camera(internal, Buffer = <<16#20, Rest/binary>>, Data) when byte_size(Rest) < 1 ->
    {keep_state, Data#{buffer := Buffer}};
camera(internal, Buffer = <<16#20, PlateLength:1/unit:8, Rest/binary>>, Data) when
    byte_size(Rest) < PlateLength + 4
->
    {keep_state, Data#{buffer := Buffer}};
camera(
    internal,
    <<16#20, PlateLength:1/unit:8, Plate:PlateLength/binary, Timestamp:4/unit:8, Rest/binary>>,
    #{road := Road, mile := Mile}
) ->
    speed_daemon_server:add_plate(Plate, Road, Mile, Timestamp),
    {keep_state_and_data, {next_event, internal, Rest}};
?HANDLE_COMMON.

dispatcher(
    cast,
    #{
        plate := Plate,
        road := Road,
        mile1 := Mile1,
        timestamp1 := Timestamp1,
        mile2 := Mile2,
        timestamp2 := Timestamp2,
        speed := Speed
    },
    #{transport := Transport, socket := Socket}
) ->
    PlateLength = byte_size(Plate),
    Speed100 = trunc(round(100 * Speed)),
    Transport:send(
        Socket,
        <<
            16#21,
            PlateLength,
            Plate/binary,
            Road:2/unit:8,
            Mile1:2/unit:8,
            Timestamp1:4/unit:8,
            Mile2:2/unit:8,
            Timestamp2:4/unit:8,
            Speed100:2/unit:8
        >>
    ),
    keep_state_and_data;
?HANDLE_COMMON.
