-module(speed_daemon_server).

-behaviour(gen_server).

-export([start_link/0, set_road_limit/2, add_plate/4, register_dispatcher/2]).
-export([init/1, handle_call/3, handle_info/2, handle_cast/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

set_road_limit(Road, Limit) ->
    gen_server:cast(?MODULE, {road, Road, Limit}).

add_plate(Plate, Road, Mile, Timestamp) ->
    gen_server:cast(?MODULE, {plate, Plate, Road, Mile, Timestamp}).

register_dispatcher(Dispatcher, Roads) ->
    gen_server:call(?MODULE, {dispatcher, Dispatcher, Roads}).

init([]) ->
    State = #{
        limits => array:new(65536, {default, 0}),
        dispatchers => array:new(65536, {default, gb_sets:new()}),
        monitors => #{},
        reports => #{},
        tickets => #{},
        days => sets:new()
    },
    {ok, State}.

handle_call(
    {dispatcher, Dispatcher, Roads},
    _From,
    State = #{dispatchers := Dispatchers, monitors := Monitors, tickets := Tickets}
) ->
    Ref = monitor(process, Dispatcher),
    NewMonitors = maps:put(Ref, Roads, Monitors),
    NewDispatchers = add_dispatcher(Roads, Dispatcher, Dispatchers),
    {DispatcherTickets, OtherTickets} = take_tickets(Roads, Tickets),
    {reply, DispatcherTickets, State#{
        dispatchers := NewDispatchers, tickets := OtherTickets, monitors := NewMonitors
    }}.

handle_info(
    {'DOWN', Ref, process, Dispatcher, _},
    State = #{dispatchers := Dispatchers, monitors := Monitors}
) ->
    case maps:take(Ref, Monitors) of
        {Roads, NewMonitors} ->
            NewDispatchers = remove_dispatcher(Roads, Dispatcher, Dispatchers),
            {noreply, State#{dispatchers := NewDispatchers, monitors := NewMonitors}};
        error ->
            {noreply, State}
    end.

handle_cast({road, Road, Limit}, State = #{limits := Limits}) ->
    {noreply, State#{limits := array:set(Road, Limit, Limits)}};
handle_cast(
    {plate, Plate, Road, Mile, Timestamp},
    State = #{
        limits := Limits,
        dispatchers := Dispatchers,
        reports := OldReports,
        tickets := Tickets,
        days := OldDays
    }
) ->
    {CheckReports, Reports} = add_report(Plate, Road, Mile, Timestamp, OldReports),
    Limit = array:get(Road, Limits),
    case check_speed(Plate, Road, Mile, Timestamp, CheckReports, Limit, OldDays) of
        no_ticket ->
            {noreply, State#{reports := Reports}};
        Ticket = #{timestamp1 := Timestamp1, timestamp2 := Timestamp2} ->
            Days = lists:foldl(
                fun(Day, NewDays) -> sets:add_element({Plate, Day}, NewDays) end,
                OldDays,
                timespan_to_days(Timestamp1, Timestamp2)
            ),
            NewTickets = send_ticket(Ticket, Dispatchers, Tickets),
            {noreply, State#{reports := Reports, tickets := NewTickets, days := Days}}
    end.

add_dispatcher([], _Dispatcher, Dispatchers) ->
    Dispatchers;
add_dispatcher([Road | Roads], Dispatcher, Dispatchers) ->
    RoadDispatchers = array:get(Road, Dispatchers),
    NewRoadDispatchers = gb_sets:add_element(Dispatcher, RoadDispatchers),
    NewDispatchers = array:set(Road, NewRoadDispatchers, Dispatchers),
    add_dispatcher(Roads, Dispatcher, NewDispatchers).

remove_dispatcher([], _Dispatcher, Dispatchers) ->
    Dispatchers;
remove_dispatcher([Road | Roads], Dispatcher, Dispatchers) ->
    RoadDispatchers = array:get(Road, Dispatchers),
    NewRoadDispatchers = gb_sets:del_element(Dispatcher, RoadDispatchers),
    NewDispatchers = array:set(Road, NewRoadDispatchers, Dispatchers),
    remove_dispatcher(Roads, Dispatcher, NewDispatchers).

take_tickets(Roads, Tickets) ->
    take_tickets(Roads, [], Tickets).

take_tickets([], DispatcherTickets, OtherTickets) ->
    {DispatcherTickets, OtherTickets};
take_tickets([Road | Roads], DispatcherTickets, Tickets) ->
    case maps:take(Road, Tickets) of
        {NewRoadTickets, OtherTickets} ->
            take_tickets(Roads, NewRoadTickets ++ DispatcherTickets, OtherTickets);
        error ->
            take_tickets(Roads, DispatcherTickets, Tickets)
    end.

add_report(Plate, Road, Mile, Timestamp, OldReports) ->
    CheckReports = maps:get({Road, Plate}, OldReports, []),
    Reports = OldReports#{{Road, Plate} => [{Mile, Timestamp} | CheckReports]},
    {CheckReports, Reports}.

timespan_to_days(Timestamp1, Timestamp2) ->
    lists:seq(trunc(Timestamp1 / 86400), trunc(Timestamp2 / 86400)).

check_speed(_Plate, _Road, _Mile, _Timestamp, [], _Limit, _Days) ->
    no_ticket;
check_speed(
    Plate,
    Road,
    Mile,
    Timestamp,
    [{OtherMile, OtherTimestamp} | CheckReports],
    Limit,
    Days
) ->
    {Mile1, Timestamp1, Mile2, Timestamp2} = order_reports(
        Mile, Timestamp, OtherMile, OtherTimestamp
    ),
    Speed = 3600 * abs(Mile2 - Mile1) / abs(Timestamp2 - Timestamp1),
    TicketDays = timespan_to_days(Timestamp1, Timestamp2),
    Overlapping = lists:any(fun(Day) -> sets:is_element({Plate, Day}, Days) end, TicketDays),
    if
        Speed > Limit, not Overlapping ->
            #{
                plate => Plate,
                road => Road,
                mile1 => Mile1,
                timestamp1 => Timestamp1,
                mile2 => Mile2,
                timestamp2 => Timestamp2,
                speed => Speed
            };
        true ->
            check_speed(Plate, Road, Mile, Timestamp, CheckReports, Limit, Days)
    end.

order_reports(Mile1, Timestamp1, Mile2, Timestamp2) when Timestamp1 =< Timestamp2 ->
    {Mile1, Timestamp1, Mile2, Timestamp2};
order_reports(Mile1, Timestamp1, Mile2, Timestamp2) ->
    {Mile2, Timestamp2, Mile1, Timestamp1}.

send_ticket(Ticket = #{road := Road}, Dispatchers, Tickets) ->
    RoadDispatchers = array:get(Road, Dispatchers),
    case gb_sets:next(gb_sets:iterator(RoadDispatchers)) of
        {Dispatcher, _} ->
            speed_daemon_protocol:send_ticket(Dispatcher, Ticket),
            Tickets;
        none ->
            RoadTickets = maps:get(Road, Tickets, []),
            Tickets#{Road => [Ticket | RoadTickets]}
    end.
