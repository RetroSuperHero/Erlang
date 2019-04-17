%%%-------------------------------------------------------------------
%%% @author ErykSikora
%%% @copyright (C) 2019, <ErykSikora>
%%% @doc
%%%
%%% @end
%%% Created : 10. kwi 2019 13:25
%%%-------------------------------------------------------------------

-module(pollution_server).
-author("ErykSikora").

%% API
-export([start/0, stop/0, addStation/2, addValue/4, removeValue/3,
  getOneValue/3, getStationMean/2, getDailyMean/2, getAreaMean/3]).

start() ->
  PID = spawn(fun() -> init() end),
  register(pserv, PID),
  io:format("Rozpoczeto dzialanie serwera (PID ~w) -> ", [PID]),
  ok.

init() ->
  Monitor = pollution:createMonitor(),
  loopServer(Monitor).

addStation(Name, Position) ->
  pserv ! {addStation, Name, Position, self()},
  receive
    ok -> io:format("Stacja ~s, ~w zostala utworzona -> ", [Name, Position]),
      ok;
    stationAlreadyExistsError -> io:format("Stacja ~w, ~w juz istnieje -> ", [Name, Position]),
      stationAlreadyExistsError;
    badPositionError -> io:format("Podano bledna lokalizacje ~w -> ", [Position]),
      badPositionError;
    badMonitorError -> io:format("Podano bledny monitor -> "),
      badMonitorError
  end.

addValue(NameOrPosition, Date, Type, Value) ->
  pserv ! {addValue, NameOrPosition, Date, Type, Value, self()},
  receive
    ok -> io:format("Pomiar ~w, ~w, ~w, ~w zostal dodany -> ", [NameOrPosition, Date, Type, Value]),
      ok;
    measurementAlreadyExistsError -> io:format("Pomiar ~w, ~w, ~w, ~w juz istnieje! -> ", [NameOrPosition, Date, Type, Value]),
      measurementAlreadyExistsError;
    stationDoesNotExistError -> io:format("Podano bledna stacje ~w -> ", [NameOrPosition]),
      stationDoesNotExistError;
    badMonitorError -> io:format("Podano bledny monitor -> "),
      badMonitorError
  end.

removeValue(NameOrPosition, Date, Type) ->
  pserv ! {removeValue, NameOrPosition, Date, Type, self()},
  receive
    ok -> io:format("Pomiar ~w, ~w, ~w został usuniety -> ", [NameOrPosition, Date, Type]),
      ok;
    stationDoesNotExistError -> io:format("Podano bledna stacje ~w -> ", [NameOrPosition]),
      stationDoesNotExistError;
    badMonitorError -> io:format("Podano bledny monitor -> "),
      badMonitorError
  end.

getOneValue(NameOrPosition, Date, Type) ->
  pserv ! {getOneValue, NameOrPosition, Date, Type, self()},
  receive
    badMonitorError -> io:format("Podano bledny monitor -> "),
      badMonitorError;
    stationDoesNotExistError -> io:format("Podano bledna stacje ~w -> ", [NameOrPosition]),
      stationDoesNotExistError;
    Value -> io:format("Wartosc pomiaru dla ~w, ~w, ~w to ~w -> ", [NameOrPosition, Date, Type, Value]),
      Value
  end.

getStationMean(NameOrPosition, Type) ->
  pserv ! {getStationMean, NameOrPosition, Type, self()},
  receive
    badMonitorError -> io:format("Podano bledny monitor -> "),
      badMonitorError;
    stationDoesNotExistError -> io:format("Podano bledna stacje ~w -> ", [NameOrPosition]),
      stationDoesNotExistError;
    Value -> io:format("Srednia pomiarow dla ~w, ~w to ~w -> ", [NameOrPosition, Type, Value]),
      Value
  end.

getDailyMean(Date, Type) ->
  pserv ! {getDailyMean, Date, Type, self()},
  receive
    badMonitorError -> io:format("Podano bledny monitor -> "),
      badMonitorError;
    Value -> io:format("Srednia pomiarow dla ~w, ~w to ~w -> ", [Date, Type, Value]),
      Value
  end.

getAreaMean(NameOrPosition, Radius, Type) ->
  pserv ! {getAreaMean, NameOrPosition, Radius, Type, self()},
  receive
    badMonitorError -> io:format("Podano bledny monitor -> "),
      badMonitorError;
    stationDoesNotExistError -> io:format("Podano bledna stacje ~w -> ", [NameOrPosition]),
      stationDoesNotExistError;
    Value -> io:format("Srednia pomiarow dla ~w, ~w, ~w to ~w -> ", [NameOrPosition, Radius, Type, Value]),
      Value
  end.

loopServer(Monitor) ->
  receive
    {addStation, Name, Position, PID} -> NewMonitor = pollution:addStation(Monitor, Name, Position),
      case NewMonitor of
        {monitor, _, _} -> PID ! ok,
          loopServer(NewMonitor);
        stationAlreadyExistsError -> PID ! stationAlreadyExistsError,
          loopServer(Monitor);
        badPositionError -> PID ! badPositionError,
          loopServer(Monitor);
        badMonitorError -> PID ! badMonitorError,
          loopServer(Monitor)
      end;

    {addValue, NameOrPosition, Date, Type, Value, PID} -> NewMonitor = pollution:addValue(Monitor, NameOrPosition, Date, Type, Value),
      case NewMonitor of
        {monitor, _ , _} -> PID ! ok,
          loopServer(NewMonitor);
        measurementAlreadyExistsError -> PID ! measurementAlreadyExistsError,
          loopServer(Monitor);
        stationDoesNotExistError -> PID ! stationDoesNotExistError,
          loopServer(Monitor);
        badMonitorError -> PID ! badMonitorError,
          loopServer(Monitor)
      end;

    {removeValue, NameOrPosition, Date, Type, PID} -> NewMonitor = pollution:removeValue(Monitor, NameOrPosition, Date, Type),
      case NewMonitor of
        {monitor, _, _} -> PID ! ok,
          loopServer(NewMonitor);
        stationDoesNotExistError -> PID ! stationDoesNotExistError,
          loopServer(Monitor);
        badMonitorError -> PID ! badMonitorError,
          loopServer(Monitor)
      end;

    {getOneValue, NameOrPosition, Date, Type, PID} -> Value = pollution:getOneValue(Monitor, NameOrPosition, Date, Type),
      case Value of
        badMonitorError -> PID ! badMonitorError,
          loopServer(Monitor);
        stationDoesNotExistError -> PID ! stationDoesNotExistError,
          loopServer(Monitor);
        Value -> PID ! Value,
          loopServer(Monitor)
      end;

    {getStationMean, NameOrPosition, Type, PID} -> Value = pollution:getStationMean(Monitor, NameOrPosition, Type),
      case Value of
        badMonitorError -> PID ! badMonitorError,
          loopServer(Monitor);
        stationDoesNotExistError -> PID ! stationDoesNotExistError,
          loopServer(Monitor);
        Value -> PID ! Value,
          loopServer(Monitor)
      end;

    {getDailyMean, Date, Type, PID} -> Value = pollution:getDailyMean(Monitor, Date, Type),
      case Value of
        badMonitorError -> PID ! badMonitorError,
          loopServer(Monitor);
        Value -> PID ! Value,
          loopServer(Monitor)
      end;

    {getAreaMean, NameOrPosition, Radius, Type, PID} -> Value = pollution:getAreaMean(Monitor, NameOrPosition, Radius, Type),
      case Value of
        badMonitorError -> PID ! badMonitorError,
          loopServer(Monitor);
        stationDoesNotExistError -> PID ! stationDoesNotExistError,
          loopServer(Monitor);
        Value -> PID ! Value,
          loopServer(Monitor)
      end;

    {stop, PID} -> PID ! ok;

    _ -> loopServer(Monitor)
  end.

stop() ->
  pserv ! {stop, self()},
  io:format("Zakonczono dzialanie serwera -> "),
  ok.