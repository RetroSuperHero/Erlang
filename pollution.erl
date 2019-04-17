%%%-------------------------------------------------------------------
%%% @author ErykSikora
%%% @copyright (C) 2019, <ErykSikora>
%%% @doc
%%%
%%% @end
%%% Created : 04. kwi 2019 15:11
%%%-------------------------------------------------------------------

-module(pollution).
-author("ErykSikora").

%% API
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4,
  getOneValue/4, getStationMean/3, getDailyMean/3, getAreaMean/4]).

-record(monitor, {stations, measurements}).
-record(measurement, {date, type, value}).
-record(station, {name, position}).

createMonitor() ->
  #monitor{stations = #{}, measurements = #{}}.

%Dodaje do monitora Monitor stację o nazwie Name i pozycji (PositionX, PoistionY)
addStation(Monitor, Name, {PositionX, PositionY}) when is_record(Monitor, monitor)
  and is_number(PositionX) and is_number(PositionY) ->
  case is_map_key(Name, Monitor#monitor.stations)
    or is_map_key({PositionX, PositionY}, Monitor#monitor.stations) of
    false ->
      {monitor, Stations, Measurements} = Monitor,
      Station = #station{name = Name, position = {PositionX, PositionY}},
      #monitor{
        stations = Stations#{Name => Station, {PositionX, PositionY} => Station},
        measurements = Measurements#{Station => []}
      };
    true -> stationAlreadyExistsError
  end;

addStation(Monitor, _, _) when is_record(Monitor, monitor) ->
  badPositionError;

addStation(_, _, _) ->
  badMonitorError.

%Dodaje do monitora Monitor pomiar dla stacji po NameOrPosition, z dnia Date, typu Type i wartości Value
addValue(Monitor, NameOrPosition, Date, Type, Value) when is_record(Monitor, monitor) ->
  case not is_map_key(NameOrPosition, Monitor#monitor.stations) of
    false ->
      case getOneValue(Monitor, NameOrPosition, Date, Type) of
        [] ->
          {monitor, Stations, Measurements} = Monitor,
          Measurement = #measurement{date = Date, type = Type, value = Value},
          Station = maps:get(NameOrPosition, Stations),
          MeasurementList = maps:get(Station, Measurements, []),
          #monitor{
            stations = Stations,
            measurements = Measurements#{Station => [Measurement | MeasurementList]}
          };
        _ -> measurementAlreadyExistsError
      end;
    true -> stationDoesNotExistError
  end;

addValue(_, _, _, _, _) ->
  badMonitorError.

%Usuwa z monitora Monitor pomiar ze stacji po NameOrPosition, z dnia Date i typu Type
removeValue(Monitor, NameOrPosition, Date, Type) when is_record(Monitor, monitor) ->
  case not is_map_key(NameOrPosition, Monitor#monitor.stations) of
    false ->
      {monitor, Stations, Measurements} = Monitor,
      Station = maps:get(NameOrPosition, Stations),
      MeasurementList = maps:get(Station, Measurements, []),
      NewMeasurementList = deleteFromList(MeasurementList, Date, Type),
      #monitor{
        stations = Stations,
        measurements = Measurements#{Station => NewMeasurementList}
      };
    true -> stationDoesNotExistError
  end;

removeValue(_, _, _, _) ->
  badMonitorError.

%Zwraca listę bez pomiaru z dnia Date typu Type
deleteFromList([], _, _) -> [];
deleteFromList([{measurement, Date, Type, _} | Tail], Date, Type) -> Tail;
deleteFromList([Head | []], _, _) -> Head;
deleteFromList([Head | Tail], Date, Type) -> Head ++ deleteFromList(Tail, Date, Type).

%Zwraca z monitora Monitor wartość pomiaru ze stacji po NameOrPosition, z dnia Date i typu Type
getOneValue(Monitor, NameOrPosition, Date, Type) when is_record(Monitor, monitor) ->
  case not is_map_key(NameOrPosition, Monitor#monitor.stations) of
    false ->
      {monitor, Stations, Measurements} = Monitor,
      Station = maps:get(NameOrPosition, Stations),
      MeasurementList = maps:get(Station, Measurements, []),
      getValueFromList(MeasurementList, Date, Type);
    true -> stationDoesNotExistError
  end;

getOneValue(_, _, _, _) ->
  badMonitorError.

%Zwraca wartość pomiaru z listy o dacie Date i typu Type
getValueFromList([], _, _) -> [];
getValueFromList([{measurement, Date, Type, Value} | _], Date, Type) -> Value;
getValueFromList([_ | Tail], Date, Type) -> getValueFromList(Tail, Date, Type).

%Zwraca z monitora Monitor średnią wartość pomiarów ze stacji po NameOrPosition typu Type
getStationMean(Monitor, NameOrPosition, Type) when is_record(Monitor, monitor) ->
  case not is_map_key(NameOrPosition, Monitor#monitor.stations) of
    false ->
      {monitor, Stations, Measurements} = Monitor,
      Station = maps:get(NameOrPosition, Stations),
      MeasurementList = maps:get(Station, Measurements, []),
      TypeMeasurements = lists:filter(fun(ThisMeasurement) -> ThisMeasurement#measurement.type == Type end, MeasurementList),
      Sum = lists:foldl(fun(ThisMeasurement, Sum) -> ThisMeasurement#measurement.value + Sum end, 0,TypeMeasurements),
      Length = length(lists:filter(fun(ThisMeasurement) -> ThisMeasurement#measurement.type == Type end, MeasurementList)),
      Sum / Length;
    true -> stationDoesNotExistError
  end;

getStationMean(_, _, _) ->
  badMonitorError.

%Zwraca z monitora Monitor średnią wartość pomiarów z dnia Date typu Type
getDailyMean(Monitor, Date, Type) when is_record(Monitor, monitor) ->
  {monitor, _, Measurements} = Monitor,
  MeasurementListsList = maps:values(Measurements),
  MeasurementList = concatLists(MeasurementListsList),
  FilteredList = lists:filter(fun(ThisMeasurement) -> ThisMeasurement#measurement.type == Type end,
    lists:filter(fun(ThisMeasurement) -> ThisMeasurement#measurement.date == Date end, MeasurementList)),
  lists:foldl(fun(X, Sum) -> X#measurement.value + Sum end, 0,FilteredList) / length(FilteredList);

getDailyMean(_, _, _) ->
  badMonitorError.

%Zwraca z monitora Monitor średnią wartość pomiarów z obszarów oddalonych o Radius od stacji po NameOrPosition typu Type
getAreaMean(Monitor, NameOrPosition, Radius, Type) when is_record(Monitor, monitor) ->
  case not is_map_key(NameOrPosition, Monitor#monitor.stations) of
    false ->
      {monitor, Stations, Measurements} = Monitor,
      Station = maps:get(NameOrPosition, Stations),
      {station, _, {PositionX, PositionY}} = Station,
      StationsList = maps:values(Stations),
      InAreaStations = lists:filter(fun(ThisStation) -> {SPositionX, SPositionY} = ThisStation#station.position,
        math:sqrt(math:pow(SPositionX - PositionX, 2) + math:pow(SPositionY - PositionY,2)) < Radius end, StationsList),
      InAreaMeasurementsLists = lists:map(fun(ThisStation) -> maps:get(ThisStation, Measurements, []) end, InAreaStations),
      InAreaMeasurements = concatLists(InAreaMeasurementsLists),
      lists:foldl(fun(X, Y) -> X#measurement.value + Y end, 0, lists:filter(fun(X) -> X#measurement.type == Type end,
        InAreaMeasurements)) / length(InAreaMeasurements);
    true -> stationDoesNotExistError
  end;

getAreaMean(_, _, _, _) ->
  badMonitorError.

%Zwraca tablicę z tablicy tablic
concatLists([]) -> [];
concatLists([[]| Tail]) -> concatLists(Tail);
concatLists([Head | []]) -> Head;
concatLists([Head | Tail]) -> lists:merge(Head, concatLists(Tail)).
