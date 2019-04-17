%%%-------------------------------------------------------------------
%%% @author ErykSikora
%%% @copyright (C) 2019, <ErykSikora>
%%% @doc
%%%
%%% @end
%%% Created : 17. kwi 2019 11:48
%%%-------------------------------------------------------------------

-module(pollution_test).
-include_lib("eunit/include/eunit.hrl").
-author("ErykSikora").

%% API
-export([]).

%% Server start test
start_test() -> ok = pollution_server:start().

%% Server addStation tests
addStation_test() -> ok = pollution_server:addStation("Kety", {10,10}).

addStation_stationAlreadyExistsError_test() ->
  ok = pollution_server:addStation("Czaniec", {11,10}),
  stationAlreadyExistsError = pollution_server:addStation("Czaniec", {11,10}).

addStation_badPositionError_test() -> badPositionError = pollution_server:addStation("Warszawa", {11,10,12}).

%% Server addValue tests
addValue_test() ->
  ok = pollution_server:addValue("Kety", {{17,04,2019}, {11,59,04}}, "Temperatura", 32),
  ok = pollution_server:addValue({10,10}, {{17,04,2019}, {11,59,07}}, "Temperatura", 29).

addValue_measurementAlreadyExistsError_test() ->
  Time = calendar:local_time(),
  ok = pollution_server:addValue("Czaniec", Time, "Temperatura", 27),
  measurementAlreadyExistsError = pollution_server:addValue("Czaniec", Time, "Temperatura", 27).

addValue_stationDoesNotExistError_test() ->
  stationDoesNotExistError = pollution_server:addValue("Warszawa", {{17,04,2019}, {11,59,04}}, "Temperatura", 27).

%% Server removeValue tests
removeValue_test() ->
  Time = calendar:local_time(),
  ok = pollution_server:addValue("Czaniec", Time, "Wilgotnosc", 1/2),
  ok = pollution_server:removeValue("Czaniec", Time, "Wilgotnosc").

removeValue_stationDoesNotExistError_test() ->
  Time = calendar:local_time(),
  stationDoesNotExistError = pollution_server:removeValue("Wilamowice", Time, "Wilgotnosc").

%% Server getOneValue tests
getOneValue_test() ->
  Time = calendar:local_time(),
  ok = pollution_server:addValue("Czaniec", Time, "Wilgotnosc", 0.5),
  0.5 = pollution_server:getOneValue("Czaniec", Time, "Wilgotnosc").

getOneValue_stationDoesNotExistError_test() ->
  Time = calendar:local_time(),
  stationDoesNotExistError = pollution_server:getOneValue("Wilamowice", Time, "Wilgotnosc").

%% Server getStationMean tests
getStationMean_test() ->
  ok = pollution_server:addValue("Kety", {1,1}, "Wilgotnosc", 0.1),
  ok = pollution_server:addValue("Kety", {2,2}, "Wilgotnosc", 0.2),
  ok = pollution_server:addValue("Kety", {3,3}, "Wilgotnosc", 0.5),
  ok = pollution_server:addValue("Kety", {4,4}, "Wilgotnosc", 0.8),
  0.4 = pollution_server:getStationMean("Kety", "Wilgotnosc").

getStationMean_stationDoesNotExistError_test() ->
  stationDoesNotExistError = pollution_server:getStationMean("Wilamowice", "Wilgotnosc").

%% Server getDailyMean tests
getDailyMean_test() ->
  ok = pollution_server:addValue("Kety", {100,100}, "Wilgotnosc", 0.1),
  ok = pollution_server:addValue("Czaniec", {100,100}, "Wilgotnosc", 0.3),
  0.2 = pollution_server:getDailyMean({100,100}, "Wilgotnosc").

%% Server getAreaMean tests
getAreaMean_test() ->
  ok = pollution_server:addStation("NY", {1000, 1000}),
  ok = pollution_server:addStation("DC", {1100, 1100}),
  ok = pollution_server:addStation("Chicago", {900, 1050}),
  ok = pollution_server:addValue("NY", {1,1}, "Temperatura", 28),
  ok = pollution_server:addValue("NY", {2,2}, "Temperatura", 30),
  ok = pollution_server:addValue("NY", {3,3}, "Temperatura", 18),
  ok = pollution_server:addValue("DC", {1,1}, "Temperatura", 24),
  ok = pollution_server:addValue("Chicago", {1,1}, "Temperatura", 20),
  ok = pollution_server:addValue("Chicago", {2,2}, "Temperatura", 18),
  23.0 = pollution_server:getAreaMean("NY", 200, "Temperatura").

getAreaMean_stationDoesNotExistError_test() ->
  stationDoesNotExistError = pollution_server:getAreaMean("Wilamowice", 100, "Wilgotnosc").

