-module(day05).
-export([main/0]).

main() ->
  Seats = load(),
  {Min, Max, Result} = process(99999999, 0, #{}, Seats),
  Mine = find_missing(Min, Result),
  io:format("Min: ~p~nMax: ~p~nMine: ~p~n", [Min, Max, Mine]).

% Process the list of seats to find the min seat ID, max seat ID, and build a lookup
% table by ID.
process(Min, Max, Result, []) ->
  {Min, Max, Result};
process(Min, Max, Result, [{Row, Seat} | Rest]) ->
  ID = (Row * 8) + Seat,
  NewMin = if Min > ID -> ID; true -> Min end,
  NewMax = if Max < ID -> ID; true -> Max end,
  process(NewMin, NewMax, Result#{ID => true}, Rest).

% Finds the first missing seat at or after ID I.
find_missing(I, Map) ->
  case maps:is_key(I, Map) of
    false -> I;
    true -> find_missing(I+1, Map)
  end.

% Loads in the input from a file, returning a list of key:value maps for each passport.
load() ->
  {ok, File} = file:open("input.txt", [read]),
  {ok, Text} = file:read(File, 1024*1024),
  Lines = string:split(Text, "\n", all),
  lists:map(fun parse_bsp/1, Lines).

% Parses a BSP seat designator. The first 7 characters (F=0, B=1) indicate the row,
% the next 3 (L=0, R=1) designate the seat.
parse_bsp(S) ->
  {
    parse_row(string:slice(S, 0, 7)),
    parse_seat(string:slice(S, 7))
  }.

parse_row(S) ->
  parse_binary($F, $B, 0, S).

parse_seat(S) ->
  parse_binary($L, $R, 0, S).

% Parses a binary integer whose digits are represented by the characters in Zero and One.
parse_binary(_, _, Acc, []) ->
  Acc;
parse_binary(Zero, One, Acc, [Head | Rest]) ->
  Bit = case Head of Zero -> 0; One -> 1 end,
  parse_binary(Zero, One, Acc*2 + Bit, Rest).
