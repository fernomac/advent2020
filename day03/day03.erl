-module(day03).
-export([main/0]).

main() ->
  Map = load(),
  part1(Map),
  part2(Map).

% Part one: determine how many trees are passed at slope {3,1}
part1(Map) ->
  Ans = trees_for_slope(3, 1, Map),
  io:format("~p~n", [Ans]).

% Part two: the product of the number of trees on a fine selection of slopes.
part2(Map) ->
  Slopes = [{1,1}, {3,1}, {5,1}, {7,1}, {1,2}],
  Trees = lists:map(fun ({DX, DY}) -> trees_for_slope(DX, DY, Map) end, Slopes),
  Ans = lists:foldl(fun (X, Sum) -> X * Sum end, 1, Trees),
  io:format("~p~n", [Ans]).

% Determines how many trees are passed on the given slope through the given map.
trees_for_slope(DX, DY, Map) ->
  trees_for_slope(0, 0, 0, DX, DY, Map).

% Base case: return the accumulated count.
trees_for_slope(Count, _, _, _, _, Map) when map_size(Map) == 0 ->
  Count;
% Recursive case: add one if we've passed a tree here, then recurse to the
% next step on our course.
trees_for_slope(Count, X, Y, DX, DY, Map) ->
  NewCount = case tree_at(X, Y, Map) of
    true  -> Count + 1;
    false -> Count
  end,
  NewX = X+DX,
  NewY = Y+DY,
  NewMap = maps:filter(fun(K, _) -> K >= NewY end, Map),
  trees_for_slope(NewCount, NewX, NewY, DX, DY, NewMap).

% Determines if there is a tree at position {X, Y} in the given map.
tree_at(X, Y, Map) ->
  tree_at(X, maps:get(Y, Map)).
tree_at(X, Map) ->
  maps:get(X rem maps:size(Map), Map).

% Loads in the input from a file.
load() ->
  {ok, File} = file:open("input.txt", [read]),
  {ok, Text} = file:read(File, 1024*1024),
  Lines = string:split(Text, "\n", all),
  parse(#{}, 0, Lines).

% Parses a list of lines into a map from integer Y coordinate to parsed line.
parse(Map, _, []) ->
  Map;
parse(Map, Y, [Line | Rest]) ->
  parse(Map#{Y => parse_line(#{}, 0, Line)}, Y+1, Rest).

% Parses a line of input into a map from integer X coordinate to boolean value
% indicating the presence of a tree.
parse_line(Map, _, []) ->
  Map;
parse_line(Map, X, [Char | Rest]) ->
  parse_line(Map#{X => Char == $#}, X+1, Rest).
