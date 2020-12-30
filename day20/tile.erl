-module(tile).
-export([load/1, parse/1, size/1, at/3, rotate/2, flip/1, print/1, print_one/1]).
-export([left/1, right/1, top/1, bottom/1, merge/1]).
-include_lib("eunit/include/eunit.hrl").

-record(tile, {rows}).

% Loads input from the given filename.

load(Filename) ->
  {ok, File} = file:open(Filename, [read]),
  {ok, Text} = file:read(File, 1024*1024),
  parse(string:split(Text, "\n", all)).


% Parses a set of input lines into a map from tile ID to tile data.

parse(Lines) -> parse(Lines, undefined, #{}).

parse([Line | Rest], undefined, Acc) ->
  {ID, _} = string:take(string:prefix(Line, "Tile "), "0123456789"),
  parse(Rest, {list_to_integer(ID), []}, Acc);

parse([Line | Rest], {ID, Rows}, Acc) when length(Line) > 0 ->
  parse(Rest, {ID, [Line | Rows]}, Acc);

parse(["" | Rest], {ID, Rows}, Acc) ->
  parse(Rest, undefined, Acc#{ ID => #tile{rows=lists:reverse(Rows)} });

parse([], Tile, Acc) when Tile /= undefined -> parse([""], Tile, Acc);
parse([], undefined, Acc) -> Acc.

% Returns the size of the tile.

size({rotate, _, Tile}) -> tile:size(Tile);
size({flip, Tile}) -> tile:size(Tile);
size(Tile) -> length(Tile#tile.rows).

% Returns the character at position X, Y on the tile.

at({rotate, D, Tile}, X, Y) ->
  Max = tile:size(Tile) + 1,
  {XP, YP} = case D of
     90 -> {      Y, Max - X};
    180 -> {Max - X, Max - Y};
    270 -> {Max - Y,       X}
  end,
  at(Tile, XP, YP);
at({flip,      Tile}, X, Y) ->
  Max = tile:size(Tile) + 1,
  at(Tile, Max - X, Y);
at(            Tile,  X, Y) ->
  lists:nth(X, lists:nth(Y, Tile#tile.rows)).


% Rotates a tile clockwise D degrees.
rotate(Tile, D) -> {rotate, D, Tile}.

% Flips a tile over horizontally.
flip(Tile)      -> {flip, Tile}.


% Helpers to access the edges of the tile.

top(Tile) ->
  Max = tile:size(Tile),
  lists:map(fun (X) -> at(Tile, X, 1) end, lists:seq(1, Max)).
bottom(Tile) ->
  Max = tile:size(Tile),
  lists:map(fun (X) -> at(Tile, X, Max) end, lists:seq(1, Max)).
left(Tile) ->
  Max = tile:size(Tile),
  lists:map(fun (Y) -> at(Tile, 1, Y) end, lists:seq(1, Max)).
right(Tile) ->
  Max = tile:size(Tile),
  lists:map(fun (Y) -> at(Tile, Max, Y) end, lists:seq(1, Max)).

row(Tile, Y) ->
  lists:map(fun (X) -> at(Tile, X, Y) end, lists:seq(1, tile:size(Tile))).

% Merges a grid of tiles into a single super-tile.

merge(Tiles) ->
  Rows = lists:flatmap(fun (Row) -> merge_row(Row) end, Tiles),
  #tile{rows = lists:reverse(Rows)}.

% Merges a horizontal row of tiles into a set of horizontal rows of pixels.

merge_row(Tiles) ->
  lists:map(fun (Y) ->
    merge_row(Tiles, Y)
  end, lists:seq(2, tile:size(hd(Tiles))-1)).

% Merges a single Y line across multiple tiles.

merge_row(Tiles, Y) ->
  lists:flatmap(fun (Tile) ->
    Row = row(Tile, Y),
    lists:sublist(Row, 2, length(Row)-1)
  end, Tiles).


% Prints a sequence of tiles.

print(Tiles) ->
  lists:foreach(fun (Y) ->
    print_row(Tiles, Y)
  end, lists:seq(1, tile:size(hd(Tiles)))).

print_row([Tile | Rest], Y) ->
  print_one_row(Tile, Y),
  io:format(" "),
  print_row(Rest, Y);
print_row([], _) -> io:format("~n").

print_one(Tile) ->
  Seq = lists:seq(1, tile:size(Tile)),
  lists:foreach(fun (Y) ->
    print_one_row(Tile, Y),
    io:format("~n")
  end, Seq).

print_one_row(Tile, Y) ->
  lists:foreach(fun (X) ->
    io:format("~c", [at(Tile, X, Y)])
  end, lists:seq(1, tile:size(Tile))).


-ifdef(TEST).
test_tile() ->
  Tiles = parse([
    "Tile 2311:",
    "..##.#..#.",
    "##..#.....",
    "#...##..#.",
    "####.#...#",
    "##.##.###.",
    "##...#.###",
    ".#.#.#..##",
    "..#....#..",
    "###...#.#.",
    "..###..###"
  ]),
  maps:get(2311, Tiles).

at_test() ->
  Tile = test_tile(),
  ?assertEqual($., at(Tile, 1, 1)),
  ?assertEqual($., at(Tile, 2, 1)),
  ?assertEqual($#, at(Tile, 3, 1)),
  ?assertEqual($#, at(Tile, 1, 2)).

edges_test() ->
  Tile = test_tile(),
  ?assertEqual("..##.#..#.", top(Tile)),
  ?assertEqual(".#####..#.", left(Tile)),
  ?assertEqual("...#.##..#", right(Tile)),
  ?assertEqual("..###..###", bottom(Tile)).

flip_test() ->
  Tile = flip(test_tile()),
  ?assertEqual(".#..#.##..", top(Tile)),
  ?assertEqual("...#.##..#", left(Tile)).

rotate_test() ->
  Tile = rotate(test_tile(), 90),
  ?assertEqual(".#..#####.", top(Tile)),
  ?assertEqual("..##.#..#.", right(Tile)).
-endif.
