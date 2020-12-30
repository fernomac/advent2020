-module(day20).
-export([main/0]).
-include_lib("eunit/include/eunit.hrl").

main() ->
  Tiles = tile:load("input.txt"),
  Grid = solver:solve(Tiles),

  io:format("~p~n", [part_one(Grid)]),

  

  ok.


part_one(Grid) ->
  Size = grid:size(Grid),
  TL = grid:id_at(   1,    1, Grid),
  TR = grid:id_at(Size,    1, Grid),
  BL = grid:id_at(   1, Size, Grid),
  BR = grid:id_at(Size, Size, Grid),
  TL * TR * BL * BR.

part_two(Grid) ->
  Pic = grid:to_tile(Grid),
  

  ok.

pixels(Tile) ->
  