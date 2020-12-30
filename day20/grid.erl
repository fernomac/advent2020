-module(grid).
-export([new/1, size/1, put/5, at/3, id_at/3, tile_at/3, to_list/1, to_tile/1, print/1]).
-include_lib("eunit/include/eunit.hrl").

-record(grid, {size, map}).

new(Size) -> #grid{size = Size, map = #{}}.

size(Grid) -> Grid#grid.size.

put(X, Y, ID, Tile, Grid) ->
  OldMap = Grid#grid.map,
  NewMap = OldMap#{ {X, Y} => {ID, Tile} },
  Grid#grid{ map = NewMap }.

at(X, Y, Grid) -> maps:get({X, Y}, Grid#grid.map).

id_at(X, Y, Grid) -> {ID, _} = at(X, Y, Grid), ID.

tile_at(X, Y, Grid) -> {_, Tile} = at(X, Y, Grid), Tile.

to_list(Grid) ->
  Seq = lists:seq(1, Grid#grid.size),
  Rows = lists:foldl(fun (Y, Acc) ->
    Row = lists:foldl(fun (X, Acc2) ->
      [tile_at(X, Y, Grid) | Acc2]
    end, [], Seq),
    [lists:reverse(Row) | Acc]
  end, [], Seq),
  lists:reverse(Rows).

to_tile(Grid) ->
  tile:merge(to_list(Grid)).

print(Grid) ->
  Rows = to_list(Grid),
  lists:foreach(fun (Row) ->
    tile:print(Row),
    io:format("~n")
  end, Rows).
