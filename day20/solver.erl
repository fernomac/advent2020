-module(solver).
-export([solve/1]).
-include_lib("eunit/include/eunit.hrl").

% Solve the puzzle by placing tiles onto a grid in appropriate places.

solve(Tiles) ->
  Adjs = build_adjacencies(Tiles),
  Size = trunc(math:sqrt(map_size(Tiles))),
  {ok, Grid} = solve(Tiles, Adjs, 1, 1, Size, grid:new(Size)),
  Grid.

% The top left corner: find the corner tiles and pick one that we don't have
% to reorient because there happens to be one of those in my input data. :)

solve(Tiles, Adjs, X, Y, Size, Grid) when X == 1 andalso Y == 1 ->
  Corners = find_corners(Adjs),
  [{TL, _}] = lists:filter(fun ({_, V}) ->
    maps:is_key(left, V) andalso maps:is_key(top, V)
  end, Corners),
  NewGrid = grid:put(X, Y, TL, maps:get(TL, Tiles), Grid),
  % io:format("solve(1, 1): ~p~n", [TL]),
  solve(maps:remove(TL, Tiles), Adjs, X+1, Y, Size, NewGrid);

% The rest of the top row: pick the only tile that can be adjacent to the
% right edge of the previous tile (as luck has it there is only one), and
% orient it appropriately.

solve(Tiles, Adjs, X, Y, Size, Grid) when X =< Size andalso Y == 1 ->
  Prev = grid:tile_at(X-1, 1, Grid),
  Right = tile:right(Prev),
  Options = maps:get(Right, Adjs),
  solve_right(Tiles, Adjs, X, Y, Size, Grid, Options);

% Off the edge of a row: start solving the next row.

solve(Tiles, Adjs, X, Y, Size, Grid) when X > Size ->
  solve(Tiles, Adjs, 1, Y+1, Size, Grid);

% Down the left column of the grid: pick a tile that can be adjacent to the
% bottom edge of the row above.

solve(Tiles, Adjs, X, Y, Size, Grid) when X == 1 andalso Y =< Size ->
  Prev = grid:tile_at(X, Y-1, Grid),
  Bottom = tile:bottom(Prev),
  Options = maps:get(Bottom, Adjs),
  solve_down(Tiles, Adjs, X, Y, Size, Grid, Options);

% The rest of the tiles: match both the tile to the left AND the tile above.
solve(Tiles, Adjs, X, Y, Size, Grid) when X =< Size andalso Y =< Size ->
  Prev = grid:tile_at(X-1, Y, Grid),
  Right = tile:right(Prev),
  Options = maps:get(Right, Adjs),
  solve_both(Tiles, Adjs, X, Y, Size, Grid, Options);

% Off the bottom edge of the grid: we're all finished!

solve(_, _, _, Y, Size, Grid) when Y > Size -> {ok, Grid}.


solve_right(Tiles, Adjs, X, Y, Size, Grid, [Option | Rest]) ->
  case solve_right_helper(Tiles, Adjs, X, Y, Size, Grid, Option) of
    {ok, Ret} -> {ok, Ret};
    error     -> solve_right(Tiles, Adjs, X, Y, Size, Grid, Rest)
  end;
solve_right(_, _, _, _, _, _, []) -> error.

solve_right_helper(Tiles, Adjs, X, Y, Size, Grid, {ID, Side, Flip}) ->
  case maps:find(ID, Tiles) of
    {ok, T} ->
      Tile = orient_left(T, Side, Flip),
      NewGrid = grid:put(X, Y, ID, Tile, Grid),
      % io:format("solve(~p, ~p): ~p~n", [X, Y, ID]),
      case solve(maps:remove(ID, Tiles), Adjs, X+1, Y, Size, NewGrid) of
        {ok, Ret} -> {ok, Ret};
        error     -> error
      end;
    error -> error
  end.


solve_down(Tiles, Adjs, X, Y, Size, Grid, [Option | Rest]) ->
  case solve_down_helper(Tiles, Adjs, X, Y, Size, Grid, Option) of
    {ok, Ret} -> {ok, Ret};
    error     -> solve_down(Tiles, Adjs, X, Y, Size, Grid, Rest)
  end;
solve_down(_, _, _, _, _, _, []) -> error.

solve_down_helper(Tiles, Adjs, X, Y, Size, Grid,{ID, Side, Flip}) ->
  case maps:find(ID, Tiles) of
    {ok, T} ->
      Tile = orient_up(T, Side, Flip),
      NewGrid = grid:put(X, Y, ID, Tile, Grid),
      % io:format("solve(~p, ~p): ~p~n", [X, Y, ID]),
      case solve(maps:remove(ID, Tiles), Adjs, X+1, Y, Size, NewGrid) of
        {ok, Ret} -> {ok, Ret};
        error     -> error
      end;
    error -> error
  end.


solve_both(Tiles, Adjs, X, Y, Size, Grid, [Option | Rest]) ->
  case solve_both_helper(Tiles, Adjs, X, Y, Size, Grid, Option) of
    {ok, Ret} -> {ok, Ret};
    error     -> solve_both(Tiles, Adjs, X, Y, Size, Grid, Rest)
  end;
solve_both(_, _, _, _, _, _, []) -> error.

solve_both_helper(Tiles, Adjs, X, Y, Size, Grid, {ID, Side, Flip}) ->
  case maps:find(ID, Tiles) of
    {ok, T} ->
      Tile = orient_left(T, Side, Flip),
      Above = grid:tile_at(X, Y-1, Grid),
      Top = tile:top(Tile),
      Match = tile:bottom(Above),
      case Top of
        Match ->
          NewGrid = grid:put(X, Y, ID, Tile, Grid),
          % io:format("solve(~p, ~p): ~p~n", [X, Y, ID]),
          case solve(maps:remove(ID, Tiles), Adjs, X+1, Y, Size, NewGrid) of
            {ok, Ret} -> {ok, Ret};
            error     -> error
          end;
        _ -> error
      end;
    error -> error
  end.


orient_left(Tile, Side, Flip) ->
  case {Side, Flip} of
    {left,   false} -> Tile;
    {left,   true}  -> tile:rotate(tile:flip(Tile), 180);
    {right,  false} -> tile:flip(Tile);
    {right,  true}  -> tile:rotate(Tile, 180);
    {top,    false} -> tile:rotate(tile:flip(Tile), 270);
    {top,    true}  -> tile:rotate(Tile, 270);
    {bottom, false} -> tile:rotate(Tile, 90);
    {bottom, true}  -> tile:rotate(tile:flip(Tile), 90)
  end.

orient_up(Tile, Side, Flip) ->
  case {Side, Flip} of
    {left,   false} -> tile:rotate(tile:flip(Tile), 270);
    {left,   true}  -> tile:rotate(Tile, 90);
    {right,  false} -> tile:rotate(Tile, 270);
    {right,  true}  -> tile:rotate(tile:flip(Tile), 90);
    {top,    false} -> Tile;
    {top,    true}  -> tile:flip(Tile);
    {bottom, false} -> tile:rotate(tile:flip(Tile), 180);
    {bottom, true}  -> tile:rotate(Tile, 180)
  end.


% Builds an index of which tiles/orientations are able to match a given pattern.

build_adjacencies(Tiles) ->
  maps:fold(fun (ID, Tile, Acc) ->
    build_adjacencies(ID, Tile, Acc)
  end, #{}, Tiles).

build_adjacencies(ID, Tile, Map) ->
  Dirs = [
    {top,    tile:top(Tile)},
    {left,   tile:left(Tile)},
    {right,  tile:right(Tile)},
    {bottom, tile:bottom(Tile)}
  ],
  lists:foldl(fun ({Side, String}, Acc) ->
    build_adjacencies(ID, Side, String, Acc)
  end, Map, Dirs).

build_adjacencies(ID, Side, String, Map) ->
  lists:foldl(fun ({Flip, Key}, Acc) ->
    Existing = maps:get(Key, Acc, []),
    Acc#{ Key => [{ID, Side, Flip} | Existing] }
  end, Map, [{false, String}, {true, lists:reverse(String)}]).


% Find corner pieces by looking for tiles with two edges that can't be adjacent
% to any other edges.

find_corners(Adjs) ->
  UnmatchedEdges = maps:filter(fun (_, V) -> length(V) == 1 end, Adjs),
  ByID = partition_by_id(UnmatchedEdges),
  fold_out_corners(ByID).

partition_by_id(Edges) ->
  Folder = fun (_, [{ID, Side, _}], Acc) ->
    ExistingSet = maps:get(ID, Acc, #{}),
    Acc#{ ID => ExistingSet#{ Side => true } }
  end,
  maps:fold(Folder, #{}, Edges).

fold_out_corners(ByID) ->
  maps:fold(fun (K, V, Acc) ->
    case map_size(V) of
      2 -> [{K, V} | Acc];
      _ -> Acc
    end
  end, [], ByID).

-ifdef(TEST).
test_data() -> tile:load("test.txt").

find_corners_test() ->
  Tiles = test_data(),
  Adj = build_adjacencies(Tiles),
  Corners = find_corners(Adj),
  ?assertEqual(4, length(Corners)).

solve_test() ->
  Soln = solve(test_data()),
  ?assertEqual("", Soln).
-endif.
