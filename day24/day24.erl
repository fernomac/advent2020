-module(day24).
-export([main/0]).
-include_lib("eunit/include/eunit.hrl").

main() ->
  Paths = load("input.txt"),
  Tiles = flip(Paths),
  io:format("~p~n", [sets:size(Tiles)]),

  Result = update(Tiles, 100),
  io:format("~p~n", [sets:size(Result)]),
  ok.


flip(Paths) -> lists:foldl(fun flip/2, sets:new(), Paths).


flip(Path, Tiles) -> flip(0, 0, Path, Tiles).

flip(X, Y, [$e | Rest], Tiles) -> flip(X+2, Y, Rest, Tiles);
flip(X, Y, [$w | Rest], Tiles) -> flip(X-2, Y, Rest, Tiles);

flip(X, Y, [$n , $e | Rest], Tiles) -> flip(X+1, Y+1, Rest, Tiles);
flip(X, Y, [$n , $w | Rest], Tiles) -> flip(X-1, Y+1, Rest, Tiles);
flip(X, Y, [$s , $e | Rest], Tiles) -> flip(X+1, Y-1, Rest, Tiles);
flip(X, Y, [$s , $w | Rest], Tiles) -> flip(X-1, Y-1, Rest, Tiles);

flip(X, Y, [], Tiles) ->
  case sets:is_element({X, Y}, Tiles) of
    true  -> sets:del_element({X, Y}, Tiles);
    false -> sets:add_element({X, Y}, Tiles)
  end.


-ifdef(TEST).
test_paths() -> [
    "sesenwnenenewseeswwswswwnenewsewsw",
    "neeenesenwnwwswnenewnwwsewnenwseswesw",
    "seswneswswsenwwnwse",
    "nwnwneseeswswnenewneswwnewseswneseene",
    "swweswneswnenwsewnwneneseenw",
    "eesenwseswswnenwswnwnwsewwnwsene",
    "sewnenenenesenwsewnenwwwse",
    "wenwwweseeeweswwwnwwe",
    "wsweesenenewnwwnwsenewsenwwsesesenwne",
    "neeswseenwwswnwswswnw",
    "nenwswwsewswnenenewsenwsenwnesesenew",
    "enewnwewneswsewnwswenweswnenwsenwsw",
    "sweneswneswneneenwnewenewwneswswnese",
    "swwesenesewenwneswnwwneseswwne",
    "enesenwswwswneneswsenwnewswseenwsese",
    "wnwnesenesenenwwnenwsewesewsesesew",
    "nenewswnwewswnenesenwnesewesw",
    "eneswnwswnwsenenwnwnwwseeswneewsenese",
    "neswnwewnwnwseenwseesewsenwsweewe",
    "wseweeenwnesenwwwswnew"
  ].

flip_test() ->
  Paths = test_paths(),
  Tiles = flip(Paths),
  ?assertEqual(10, sets:size(Tiles)).
-endif.



update(Tiles, N) when N > 0 ->
  NewTiles = visit(fun (X, Y, Acc) ->
    update_tile(X, Y, Tiles, Acc)
  end, Tiles),
  update(NewTiles, N-1);

update(Tiles, 0) -> Tiles.

visit(Visitor, Tiles) ->
  {MinX, MaxX, MinY, MaxY} = minmax(Tiles),
  lists:foldl(fun (Y, Acc1) ->
    Min = match_parity(MinX-1, Y, -1),
    Max = match_parity(MaxX+1, Y,  1),
    lists:foldl(fun (X, Acc2) ->
      Visitor(X, Y, Acc2)
    end, Acc1, lists:seq(Min, Max))
  end, Tiles, lists:seq(MinY-1, MaxY+1)).

minmax(Tiles) -> sets:fold(fun minmax/2, {0, 0, 0, 0}, Tiles).

minmax({X, Y}, {MinX, MaxX, MinY, MaxY}) -> {
    min(X, MinX), max(X, MaxX),
    min(Y, MinY), max(Y, MaxY)
  }.



match_parity(X, Y, Sign) ->
  case Y rem 2 of
    0 -> make_even(X, Sign);
    _ -> make_odd(X, Sign)
  end.

make_even(X, Sign) ->
  case X rem 2 of
    0 -> X;
    _ -> X + Sign
  end.
make_odd(X, Sign) ->
  case X rem 2 of
    0 -> X + Sign;
    _ -> X
  end.


update_tile(X, Y, Tiles, Acc) ->
  Neighbors = sets:size(sets:intersection(Tiles, sets:from_list([
    {X-2, Y},
    {X-1, Y+1},
    {X+1, Y+1},
    {X+2, Y},
    {X+1, Y-1},
    {X-1, Y-1}
  ]))),
  case sets:is_element({X, Y}, Tiles) of
    true  -> update_flipped(X, Y, Neighbors, Acc);
    false -> update_unflipped(X, Y, Neighbors, Acc)
  end.



update_unflipped(X, Y, Neighbors, Acc) ->
  case Neighbors == 2 of
    true  -> sets:add_element({X, Y}, Acc);
    false -> Acc
  end.

update_flipped(X, Y, Neighbors, Acc) ->
  case Neighbors == 0 orelse Neighbors > 2 of
    true  -> sets:del_element({X, Y}, Acc);
    false -> Acc
  end.



-ifdef(TEST).
update_test() ->
  Tiles = flip(test_paths()),
  Result = update(Tiles, 100),
  ?assertEqual(2208, sets:size(Result)).
-endif.


% Loads the given input file.

load(Filename) ->
  {ok, File} = file:open(Filename, [read]),
  {ok, Text} = file:read(File, 1024*1024),
  string:split(Text, "\n", all).
