-module(day23).
-export([main/0]).
-include_lib("eunit/include/eunit.hrl").

main() ->
  Input = "158937462",
  io:format("~s~n", [part_one(Input)]),
  io:format("~p~n", [part_two(Input)]),
  ok.

% Part one: simulate 100 rounds with the 9-element ring.

part_one(Input) -> part_one(Input, 100).

part_one(Input, N) ->
  {Cur, Ring} = to_ring(parse(Input)),
  Result = steps(Cur, Ring, N),
  format(take_after(8, 1, Result)).

-ifdef(TEST).
part_one_test() ->
  ?assertEqual("92658374", part_one("389125467", 10)),
  ?assertEqual("67384529", part_one("389125467", 100)).
-endif.


% Part two: simulate 10 million rounds with a one-million-element ring.

part_two(Input) ->
  {Cur, Ring} = to_ring(parse(Input) ++ lists:seq(10, 1000000)),
  Result = steps(Cur, Ring, 10000000),
  [A, B] = take_after(2, 1, Result),
  A * B.

-ifdef(TEST).
part_two_test() ->
  ?assertEqual(149245887792, part_two("389125467")).
-endif.


% Parse from and format to strings.

parse(String) -> lists:map(fun (E) -> E - $0 end, String).
format(List)  -> lists:map(fun (E) -> E + $0 end, List).


% Run N steps of the game.

steps(Cur, Ring, N) when N > 0 ->
  {Next, NewRing} = step(Cur, Ring),
  steps(Next, NewRing, N-1);

steps(_,   Ring, 0)   -> Ring.


% Run a single step of the game.

step(Cur, Ring) ->
  Move1 = maps:get(Cur, Ring),
  Move2 = maps:get(Move1, Ring),
  Move3 = maps:get(Move2, Ring),
  Next  = maps:get(Move3, Ring),

  Target  = target(Cur, Move1, Move2, Move3, map_size(Ring)),
  Target2 = maps:get(Target, Ring),

  {Next, Ring#{
    Cur    => Next,
    Target => Move1,
    Move3  => Target2
  }}.


% Determine the target cup: the first cup left in the ring whose value is smaller
% than the current cup (wrapping around to the largest cup in the ring if needed).

target(C, M1, M2, M3, Size) -> target_helper(C-1, M1, M2, M3, Size).

target_helper(T, M1, M2, M3, Size) when T == 0 ->
  target_helper(Size, M1, M2, M3, Size);

target_helper(T, M1, M2, M3, Size) when T == M1 orelse T == M2 orelse T == M3 ->
  target_helper(T-1,  M1, M2, M3, Size);

target_helper(T, _, _, _, _) -> T.


% Transform a permuted list to a ring: a circular linked list represented as a
% map from an element to the element following it in the ring.

to_ring(List) -> to_ring(List, #{}).

to_ring([First, Second | Rest], Acc) ->
  to_ring(First, [Second | Rest], Acc#{ First => Second }).

to_ring(First, [E1, E2 | Rest], Acc) ->
  to_ring(First, [E2 | Rest], Acc#{ E1 => E2 });

to_ring(First, [Last], Acc) ->
  {First, Acc#{ Last => First }}.


% Takes N elements of the ring following From and returns them in a list.

take_after(N, From, Ring) -> take_after(N, From, Ring, []).

take_after(N, From, Ring, Acc) when N > 0 ->
  Next = maps:get(From, Ring),
  take_after(N-1, Next, Ring, [Next | Acc]);

take_after(0, _, _, Acc) ->
  lists:reverse(Acc).



-ifdef(TEST).
to_ring_test() ->
  Ring = #{3 => 2, 2 => 1, 1 => 3},
  ?assertEqual({3, Ring}, to_ring([3,2,1])).

take_after_test() ->
  {_, Ring} = to_ring([3,1,4,2]),
  ?assertEqual([3], take_after(1, 2, Ring)),
  ?assertEqual([4,2], take_after(2, 1, Ring)),
  ?assertEqual([3,1,4,2,3], take_after(5, 2, Ring)).
-endif.
