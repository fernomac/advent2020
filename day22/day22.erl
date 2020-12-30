-module(day22).
-export([main/0]).
-include_lib("eunit/include/eunit.hrl").

main() ->
  P1 = load("p1.txt"),
  P2 = load("p2.txt"),
  % io:format("~p~n~p~n", [P1, P2]),
  io:format("~p~n", [score(play_std(P1, P2))]),
  io:format("~p~n", [score(play_rec(P1, P2))]),
  ok.


% Part one: play a standard game of combat.

play_std(P1, [])                            -> P1;
play_std([], P2)                            -> P2;
play_std([C1 | R1], [C2 | R2]) when C1 > C2 -> play_std(R1 ++ [C1, C2], R2);
play_std([C1 | R1], [C2 | R2]) when C1 < C2 -> play_std(R1, R2 ++ [C2, C1]).


-ifdef(TEST).
play_std_test() ->
  P1 = [9, 2, 6, 3, 1],
  P2 = [5, 8, 4, 7, 10],
  ?assertEqual([3,2,10,6,8,5,9,4,7,1], play_std(P1, P2)).
-endif.


% Part two: play a game of recursive combat.

play_rec(P1, P2) ->
  {_, H} = play_rec_helper(P1, P2, #{}),
  H.

play_rec_helper(P1, [], _) -> {1, P1};
play_rec_helper([], P2, _) -> {2, P2};
play_rec_helper(P1, P2, Mem) ->
  Key = {P1, P2},
  case maps:is_key(Key, Mem) of
    true  -> {1, P1};
    false ->
      Winner = play_rec_helper(P1, P2),
      {NP1, NP2} = new_hands(P1, P2, Winner),
      play_rec_helper(NP1, NP2, Mem#{ Key => true })
  end.

play_rec_helper([C1 | R1], [C2 | R2]) when C1 =< length(R1) andalso C2 =< length(R2) ->
  {W, _} = play_rec_helper(lists:sublist(R1, C1), lists:sublist(R2, C2), #{}),
  W;
play_rec_helper([C1 | _], [C2 | _]) when C1 > C2 -> 1;
play_rec_helper([C1 | _], [C2 | _]) when C1 < C2 -> 2.

new_hands([C1 | R1], [C2 | R2], 1) -> {R1 ++ [C1, C2], R2};
new_hands([C1 | R1], [C2 | R2], 2) -> {R1, R2 ++ [C2, C1]}.

-ifdef(TEST).
play_rec_test() ->
  P1 = [9, 2, 6, 3, 1],
  P2 = [5, 8, 4, 7, 10],
  ?assertEqual([7,5,6,2,4,1,10,8,9,3], play_rec(P1, P2)).
-endif.


% Calculates the score of a given ending hand: 1 times the final card
% plus two times the penultimate card plus ...

score(P) -> score(P, length(P), 0).

score([], _, Acc) -> Acc;
score([C | Rest], Mul, Acc) -> score(Rest, Mul-1, Acc+(C*Mul)).

-ifdef(TEST).
score_test() ->
  ?assertEqual(306, score([3,2,10,6,8,5,9,4,7,1])).
-endif.


% Loads the given input file.

load(Filename) ->
  {ok, File} = file:open(Filename, [read]),
  {ok, Text} = file:read(File, 1024*1024),
  parse(string:split(Text, "\n", all)).

parse(Lines) -> lists:map(fun list_to_integer/1, Lines).
