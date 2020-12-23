-module(day15).
-export([main/0]).
-include_lib("eunit/include/eunit.hrl").

-record(item, {ultimate = never, penultimate = never}).

main() ->
  Input = [2,0,6,12,1,3],
  io:format("~p~n", [part_one(Input)]),
  io:format("~p~n", [part_two(Input)]),
  ok.

part_one(Vs) ->
  {N, Last, State} = start(1, Vs, 0, #{}),
  turns(N, 2020, Last, State).

-ifdef(TEST).
part_one_test() ->
  ?assertEqual(436, part_one([0, 3, 6])).
-endif.

part_two(Vs) ->
  {N, Last, State} = start(1, Vs, 0, #{}),
  turns(N, 30000000, Last, State).

start(N, [], Last, State) -> {N, Last, State};
start(N, [V | Rest], _, State) ->
  NewState = say(N, V, State),
  start(N+1, Rest, V, NewState).

turns(N, M, Last, _) when N > M -> Last;
turns(N, M, Last, State) ->
  {NewLast, NewState} = turn(N, Last, State),
  turns(N+1, M, NewLast, NewState).

turn(N, Last, State) ->
  #{ Last := Item } = State,
  V = case Item#item.penultimate of
    never -> 0;
    _     -> Item#item.ultimate - Item#item.penultimate
  end,
  NewState = say(N, V, State),
  {V, NewState}.

say(N, V, State) ->
  % io:format("~p: ~p~n", [N, V]),
  Item = maps:get(V, State, #item{}),
  NewItem = #item{ ultimate = N, penultimate = Item#item.ultimate },
  State#{ V => NewItem }.
