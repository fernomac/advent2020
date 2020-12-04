-module(day01).
-export([main/0]).

main() ->
  Ints = load(),
  Product1 = part1(Ints),
  io:fwrite("~p~n", [Product1]),
  Product2 = part2(Ints),
  io:fwrite("~p~n", [Product2]).


% Part one: find a pair of integers in the list that sum to 2020 and return their product.
part1(Ints) ->
  part1(#{}, Ints).

% Base case: we made it through the whole list without finding a match.
part1(_, []) ->
  error;

% Recursive case: have we previously seen an integer that matches I? If so we're done,
% if not keep going but remember that we've seen I.
part1(Seen, [I | Rest]) ->
  J = 2020 - I,
  Ok = maps:is_key(J, Seen),
  case Ok of
    true  -> {ok, I * J};
    false -> part1(Seen#{I => true}, Rest)
  end.

% Part two: find three integers in the list that sum to 2020.
part2(Ints) ->
  part2(#{}, [], Ints).

% Base case: we made it through the list without finding anything.
part2(_, _, []) ->
  error;

% Recursive case: have we previously seen a pair of integers that, together with
% I, sum to 2020? If so we're done, if not keep going (remembering the sums of I
% and every previously-seen integer).
part2(Sums, Seen, [I | Rest]) ->
  Sum = maps:find(2020 - I, Sums),
  case Sum of
    {ok, [J, K]} -> {ok, I * J * K}; % I + (J + K) = 2020
    error        -> part2(sums(Sums, Seen, I), [I | Seen], Rest)
  end.

% Adds the sums of every J plus K to the sums map.
sums(Sums, [], _) ->
  Sums;
sums(Sums, [J | Rest], K) ->
  sums(Sums#{J+K => [J, K]}, Rest, K).

% Loads the input from disk and returns the list of integers.
load() ->
  {ok, File} = file:open("input.txt", [read]),
  {ok, Text} = file:read(File, 1024*1024),
  Lines = string:split(Text, "\n", all),
  integerize(Lines).

integerize([]) ->
  [];
integerize([Head | Rest]) ->
  [list_to_integer(Head) | integerize(Rest)].
