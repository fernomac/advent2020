-module(day19).
-export([main/0]).
-include_lib("eunit/include/eunit.hrl").

main() ->
  Rules = parse(load("rules.txt")),
  Input = load("input.txt"),
  io:format("~p~n", [part_one(Rules, Input)]),
  io:format("~p~n", [part_two(Rules, Input)]),
  ok.

% Part one: count the number of strings in the input that match the given set
% of rules.

part_one(Rules, Input) ->
  count_matching(Rules, Input).

% Part two: count the number of strings in the input that match the updated set
% of rules, including loop and pair rules.

part_two(Rules, Input) ->
  NewRules = Rules#{
     8 => {loop, 42},
    11 => {pair, 42, 31}
  },
  count_matching(NewRules, Input).

% Helper that counts the number of input lines matching the given set of rules.

count_matching(Rules, Input) ->
  lists:foldl(fun (I, Acc) ->
    Acc + case matches(Rules, I) of
      true  -> 1;
      false -> 0
    end
  end, 0, Input).

% Determines whether the given matcher matches the given line, applying rule
% zero with a continuation that checks if the entire input has been matched.
% Rules are one of:
%
% {literal, "literal"}     - matches a literal sequence of characters
% {sequence, [1, 2, 3]}    - applies a sequence of other rules (by ID) in order
% {choice, [{...}, {...}]] - a choice between multiple (inline) rules
% {loop, 42}               - a safe form of ID: 42 | 42 ID
% {pair, 24, 42}           - a safe form of ID: 24 42 | 24 ID 42

matches(Rules, Line) ->
  matches(Rules, maps:get(0, Rules), Line, fun nothing_left/1).

nothing_left("") -> true;
nothing_left(_)  -> false.

% Literal rule: matches if the line begins with the provided literal. If so,
% strips off the prefix and continues matching the remainder of the input.

matches(_, {literal, Literal}, Line, Continuation) ->
  case string:prefix(Line, Literal) of
    nomatch -> false;
    Suffix  -> Continuation(Suffix)
  end;

% Choice rule: matches if the line matches at least one of the child rules.
% Tries the first one, and if it fails recursively calls itself to check the
% next. If none of them succeed, returns false.

matches(Rules, {choice, [Rule | Rest]}, Line, Continuation) ->
  case matches(Rules, Rule, Line, Continuation) of
    true  -> true;
    false -> matches(Rules, {choice, Rest}, Line, Continuation)
  end;
matches(_, {choice, []}, _, _) -> false;

% Loop rule: matches any number of consecutive iterations of rule N. Conceptually,
% it's `ID: N | N ID`, except doing that naively with a choice rule will lead to an
% infinite loop. We short circuit if we fail to match `N` instead of trying `N ID`
% (which will certainly fail as well).

matches(Rules, {loop, N}, Line, Continuation) ->
  matches(Rules, maps:get(N, Rules), Line, fun (Suffix) ->
    case Continuation(Suffix) of
      true  -> true;
      false -> matches(Rules, {loop, N}, Suffix, Continuation)
    end
  end);

% Pair rule: matches any number of consecutive iterations of rule N, followed by
% the SAME number of iterations of rule O. Conceptually, it's `ID: N O | N ID O`.
% As with the loop rule we short circuit if we fail to match `N` instead of trying
% to match `N ID O` (which will likewise fail). Unlike the loop rule, we need to
% wrap the continuation with an `O` matcher each time we successfully match an `N`.

matches(Rules, {pair, N, O}, Line, Continuation) ->
  Wrapper = fun (Suffix) ->
    matches(Rules, maps:get(O, Rules), Suffix, Continuation)
  end,
  matches(Rules, maps:get(N, Rules), Line, fun (Suffix) ->
    case Wrapper(Suffix) of
      true  -> true;
      false -> matches(Rules, {pair, N, O}, Suffix, Wrapper)
    end
  end);

% Sequence rule: matches if the line matches each of the child rules in
% sequence. Evaluates the first, pushing a continuation onto the stack that
% evaluates the rest (and finally running the originally-passed-in continuation
% if each rule in the sequence returns true).

matches(Rules, {sequence, [N | Rest]}, Line, Continuation) ->
  matches(Rules, maps:get(N, Rules), Line, fun (Suffix) ->
    matches(Rules, {sequence, Rest}, Suffix, Continuation)
  end);
matches(_, {sequence, []}, Line, Continuation) -> Continuation(Line).

-ifdef(TEST).
matches_test() ->
  Input = [
    "0: 4 1 5",
    "1: 2 3 | 3 2",
    "2: 4 4 | 5 5",
    "3: 4 5 | 5 4",
    "4: \"a\"",
    "5: \"b\""
  ],
  Rules = parse(Input),
  ?assertEqual(true,  matches(Rules, "ababbb")),
  ?assertEqual(true,  matches(Rules, "abbbab")),
  ?assertEqual(false, matches(Rules, "bababa")),
  ?assertEqual(false, matches(Rules, "aaabbb")),
  ?assertEqual(false, matches(Rules, "aaaabbb")).
-endif.

-ifdef(TEST).
loop_test() ->
  Rules = #{
    0 => {sequence, [1, 11]},  % a*b
    1 => {loop, 10},           % a*
    10 => {literal, "a"},
    11 => {literal, "b"}
  },
  ?assertEqual(false, matches(Rules, "a")),
  ?assertEqual(false, matches(Rules, "aaaaa")),
  ?assertEqual(false, matches(Rules, "b")),
  ?assertEqual(true,  matches(Rules, "ab")),
  ?assertEqual(true,  matches(Rules, "aaaaab")).
-endif.

-ifdef(TEST).
pair_test() ->
  Rules = #{
    0 => {sequence, [1, 10]},    % ab[...]baab
    1 => {pair, 10, 11},       % ab[...]ba
    10 => {sequence, [12, 13]},  % ab
    11 => {sequence, [13, 12]},  % ba
    12 => {literal, "a"},
    13 => {literal, "b"}
  },
  ?assertEqual(false, matches(Rules, "ab")),
  ?assertEqual(false, matches(Rules, "ababab")),
  ?assertEqual(false, matches(Rules, "ababbaab")),
  ?assertEqual(false, matches(Rules, "abababbababa")),
  ?assertEqual(true,  matches(Rules, "abbaab")),
  ?assertEqual(true,  matches(Rules, "abababbababaab")).
-endif.

% Parses a set of rules, returning a map from rule ID to rule definition.

parse(Lines) -> maps:from_list(lists:map(fun parse_rule/1, Lines)).

parse_rule(Line) ->
  [ID, Rest] = string:split(Line, ": "),
  Parts = string:split(Rest, " | ", all),
  Rule = case length(Parts) of
    1 -> parse_clause(hd(Parts));
    _ -> {choice, lists:map(fun parse_clause/1, Parts)}
  end,
  {list_to_integer(ID), Rule}.

parse_clause(Line) when hd(Line) == $" ->
  {literal, string:trim(Line, both, "\"")};
parse_clause(Line) ->
  {sequence, lists:map(fun list_to_integer/1, string:split(Line, " ", all))}.


-ifdef(TEST).
parse_test() ->
  Rules = parse(["0: 1 2", "1: \"a\"", "2: 1 3 | 3 1", "3: \"b\""]),
  ?assertEqual(4, map_size(Rules)),
  ?assertEqual({sequence, [1, 2]}, maps:get(0, Rules)),
  ?assertEqual({literal, "a"},     maps:get(1, Rules)),
  ?assertEqual({choice, [{sequence, [1, 3]}, {sequence, [3, 1]}]}, maps:get(2, Rules)).
-endif.


% Loads the given input file.

load(Filename) ->
  {ok, File} = file:open(Filename, [read]),
  {ok, Text} = file:read(File, 1024*1024),
  string:split(Text, "\n", all).
