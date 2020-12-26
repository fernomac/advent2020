-module(day18).
-export([main/0]).
-include_lib("eunit/include/eunit.hrl").


main() ->
  Lines = load("input.txt"),
  io:format("~p~n", [part_one(Lines)]),
  io:format("~p~n", [part_two(Lines)]),
  ok.

part_one(Lines) -> lists:sum(lists:map(fun (L) -> eval1(L) end, Lines)).
part_two(Lines) -> lists:sum(lists:map(fun (L) -> eval2(L) end, Lines)).

% Loads the given input file.

load(Filename) ->
  {ok, File} = file:open(Filename, [read]),
  {ok, Text} = file:read(File, 1024*1024),
  string:split(Text, "\n", all).

% Evaluate an expression using the rules from part one. All operators have equal precedence, so we
% greedily simplify by resolving the first operator in sequence: [A, op, B | ...] always simplifies
% to [ (A op B) | ... ].

eval1(Line) -> simplify1(parse(Line)).

simplify1(N) when is_integer(N) -> N;
simplify1([N])                  -> simplify1(N);
simplify1([A, "+", B | Rest])   -> Result = simplify1(A) + simplify1(B), simplify1([Result | Rest]);
simplify1([A, "*", B | Rest])   -> Result = simplify1(A) * simplify1(B), simplify1([Result | Rest]).

-ifdef(TEST).
eval1_test() ->
  ?assertEqual(   26, eval1("2 * 3 + (4 * 5)")),
  ?assertEqual(  437, eval1("5 + (8 * 3 + 9 + 3 * 4 * 3)")),
  ?assertEqual(12240, eval1("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")),
  ?assertEqual(13632, eval1("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")).
-endif.

% Evaluate an expression using the rules from part two. Addition has precedence over multiplication,
% so we defer multiplications whenever we find them: [A, +, B | ...] still simplifies to
% [(A + B) | ...], but [A, *, B | ...] instead simplifies to (A * [B | ...]).

eval2(Line) -> simplify2(parse(Line)).

simplify2(N) when is_integer(N) -> N;
simplify2([N])                  -> simplify2(N);
simplify2([A, "+", B | Rest])   -> Result = simplify2(A) + simplify2(B), simplify2([Result | Rest]);
simplify2([A, "*", B | Rest])   -> simplify2(A) * simplify2([B | Rest]).

-ifdef(TEST).
eval2_test() ->
  ?assertEqual(    51, eval2("1 + (2 * 3) + (4 * (5 + 6))")),
  ?assertEqual(    46, eval2("2 * 3 + (4 * 5)")),
  ?assertEqual(  1445, eval2("5 + (8 * 3 + 9 + 3 * 4 * 3)")),
  ?assertEqual(669060, eval2("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")),
  ?assertEqual( 23340, eval2("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")).
-endif.


% Parses a text form equation into a list-of-lists expression form.

parse(Line) -> parse(Line, [], []).

-define(SPACE(C),  C == $\s).
-define(OPERATOR(C), C == $+ orelse C == $*).
-define(OPEN_PAREN(C), C == $().
-define(CLOSE_PAREN(C), C == $)).
-define(DIGIT(C),  C >= $0 andalso C =< $9).

parse([], [], Acc) -> lists:reverse(Acc);
parse([C | Line], Stack, Acc) when ?SPACE(C)           -> parse(Line, Stack, Acc);
parse([C | Line], Stack, Acc) when ?OPERATOR(C)        -> parse(Line, Stack, [[C] | Acc]);
parse([C | Line], Stack, Acc) when ?OPEN_PAREN(C)      -> parse(Line, [Acc | Stack], []);
parse([C | Line], [PA | PS], Acc) when ?CLOSE_PAREN(C) -> parse(Line, PS, [lists:reverse(Acc) | PA]);

parse([C | Line], Stack, Acc) when ?DIGIT(C) ->
  {Cs, Rest} = string:take(Line, "0123456789"),
  N = list_to_integer([C | Cs]),
  parse(Rest, Stack, [N | Acc]).

-ifdef(TEST).
parse_test() -> ?assertEqual(
    [1, "+", [ [ 23, "+", 45 ], "*", 6 ] ],
    parse("1 + ((23 + 45) * 6)")
  ).
-endif.

