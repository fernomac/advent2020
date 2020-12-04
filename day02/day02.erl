-module(day02).
-export([main/0]).

main() ->
  Lines = load(),
  Counts = validate(Lines),
  io:format("~p~n", [Counts]).

% Run the two validations on the list of passwords and count the number of valid ones
% according to each ruleset.
validate(Lines) ->
  validate(0, 0, Lines).

validate(Count1, Count2, []) ->
  {Count1, Count2};

validate(Count1, Count2, [Head | Rest]) ->
  Add1 = case validate1(Head) of
    true  -> 1;
    false -> 0
  end,
  Add2 = case validate2(Head) of
    true  -> 1;
    false -> 0
  end,
  validate(Count1+Add1, Count2+Add2, Rest).

% Part one: the password is valid if the number of occurances of the target character is
% within the specified range.
validate1({Range, Target, Password}) ->
  validate1(0, Range, Target, Password).

validate1(Count, {Min, Max}, _, []) ->
  Count >= Min andalso Count =< Max;

validate1(Count, Range, Target, [Head | Rest]) ->
  if
    Target == Head -> validate1(Count+1, Range, Target, Rest);
    true           -> validate1(Count, Range, Target, Rest)
  end.

% Part two: the password is valid if exactly one of the characters in the two specified
% positions equals the target.
validate2({{I, J}, Target, Password}) ->
  One = lists:nth(I, Password) == Target,
  Two = lists:nth(J, Password) == Target,
  One xor Two.

% Load the input file as a set of parsed lines.
load() ->
  {ok, File} = file:open("input.txt", [read]),
  {ok, Text} = file:read(File, 1024*1024),
  Lines = string:split(Text, "\n", all),
  lists:map(fun parse/1, Lines).

parse(Line) ->
  Parts = string:split(Line, " ", all),
  {
    parse_ints(lists:nth(1, Parts)),
    lists:nth(1, lists:nth(2, Parts)),
    lists:nth(3, Parts)
  }.

parse_ints(Part) ->
  Parts = string:split(Part, "-", all),
  {
    list_to_integer(lists:nth(1, Parts)),
    list_to_integer(lists:nth(2, Parts))
  }.
