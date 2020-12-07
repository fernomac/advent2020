-module(day04).
-export([main/0]).

main() ->
  Data = load(),
  Valid1 = length(lists:filter(fun validate1/1, Data)),
  Valid2 = length(lists:filter(fun validate2/1, Data)),
  io:format("~p~n~p~n", [Valid1, Valid2]).

% Part one: count the passports with all of the required fields.
validate1(Map) -> 
  maps:is_key("byr", Map) andalso
  maps:is_key("iyr", Map) andalso
  maps:is_key("eyr", Map) andalso
  maps:is_key("hgt", Map) andalso
  maps:is_key("hcl", Map) andalso
  maps:is_key("ecl", Map) andalso
  maps:is_key("pid", Map).

% Part two: count the passports with all of the required fields and valid values
% for those fields.
validate2(Map) ->
  valid_int("byr", 1920, 2002, Map) andalso
  valid_int("iyr", 2010, 2020, Map) andalso
  valid_int("eyr", 2020, 2030, Map) andalso
  valid_hgt(Map) andalso
  valid_hcl(Map) andalso
  valid_ecl(Map) andalso
  valid_pid(Map) andalso
  true.

% Checks if the passport's hgt (height) is valid.
valid_hgt(Map) ->
  case maps:find("hgt", Map) of
    {ok, Value} ->
      {N, U} = string:take(Value, "0123456789"),
      case parse_int(N) of
        {ok, I} ->

          if
            U == "cm" -> I >= 150 andalso I =< 193;
            U == "in" -> I >= 59 andalso I =< 76;
            true      -> false
          end;

        error -> false
      end;
    error -> false
  end.

% Checks if the passport's hcl (hair color) is valid.
valid_hcl(Map) ->
  valid_key("hcl", Map, fun (Value) ->
    {H, V} = string:take(Value, "#"),
    if
      H == "#" ->

        {P, R} = string:take(V, "0123456789abcdef"),
        length(P) == 6 andalso length(R) == 0;

      true -> false
    end
  end).

% Checks if the passport's ecl (eye color) is valid.
valid_ecl(Map) ->
  valid_key("ecl", Map, fun (Value) ->
    case Value of
      "amb" -> true;
      "blu" -> true;
      "brn" -> true;
      "gry" -> true;
      "grn" -> true;
      "hzl" -> true;
      "oth" -> true;
      _ -> false
    end
  end).

% Checks if the passport's pid (passport ID) is valid.
valid_pid(Map) ->
  valid_key("pid", Map, fun (Value) ->
    {P, R} = string:take(Value, "0123456789"),
    length(P) == 9 andalso length(R) == 0
  end).

% Checks if the passport's Key field is a valid integer in the given range.
valid_int(Key, Min, Max, Map) ->
  valid_key(Key, Map, fun (Value) ->
    case parse_int(Value) of
      {ok, I} -> I >= Min andalso I =< Max;
      error -> false
    end
  end).

% Parses an integer, returning {ok, <int>} if valid or error if not.
parse_int(S) ->
  try list_to_integer(S) of
    I -> {ok, I}
  catch
    badarg -> error
  end.

% Validates that the Key field of the passport exists and is valid per the given
% validator function F.
valid_key(Key, Map, F) ->
  case maps:find(Key, Map) of
    {ok, Value} -> F(Value);
    error -> false
  end.

% Loads in the input from a file, returning a list of key:value maps for each passport.
load() ->
  {ok, File} = file:open("input.txt", [read]),
  {ok, Text} = file:read(File, 1024*1024),
  Lines = string:split(Text, "\n", all),
  parse([], #{}, Lines).

% Parser base case: no more lines of input, if we have a non-empty Cur map flush it to
% the Result set.
parse(Result, Cur, []) ->
  if
    map_size(Cur) == 0 -> Result;
    true               -> [Cur | Result]
  end;  

% Parser recursive case: if we have an empty line flush the Cur map to the Result set
% and start over, otherwise parse the line and merge it into the Cur map.
parse(Result, Cur, [Head | Rest]) ->
  if
    length(Head) == 0 andalso map_size(Cur) > 0 ->
      parse([Cur | Result], #{}, Rest);
    true -> 
      parse(Result, maps:merge(Cur, parse_line(Head)), Rest)
  end.

% Parses a single line of input into a map of key:value pairs.
parse_line(Line) ->
  Pairs = lists:map(fun parse_pair/1, string:split(Line, " ", all)),
  lists:foldl(fun ({Key, Value}, Ret) -> Ret#{Key => Value} end, #{}, Pairs).

% Parses a single key:value pair.
parse_pair(Str) ->
  Parts = string:split(Str, ":"),
  {lists:nth(1, Parts), lists:nth(2, Parts)}.
