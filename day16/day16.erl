-module(day16).
-export([main/0]).
-include_lib("eunit/include/eunit.hrl").

-record(range, {min, max}).
-record(field, {name, r1, r2}).

-record(matchset, {index, names}).
-record(match, {index, name}).

main() ->
  Fields = fields(),
  [Mine | Nearby] = tickets(),
  
  {Valid, ErrRate} = part_one(Fields, Nearby),
  io:format("~p~n", [ErrRate]),
  io:format("~p~n", [part_two(Fields, Valid, Mine)]),

  ok.


% Part one: filter out invalid tickets, returning the remaining valid tickets
% and the sum of the invalid values.

part_one(Fields, Tickets) ->
  lists:foldl(fun (Ticket, {Valid, ErrRate}) ->
    case is_ticket_valid(Fields, Ticket) of
      true ->       {[Ticket | Valid], ErrRate};
      {false, N} -> {Valid, ErrRate + N}
    end
  end, {[], 0}, Tickets).

% Determine whether the given ticket is valid. If it is not, return the
% invalid value.

is_ticket_valid(Fields, [N | Rest]) ->
  case is_value_valid(N, Fields) of
    true  -> is_ticket_valid(Fields, Rest);
    false -> {false, N}                       % found an invalid value
  end;
is_ticket_valid(_, []) -> true.               % no invalid values means it's a valid ticket

-ifdef(TEST).
is_ticket_valid_test() ->
  Fields = test_fields(),
  ?assertEqual(true,        is_ticket_valid(Fields, [7,3,47])),
  ?assertEqual({false, 4},  is_ticket_valid(Fields, [40,4,50])),
  ?assertEqual({false, 55}, is_ticket_valid(Fields, [55,2,20])).
-endif.

% Returns true if N is a valid value for at least one of the given fields.

is_value_valid(N, [Field | Rest]) ->
  case is_valid_value_for(N, Field) of
    true  -> true;                      % matched at least one of the fields
    false -> is_value_valid(N, Rest)
  end;
is_value_valid(_, []) -> false.         % made it to the end without matching any fields


-ifdef(TEST).
is_value_valid_test() ->
  Fields = [
    #field{name="class", r1=#range{min=1,max=3},  r2=#range{min=5, max=7}},
    #field{name="row",   r1=#range{min=6,max=11}, r2=#range{min=33,max=44}},
    #field{name="seat",  r1=#range{min=13,max=40}, r2=#range{min=45,max=50}}
  ],
  ?assertEqual(true,  is_value_valid(40, Fields)),
  ?assertEqual(false, is_value_valid(4, Fields)),
  ?assertEqual(true,  is_value_valid(50, Fields)).
-endif.


% Part two: determine the names of each column and return the product of the 'departure' fields
% from my ticket.

part_two(Fields, Tickets, Mine) ->
  Columns = get_column_names(Fields, Tickets),
  Ticket = maps:from_list(lists:zip(Columns, Mine)),
  maps:fold(fun (K, V, Acc) ->
    Acc * case string:prefix(K, "departure") of
      nomatch -> 1;
      _       -> V
    end
  end, 1, Ticket).

% Figure out the names of each column by process of elimination.

get_column_names(Fields, Tickets) ->
  Matches = get_possible_matches(Fields, Tickets),

  ByLength = lists:sort(fun (A, B) ->
    length(A#matchset.names) =< length(B#matchset.names)
  end, Matches),

  {_, Result} = lists:foldl(fun (E, {Mem, Acc}) ->
    [Name] = lists:filter(fun (E2) -> not maps:is_key(E2, Mem) end, E#matchset.names),
    Match = #match{name = Name, index = E#matchset.index},
    {Mem#{Name => ok}, [Match | Acc]}
  end, {#{}, []}, ByLength),

  ByIndex = lists:sort(fun (A, B) ->
    A#match.index =< B#match.index
  end, Result),

  lists:map(fun (E) -> E#match.name end, ByIndex).

-ifdef(TEST).
get_column_names_test() ->
  Fields = [
    #field{name="class", r1=#range{min=0,max=1},  r2=#range{min=4,max=19}},
    #field{name="row",   r1=#range{min=0,max=5},  r2=#range{min=8,max=19}},
    #field{name="seat",  r1=#range{min=0,max=13}, r2=#range{min=16,max=19}}
  ],
  Tickets = [
    [3,9,18],
    [15,1,5],
    [5,14,9]
  ],
  ?assertEqual(["row","class","seat"], get_column_names(Fields, Tickets)).
-endif.

% Gets the matchset for each column of the given set of tickets.

get_possible_matches(Fields, Tickets) ->
  lists:map(fun (N) ->
    #matchset{index = N, names = get_possible_matches(N, Fields, Tickets)}
  end, lists:seq(1, length(Fields))).

% Gets a list of possible field names for the Nth column by looking at the ticket data.

get_possible_matches(N, Fields, Tickets) ->
  lists:foldl(fun (Field, Acc) ->
    case is_match_for_column(N, Field, Tickets) of
      true  -> [Field#field.name | Acc];
      false -> Acc
    end
  end, [], Fields).

% Is the given field a match for the Nth column of every ticket?

is_match_for_column(N, Field, Tickets) ->
  lists:all(fun (Ticket) ->
    is_valid_value_for(lists:nth(N, Ticket), Field)
  end, Tickets).


% Is N a valid value for the given field?

is_valid_value_for(N, Field) ->
  is_in_range(N, Field#field.r1) orelse is_in_range(N, Field#field.r2).

is_in_range(N, Range) ->
  N >= Range#range.min andalso N =< Range#range.max.


% Data loading functions.

fields() ->
  parse_fields(load("fields.txt")).

-ifdef(TEST).
test_fields() -> [
    #field{name="class", r1=#range{min=1,max=3},  r2=#range{min=5, max=7}},
    #field{name="row",   r1=#range{min=6,max=11}, r2=#range{min=33,max=44}},
    #field{name="seat",  r1=#range{min=13,max=40}, r2=#range{min=45,max=50}}
  ].
-endif.

parse_fields(Lines) ->
  parse_fields(Lines, []).

parse_fields([], Acc) -> lists:reverse(Acc);
parse_fields([First | Rest], Acc) ->
  parse_fields(Rest, [parse_field(First) | Acc]).

parse_field(Line) ->
  [Name, Rest] = string:split(Line, ": "),
  [First, Second] = string:split(Rest, " or "),
  #field{name = Name, r1 = parse_range(First), r2 = parse_range(Second)}.

parse_range(Line) ->
  [First, Second] = string:split(Line, "-"),
  #range{min = list_to_integer(First), max = list_to_integer(Second)}.

tickets() ->
  parse_tickets(load("input.txt")).

parse_tickets(Lines) ->
  parse_tickets(Lines, []).

parse_tickets([], Acc) -> lists:reverse(Acc);
parse_tickets([First | Rest], Acc) ->
  parse_tickets(Rest, [parse_ticket(First) | Acc]).

parse_ticket(Line) ->
  Parts = string:split(Line, ",", all),
  lists:map(fun list_to_integer/1, Parts).

load(Filename) ->
  {ok, File} = file:open(Filename, [read]),
  {ok, Text} = file:read(File, 1024*1024),
  string:split(Text, "\n", all).
