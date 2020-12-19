-module(day13).
-export([main/0]).

% Day 13: because it's all about that bus, 'bout that bus, no trouble.

main() ->
  {Now, Schedule} = load("input.txt"),
  io:format("~p~n", [part_one(Now, Schedule)]),
  io:format("~p~n", [part_two(Schedule)]),
  ok.

% A Bus tracks a particular bus route. It has a period (how often the bus
% visits the port; also used as its ID), and an index (which position it
% was listed in the bus schedule).

bus(Period, Index) ->
  {Period, Index}.
period(Bus) ->
  {Period, _} = Bus,
  Period.
id(Bus) ->
  period(Bus).
index(Bus) ->
  {_, Index} = Bus,
  Index.

% A Departure tracks the next departure of a bus from the port. It contains
% the ID of bus and the number of minutes until the bus departs.

departure(ID, Wait) ->
  {ID, Wait}.
bus_id(Departure) ->
  {ID, _} = Departure,
  ID.
wait(Departure) ->
  {_, Wait} = Departure,
  Wait.



% Part one: find the first bus to depart after t=Start and print the product of
% the bus ID and the amount of time we need to wait for it to depart.

part_one(Now, Schedule) ->
  Departures = departures(Now, Schedule),
  Earliest = find_earliest(Departures),
  bus_id(Earliest) * wait(Earliest).

% Finds the next departure for each bus in the given schedule.

departures(Now, Schedule) ->
  lists:map(fun (Bus) ->
    MissedBy = Now rem period(Bus),
    Wait = if
      MissedBy == 0 -> 0;
      true          -> period(Bus) - MissedBy
    end,
    departure(id(Bus), Wait)
  end, Schedule).

% Finds the earliest departure in a list of departures.

find_earliest(Departures) ->
  [First | Rest] = Departures,
  lists:foldl(fun earlier/2, First, Rest).

% Compares two departures, returning the earlier one.

earlier(Dep1, Dep2) ->
  case wait(Dep1) < wait(Dep2) of
    true -> Dep1;
    _    -> Dep2
  end.



% Part Two: find a time T such that bus N departs the station N minutes after
% T -- that is, T ≡ index(Bus[N])  (mod period(Bus[N])) for all busses.

part_two(Busses) ->
  find_t(Busses, 1, 0).

% We work our way towards a solution by building a sequence T[N] where T[N]
% satisfies the first N bus constraints. T[0] is always 0; with no constraints
% we can simply pick the smallest integer. T[1] is the smallest integer that
% satisfies the constraint for Bus[0]:
%
% T[1] = Y[1] * period(Bus[0]) - index(Bus[0])           (for some integer Y[1])
%
% T[2] is the smallest integer that matches Bus[0] AND Bus[1]. To continue
% satisfying the constraint for Bus[0], T[2] must come some number (possibly 0)
% of Bus[0] periods after T[1]:
%
% T[2] = T[1] + (X[2] * period(Bus[0]))                  (for some integer X[2])
%
% to match Bus[1], it must additionally satisfy:
%
% T[2] = (Y[2] * period(Busses[1])) - 1)                 (for some integer Y[2])
%
% Continuing on, T[3] must be both some number (possibly 0) of bus 1 periods and
% some number of bus 2 periods after T[2]:
%
% T[3] = T[2] + (X[3] * lcm(period(Bus[1]), period(Bus[2])))     (for some X[3])
%
% In addition to satisfying the condition for bus 3. Generalizing:
%
% Step[0] = 1
% Step[N] = lcm(Step[N-1], period(Bus[N]))
%
% T[0] = 0
% T[N] = T[N-1] + (X[N] * Step[N-1])                             (for some X[N])
% T[N] = (Y[N] * period(Bus[N]) - index(Bus[N])                  (for some Y[N])
%
% find_t/3 recursively builds the sequence T[N], delegating to find_next_t/3 to
% efficiently solve for T[N] given T[N-1] and Step[N-1].

find_t([], _, T) ->
  T;
find_t([Bus | Rest], PrevStep, PrevT) ->
  T = find_next_t(Bus, PrevStep, PrevT),
  Step = lcm(period(Bus), PrevStep),
  find_t(Rest, Step, T).

% Finds T[N] given T[N-1] and Step[N-1]. Conceptually, it steps T forward from
% T[N-1] in increments of Step[N-1] until it finds a T that satisfies
% T rem Period = Index. We optimize by more directly solving it as a system of
% equations. Copying from above:
%
% T[N] = T[N-1] + (X[N] * Step[N-1])                             (for some X[N])
% T[N] = (Y[N] * period(Bus[N]) - index(Bus[N])                  (for some Y[N])
%
% Setting the two equal to each other and solving for Y[N]:
%
% Y[N] * Period = T[N-1] + Index + (X[N] * Step[N-1])
% Y[N] * Period = T[N-1] + Index                                 (mod Step[N-1])
% Y[N] = (T[N-1] + index(Bus[N]) * modinv(period(Bus(N))         (mod Step[N-1])
%
% From there, we substite Y[N] back into T[N] = (Y[N] * Period) - Index to
% calculate T[N].

find_next_t(Bus, PrevStep, PrevT) ->
  Y = (PrevT + index(Bus)) * modinv(period(Bus), PrevStep),
  T = ((Y rem PrevStep) * period(Bus)) - index(Bus),
  T.

% Finds the modular inverse of A mod M: a value X such that A*X ≡ 1 (mod M).
% Returns an error if A and M are not coprime (which, conveniently, they always
% are in our puzzle input).

modinv(A, M) ->
  {X, _, GCD} = egcd(A, M),
  case GCD of
    1 -> normalize(X, M);
    _ -> error
  end.

% Normalizes X modulo M, mapping it into the range [0, M), since egcd/2 may
% return negative X values.

normalize(X, M) ->
  if
    X < 0 -> X + M;
    true  -> X
  end.

% Finds the least common multiple of A and B by dividing their product by their GCD.
% Not strictly necessary since we know all of our inputs have a GCD of 1, but we might
% as well be proper about it.

lcm(A, B) ->
  (A * B) div gcd(A, B).

% Finds the greatest common divisor of A and B by calling egcd/2 and only returning
% the interesting parts.

gcd(A, B) ->
  {_, _, GCD} = egcd(A, B),
  GCD.

% Finds greatest common divisor of A and B using the Extended Euclidean Algorithm.
% Returns {X, Y, gcd(A,B)} such that A*X + B*Y = gcd(A, B).

egcd(A, B) when A == 0 -> {0, 1, B};
egcd(A, B) ->
  {X, Y, GCD} = egcd(B rem A, A),
  {Y - (B div A) * X, X, GCD}.


% Loads the input from a file, returning a tuple of {Start, Busses}.

load(Filename) ->
  {ok, File} = file:open(Filename, [read]),
  {ok, Text} = file:read(File, 1024*1024),
  [First, Second | _] = string:split(Text, "\n", all),

  Now = list_to_integer(First),
  Busses = parse(Second),

  {Now, Busses}.

% Parses the bus schedule into a list of busses.

parse(Line) ->
  Parts = string:split(Line, ",", all),
  Busses = parse(Parts, 0, []),
  lists:reverse(Busses).

parse([], _, Acc) ->
  Acc;
parse([First | Rest], Index, Acc) ->
  case First of
    "x" -> parse(Rest, Index+1, Acc);
    _   ->
      Id = list_to_integer(First),
      parse(Rest, Index+1, [bus(Id, Index) | Acc])
  end.

