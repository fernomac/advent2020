-module(day06).
-export([main/0]).

main() ->
  Counts = lists:map(fun (Block) -> {length(Block), counts(Block)} end, blockify(lines())),
  Part1 = part1(Counts),
  Part2 = part2(Counts),
  io:format("~p~n~p~n", [Part1, Part2]),
  ok.

% Part one: the sum of the number of characters that appear on at least one line in a given block.
part1(Counts) ->
  lists:sum(lists:map(fun ({_, C}) -> map_size(C) end, Counts)).

% Part two: the sum of the number of characters that appear in every line in a given block.
part2(Counts) ->
  lists:sum(lists:map(fun ({N, M}) ->
      map_size(maps:filter(fun (_, V) -> V == N end, M))
    end, Counts)).

% Counts the lines and occurances of each character in each block.
counts(Blocks) ->
  lists:map(fun (Block) -> {length(Block), count_in_block(Block)} end, Blocks).

% Transforms a block into a map from char to number of occurances of that char.
count_in_block(Block) ->
  lists:foldl(fun (Line, Acc) ->
      lists:foldl(fun (Char, Acc2) ->
          Acc2#{ Char => case maps:find(Char, Acc2) of
            {ok, Count} -> Count+1;
            error       -> 1
          end }
        end, Acc, Line)
    end, #{}, Block).  

% Groups lines of input together into blocks separated by an empty line.
blockify(Lines) ->
  blockify([], [], Lines).

blockify(Result, Cur, []) ->
  if
    length(Cur) > 0 -> [Cur | Result];
    true            -> Result
  end;
blockify(Result, Cur, [Head | Rest]) ->
  if
    Head == "" -> blockify([Cur | Result], [], Rest);
    true       -> blockify(Result, [Head | Cur], Rest)
  end.

% Loads input from file as a list of lines.
lines() ->
  {ok, File} = file:open("input.txt", [read]),
  {ok, Text} = file:read(File, 1024*1024),
  string:split(Text, "\n", all).
