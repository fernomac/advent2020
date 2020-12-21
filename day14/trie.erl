-module(trie).
-export([new/0, put/3, sum/1, print/1]).
-include_lib("eunit/include/eunit.hrl").

% A representation of the machine's memory as a trie, allowing us to more compactly
% represent writes that touch lots of memory addresses.

-record(trie, {value, children}).

% Creates a new, empty trie.

new() -> #trie{children = #{}}.


% Puts a key/value pair into the trie, returning a new updated trie.

put(Trie, "", Value)  ->
  Trie#trie{value = Value};
put(Trie, [Key | Rest], Value) ->
  NewChildren = if
    Key == $X -> put_x(    Trie#trie.children,        Rest, Value);
    true      -> put_basic(Trie#trie.children, [Key], Rest, Value)
  end,
  Trie#trie{children = NewChildren}.


% A put that (conceptually) affects both the 0 and 1 branches of the trie.

put_x(Children, Rest, Value) when map_size(Children) == 0 ->
  % no children yet so we can use a compact representation
  #{ "X" => put(new(), Rest, Value) };

put_x(#{"X" := X}, Rest, Value) ->
  % children are already the same on both sides so we can keep it that way
  #{ "X" => put(X,     Rest, Value) };

put_x(Children, Rest, Value) ->
  % children are (or at least might be) different so we have to recurse down both paths
  #{
    "0" => put(maps:get("0", Children, new()), Rest, Value),
    "1" => put(maps:get("1", Children, new()), Rest, Value)
  }.


% A put that only affects one of the two branches.

put_basic(Children, Key, Rest, Value) ->
  NewChildren = maybe_split(Children),
  NewChildren#{Key => put(maps:get(Key, NewChildren, new()), Rest, Value)}.

% If we currently have two branches of the tree collapsed but we're only doing a
% write to one of them, we need to split the two branches first.
maybe_split(Children) ->
  case maps:find("X", Children) of
    {ok, Val} -> #{"0" => Val, "1" => Val};
    error     -> Children
  end.


% Calculate the sum of all values in a trie.

sum(Trie) when is_integer(Trie#trie.value) ->
  Trie#trie.value;
sum(Trie) -> 
  maps:fold(fun sum_helper/3, 0, Trie#trie.children).

sum_helper(K, V, Acc) ->
  Sum = if
    K == "X" -> 2 * sum(V);   % doubling it for both the 0 and 1 branches
    true     ->     sum(V)    % otherwise just add them up
  end,
  Acc + Sum.


% Debug helper to print out the state of the trie.

print(Trie) ->
  print(Trie, "").

print(Trie, Prefix) when is_integer(Trie#trie.value) ->
  io:format("~s: ~p~n", [Prefix, Trie#trie.value]);
print(Trie, Prefix) ->
  if
    map_size(Trie#trie.children) > 1 -> io:format("~s:~n", [Prefix]);
    true -> ok
  end,
  maps:map(fun (Key, Value) ->
    print(Value, Prefix ++ Key)
  end, Trie#trie.children).



-ifdef(TEST).
trie_test() ->
  T1 = put(new(), "X1101X", 100),
  T2 = put(   T1, "01X0XX",   1),
  ?assertEqual(208, sum(T2)).

trie_other_test() ->
  T1 = put(new(), "0X10110X1001000X10X01X01000X11X11111",        97),
  T2 = put(   T1, "0X10110X1001000X10X00X01100X01X01101", 494387917),
  ?assertEqual(63281665792, sum(T2)).
-endif.
