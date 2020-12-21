-module(day14).
-export([main/0]).
-include_lib("eunit/include/eunit.hrl").

-record(mask,  {m0 = 0, m1 = 0}).
-record(chip1, {mask = #mask{}, memory = #{}}).
-record(chip2, {mask = "", memory = trie:new()}).


main() ->
  Lines = load("input.txt"),
  io:format("~p~n", [part_one(Lines)]),
  io:format("~p~n", [part_two(Lines)]),
  ok.

% Part one: current mask impacts the value when performing writes to memory, with a 1 or 0
% forcing the given bit value and an X leaving the original bit value alone. Find the
% sum of all values in memory after running the program.

part_one(Lines) ->
  Chip = lists:foldl(fun process_one/2, #chip1{}, Lines),
  maps:fold(fun (_, Val, Acc) -> Val + Acc end, 0, Chip#chip1.memory).

process_one(Line, Chip) ->
  Mask = string:prefix(Line, "mask = "),
  if
    Mask /= nomatch -> Chip#chip1{mask = parse_mask(Mask, #mask{})};
    true            -> process_mem_one(Line, Chip)
  end.

process_mem_one(Line, Chip) ->
  {Addr, Val} = parse_mem(Line),
  MaskedVal = mask_value(Val, Chip#chip1.mask),
  Memory = (Chip#chip1.memory)#{ Addr => MaskedVal },
  Chip#chip1{memory = Memory}.


% Part two: mask impacts the address line, with X's introducing "floating" bits that
% set multiple memory locations in a single operation. As with part one, find the
% sum of all values in memory after running the program.

part_two(Lines) ->
  Chip = lists:foldl(fun process_two/2, #chip2{}, Lines),
  % trie:print(Chip#chip2.memory),
  trie:sum(Chip#chip2.memory).

-ifdef(TEST).
part_two_test() ->
  P2 = part_two([
    "mask = 000000000000000000000000000000X1001X",
    "mem[42] = 100",
    "mask = 00000000000000000000000000000000X0XX",
    "mem[26] = 1"]),
  ?assertEqual(208, P2).
-endif.

process_two(Line, Chip) ->
  Mask = string:prefix(Line, "mask = "),
  if
    Mask /= nomatch -> Chip#chip2{mask = Mask};
    true            -> process_mem_two(Line, Chip)
  end.

process_mem_two(Line, Chip) ->
  {Addr, Val} = parse_mem(Line),
  MaskedAddr = mask_addr(base_two_string(Addr), Chip#chip2.mask),
  Memory = trie:put(Chip#chip2.memory, MaskedAddr, Val),
  Chip#chip2{memory = Memory}.


% Print an integer as a 36-bit base-2 string.

base_two_string(I) -> base_two_string(I, 0, []).
base_two_string(_, 36, Acc) -> Acc;
base_two_string(I, N, Acc) ->
  Bit = case I rem 2 of
    0 -> $0;
    1 -> $1
  end,
  base_two_string(I div 2, N+1, [Bit | Acc]).

-ifdef(TEST).
base_two_string_test() ->
  ?assertEqual("000000000000000000000000000000011010", base_two_string(26)).
-endif.

% Parses a mask from string form.

parse_mask(Mask)               -> parse_mask(Mask, #mask{}).
parse_mask([], Acc)            -> Acc;
parse_mask([Char | Rest], Acc) ->
  M0 = (Acc#mask.m0 * 2) + case Char of
    $0 -> 0;
    _  -> 1
  end,
  M1 = (Acc#mask.m1 * 2) + case Char of
    $1 -> 1;
    _  -> 0
  end,
  parse_mask(Rest, #mask{m0 = M0, m1 = M1}).

-ifdef(TEST).
parse_mask_test() -> ?assertEqual(#mask{m0 = 2#110110, m1 = 2#100010}, parse_mask("1X0X10")).
-endif.


% Parses a memory-access line, returning a tuple of {Address, Value}.

parse_mem(Line) ->
  [Prefix, Value] = string:split(string:prefix(Line, "mem"), " = "),
  Addr = string:slice(Prefix, 1, length(Prefix)-2),
  {list_to_integer(Addr), list_to_integer(Value)}.

-ifdef(TEST).
parse_mem_test() ->
  ?assertEqual({  7, 101}, parse_mem("mem[7] = 101")),
  ?assertEqual({101,   7}, parse_mem("mem[101] = 7")).
-endif.


% Applies the given mask to a value, returning the masked value.

mask_value(Value, Mask) ->
  (Value band Mask#mask.m0) bor Mask#mask.m1.

-ifdef(TEST).
mask_value_test() ->
  Mask = parse_mask("XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"),
  ?assertEqual( 73, mask_value( 11, Mask)),
  ?assertEqual(101, mask_value(101, Mask)),
  ?assertEqual( 64, mask_value(  0, Mask)).
-endif.


% Applies the given mask to an address line, returning the masked address.

mask_addr(Val, Mask) ->
  mask_addr(Val, Mask, []).

mask_addr([], [], Acc) -> lists:reverse(Acc);
mask_addr([V1 | VR], [M1 | MR], Acc) ->
  if
    M1 == $0 -> mask_addr(VR, MR, [V1 | Acc]);
    true     -> mask_addr(VR, MR, [M1 | Acc])
  end.

-ifdef(TEST).
mask_addr_test() ->
  ?assertEqual("0X1101X", mask_addr("0101010", "0X1001X")),
  ?assertEqual("001X0XX", mask_addr("0011010", "000X0XX")).
-endif.


% Loads the input as a sequence of line.

load(Filename) ->
  {ok, File} = file:open(Filename, [read]),
  {ok, Text} = file:read(File, 1024*1024),
  string:split(Text, "\n", all).
