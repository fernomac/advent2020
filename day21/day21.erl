-module(day21).
-export([main/0]).
-include_lib("eunit/include/eunit.hrl").

main() ->
  Labels = load("input.txt"),
  IngredientCounts = count_ingredients(Labels),
  Ingredients = sets:from_list(maps:keys(IngredientCounts)),
  Allergens = collect_allergens(Labels),

  Safe = find_safe_ingredients(Labels, Ingredients, Allergens),
  Questionable = sets:subtract(Ingredients, Safe),
  Filtered = lists:map(fun ({Is, As}) -> {sets:subtract(Is, Safe), As} end, Labels),

  io:format("~p~n", [part_one(Safe, IngredientCounts)]),
  io:format("~s~n", [part_two(Filtered, Questionable, Allergens)]),
  ok.


% Part one: add up the number of appearances of ingredients which we know cannot
% contain any allergens.

part_one(Safe, IngredientCounts) ->
  sets:fold(fun (I, Acc) ->
    Acc + maps:get(I, IngredientCounts)
  end, 0, Safe).


% Part two: return a string composed of the allergen-containing ingredients, sorted
% by the name of the allergen they contain.

part_two(Labels, Ingredients, Allergens) ->
  Candidates = lists:map(fun (Allergen) ->
    Safe = find_safe_ingredients_for(Labels, Ingredients, Allergen),
    {Allergen, sets:subtract(Ingredients, Safe)}
  end, sets:to_list(Allergens)),

  Matches = match_allergens(Candidates, #{}),

  Sorted = lists:sort(fun ({K1, _}, {K2, _}) -> K1 =< K2 end, Matches),

  lists:flatten(lists:join(",", lists:map(fun ({_, V}) -> V end, Sorted))).


% Count the number of times each ingredient appears on a label.

count_ingredients(Labels) ->
  lists:foldl(fun ({Ingredients, _}, Acc) ->
    count_ingredients(Ingredients, Acc)
  end, #{}, Labels).

count_ingredients(Set, Map) ->
  sets:fold(fun (Ingredient, Acc) ->
    Acc#{ Ingredient => maps:get(Ingredient, Acc, 0) + 1 }
  end, Map, Set).

% Collect the set of all allergens.

collect_allergens(Labels) ->
  lists:foldl(fun ({_, Allergens}, Acc) ->
    sets:union(Acc, Allergens)
  end, sets:new(), Labels).


% Finds the set of ingredients that cannot contain ANY allergen.

find_safe_ingredients(Labels, Ingredients, Allergens) ->
  sets:intersection(lists:map(fun (Allergen) ->
    find_safe_ingredients_for(Labels, Ingredients, Allergen)
  end, sets:to_list(Allergens))).



% Match allergens to ingredients by looking for allergens with only a single
% candidate ingredient. That ingredient contributes the given allergen, and
% can be eliminated as a candidate for any OTHER allergen. We iterate until
% all allergens are matched and return a map (in list form) from allergen to
% ingredient.

match_allergens([], Acc)         -> maps:to_list(Acc);
match_allergens(Candidates, Acc) ->
  Matches = maps:from_list(lists:filtermap(fun ({K, V}) ->
    case sets:size(V) of
      1 -> {true, {K, hd(sets:to_list(V))}};
      _ -> false
    end
  end, Candidates)),

  Matched = sets:from_list(maps:values(Matches)),

  RemainingCandidates = lists:filtermap(fun ({K, V}) ->
    case maps:is_key(K, Matches) of
      true  -> false;
      false -> {true, {K, sets:subtract(V, Matched)}}
    end
  end, Candidates),

  match_allergens(RemainingCandidates, maps:merge(Acc, Matches)).


% Finds the set of ingredients that cannot contain a particular allergen
% because there is at least one label that contains the allergen but does
% NOT contain the ingredient.

find_safe_ingredients_for(Labels, Ingredients, Allergen) ->
  Dangerous = labels_with_allergen(Labels, Allergen),
  lists:foldl(fun ({Suspects, _}, Acc) ->
    sets:union(Acc, sets:subtract(Ingredients, Suspects))
  end, sets:new(), Dangerous).

labels_with_allergen(Labels, Allergen) ->
  lists:filter(fun ({_, As}) ->
    sets:is_element(Allergen, As)
  end, Labels).



-ifdef(TEST).
test_data() -> parse([
    "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)",
    "trh fvjkl sbzzf mxmxvkd (contains dairy)",
    "sqjhc fvjkl (contains soy)",
    "sqjhc mxmxvkd sbzzf (contains fish)"
  ]).

find_safe_ingredients_test() ->
  Labels = test_data(),
  IngredientCounts = count_ingredients(Labels),
  Ingredients = sets:from_list(maps:keys(IngredientCounts)),
  Allergens = collect_allergens(Labels),

  Safe = find_safe_ingredients(Labels, Ingredients, Allergens),
  ?assertEqual(4, sets:size(Safe)),
  lists:foreach(fun (E) ->
    ?assertEqual(true, sets:is_element(E, Safe))
  end, ["kfcds", "nhms", "sbzzf", "trh"]).
-endif.



% Loads the given input file.

load(Filename) ->
  {ok, File} = file:open(Filename, [read]),
  {ok, Text} = file:read(File, 1024*1024),
  parse(string:split(Text, "\n", all)).

parse(Lines) ->
  lists:map(fun (Line) -> 
    [First, Second] = string:split(Line, " (contains "),
    {parse_ingredients(First), parse_allergens(Second)}
  end, Lines).
  
parse_ingredients(Line) ->
  sets:from_list(string:split(Line, " ", all)).

parse_allergens(Line) ->
  sets:from_list(string:split(string:trim(Line, trailing, ")"), ", ", all)).
