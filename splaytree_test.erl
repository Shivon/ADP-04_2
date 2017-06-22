-module(splaytree_test).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).


initBT_test() -> {} = splaytree:initBT().


isEmptyBT_empty_test() -> true = splaytree:isEmptyBT({}).
isEmptyBT_not_empty_test() ->
  NotEmptyTree = {{7,2},{},{{10,1},{},{}}},
  false = splaytree:isEmptyBT(NotEmptyTree).


equalBT_equal_test() ->
  FirstTree = {{6,4}, {{3,1},{},{}}, {{10,3}, {{9,1},{},{}}, {{15,2},{{11,1},{},{}},{{33,1},{},{}}}}},
  SecondTree = {{6,4}, {{3,1},{},{}}, {{10,3}, {{9,1},{},{}}, {{15,2},{{11,1},{},{}},{{33,1},{},{}}}}},
  true = splaytree:equalBT(FirstTree, SecondTree).
equalBT_not_equal_test() ->
  FirstTree = {{6,4}, {{3,1},{},{}}, {{10,3}, {{9,1},{},{}}, {{15,2},{{11,1},{},{}},{{33,1},{},{}}}}},
  SecondTree = {{6,4}, {{3,1},{},{}}, {{10,3}, {{9,1},{},{}}, {{15,2},{{11,1},{},{}},{{34,1},{},{}}}}},
  false = splaytree:equalBT(FirstTree, SecondTree).


isBT_valid_tree_test() ->
  ValidTree = {{6,4}, {{3,1},{},{}}, {{10,3}, {{9,1},{},{}}, {{15,2},{{11,1},{},{}},{{33,1},{},{}}}}},
  true = splaytree:isBT(ValidTree).
isBT_invalid_sorted_tree_test() ->
  InvalidTree = {{6,4}, {{3,1},{},{}}, {{10,3}, {{9,1},{},{}}, {{5,2},{{11,1},{},{}},{{34,1},{},{}}}}},
  false = splaytree:isBT(InvalidTree).
isBT_invalid_height_tree_test() ->
  InvalidTree = {{6,5}, {{3,1},{},{}}, {{10,3}, {{9,1},{},{}}, {{15,2},{{11,1},{}},{{34,1},{},{}}}}},
  false = splaytree:isBT(InvalidTree).
isBT_invalid_format_tree_test() ->
  InvalidTree = {{6,4}, {{3,1},{},{}}, {{10,3}, {{9,1},{},{}}, {{15,2},{{11,1},{},7},{{34,1},{},{}}}}},
  false = splaytree:isBT(InvalidTree).


% insertBT
% deleteBT
% findSBT/2


findBT_successful_search_test() ->
  Tree = {{6,4}, {{3,1},{},{}}, {{10,3}, {{9,1},{},{}}, {{15,2},{{11,1},{},{}},{{33,1},{},{}}}}},
  ExpectedModifiedTree = {{33, 5}, {{6, 4}, {{3, 1}, {}, {}}, {{10, 3}, {{9, 1}, {}, {}}, {{15, 2}, {{11, 1}, {}, {}}, {}}}}, {}},
  {1, ExpectedModifiedTree} = splaytree:findBT(Tree, 33),
  5 = splaytree:findSBT(ExpectedModifiedTree, 33).

findBT_unsuccessful_search_small_number_test() ->
  Tree = {{6,4}, {{3,1},{},{}}, {{10,3}, {{9,1},{},{}}, {{15,2},{{11,1},{},{}},{{33,1},{},{}}}}},
  ModifiedTree = {{3,5}, {}, {{6,4}, {}, {{10,3}, {{9,1},{},{}}, {{15,2}, {{11,1},{},{}}, {{33,1},{},{}}}}}},
  {0, ModifiedTree} = splaytree:findBT(Tree, 1),
  0 = splaytree:findSBT(ModifiedTree, 1).

findBT_unsuccessful_search_big_number_test() ->
  Tree = {{6,4}, {{3,1},{},{}}, {{10,3}, {{9,1},{},{}}, {{15,2},{{11,1},{},{}},{{33,1},{},{}}}}},
  ModifiedTree = {{33,5}, {{6,4}, {{3,1},{},{}}, {{10,3}, {{9,1},{},{}}, {{15,2}, {{11,1},{},{}}, {}}}}, {}},
  {0, ModifiedTree} = splaytree:findBT(Tree, 999),
  0 = splaytree:findSBT(ModifiedTree, 999).


% findTP/2
