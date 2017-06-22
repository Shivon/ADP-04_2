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



findTP_successful_search_test() ->
  Tree = {{6,4}, {{3,1},{},{}}, {{10,3}, {{9,1},{},{}}, {{15,2},{{11,1},{},{}},{{33,1},{},{}}}}},
  ExpectedModifiedTree = {{6,5}, {{3,1},{},{}}, {{10,4}, {{9,1},{},{}}, {{33,3}, {{15,2}, {{11,1},{},{}},{}},{}}}},
  {1, ExpectedModifiedTree} = splaytree:findTP(Tree, 33),
  3 = splaytree:findSBT(ExpectedModifiedTree, 33).

findTP_unsuccessful_search_small_number_test() ->
  Tree = {{6,4}, {{3,1},{},{}}, {{10,3}, {{9,1},{},{}}, {{15,2},{{11,1},{},{}},{{33,1},{},{}}}}},
  ModifiedTree = {{3,5}, {}, {{6,4},{}, {{10,3}, {{9,1},{},{}}, {{15,2}, {{11,1},{},{}}, {{33,1},{},{}}}}}},
  {0, ModifiedTree} = splaytree:findTP(Tree, 1),
  0 = splaytree:findSBT(ModifiedTree, 1).

findTP_unsuccessful_search_big_number_test() ->
  Tree = {{6,4}, {{3,1},{},{}}, {{10,3}, {{9,1},{},{}}, {{15,2},{{11,1},{},{}},{{33,1},{},{}}}}},
  ModifiedTree = {{6,5}, {{3,1},{},{}}, {{10,4}, {{9,1},{},{}}, {{33,3}, {{15,2}, {{11,1},{},{}},{}},{}}}},
  {0, ModifiedTree} = splaytree:findTP(Tree, 999),
  0 = splaytree:findSBT(ModifiedTree, 999).

findTP_unsuccessful_search_median_number_test() ->
  Tree = {{6,4}, {{3,1},{},{}}, {{10,3}, {{9,1},{},{}}, {{15,2},{{11,1},{},{}},{{33,1},{},{}}}}},
  ModifiedTree = {{6,5}, {{3,1},{},{}}, {{10,4}, {{9,1},{},{}}, {{11,3},{}, {{15,2},{}, {{33,1},{},{}}}}}},
  {0, ModifiedTree} = splaytree:findTP(Tree, 14),
  0 = splaytree:findSBT(ModifiedTree, 14).


big_list_runtime_test() ->
  BigList = util:randomliste(1000),
  BigTree = insert_nodes({}, BigList),
  SearchListWithDuplicates = util:randomlisteD(1000, 1, 1000),

  StartTP = util:timeMilliSecond(),
  {_OldHeightElementTP, FinalTreeTP} = find_all_TP(BigTree, SearchListWithDuplicates),
  EndTP = util:timeMilliSecond(),
  splaytree:printBT(FinalTreeTP, "runtime_test_TP.dot"),

  StartBT = util:timeMilliSecond(),
  {_OldHeightElementBT, FinalTreeBT} = find_all_BT(BigTree, SearchListWithDuplicates),
  EndBT = util:timeMilliSecond(),
  splaytree:printBT(FinalTreeBT, "runtime_test_BT.dot"),

  file:write_file("runtime_logging_big_list.log", io_lib:fwrite("FindTP | start: ~p, end: ~p \n", [StartTP, EndTP]), [write]),
  file:write_file("runtime_logging_big_list.log", io_lib:fwrite("FindBT | start: ~p, end: ~p \n", [StartBT, EndBT]), [append]).


same_search_runtime_test() ->
  BigList = util:randomliste(1000),
  BigTree = insert_nodes({}, BigList),
  SearchListWithDuplicates = [378, 378],

  StartTP = util:timeMilliSecond(),
  {_OldHeightElementTP, FinalTreeTP} = find_all_TP(BigTree, SearchListWithDuplicates),
  EndTP = util:timeMilliSecond(),
  splaytree:printBT(FinalTreeTP, "duplicate_test_TP.dot"),

  StartBT = util:timeMilliSecond(),
  {_OldHeightElementBT, FinalTreeBT} = find_all_BT(BigTree, SearchListWithDuplicates),
  EndBT = util:timeMilliSecond(),
  splaytree:printBT(FinalTreeBT, "duplicate_test_BT.dot"),

  file:write_file("runtime_logging_duplicates.log", io_lib:fwrite("FindTP | start: ~p, end: ~p \n", [StartTP, EndTP]), [write]),
  file:write_file("runtime_logging_duplicates.log", io_lib:fwrite("FindBT | start: ~p, end: ~p \n", [StartBT, EndBT]), [append]).


% Helper
insert_nodes(BTree, [Element | []]) -> splaytree:insertBT(BTree, Element);
insert_nodes(BTree, [Element | Tail]) ->
  NewTree = splaytree:insertBT(BTree, Element),
  insert_nodes(NewTree, Tail).


find_all_TP(BTree, [Element | []]) -> splaytree:findTP(BTree, Element);
find_all_TP(BTree, [Element | Tail]) ->
  {_OldHeightElement, NewTree} = splaytree:findTP(BTree, Element),
  find_all_TP(NewTree, Tail).


find_all_BT(BTree, [Element | []]) -> splaytree:findBT(BTree, Element);
find_all_BT(BTree, [Element | Tail]) ->
  {_OldHeightElement, NewTree} = splaytree:findBT(BTree, Element),
  find_all_BT(NewTree, Tail).
