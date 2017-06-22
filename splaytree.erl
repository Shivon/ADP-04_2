-module(splaytree).
-export([initBT/0, isEmptyBT/1, equalBT/2, isBT/1, insertBT/2, deleteBT/2, findSBT/2, findBT/2, findTP/2, printBT/2]).


% initBT: ∅ → btree
initBT() -> btree:initBT().


% isEmptyBT: btree → bool
isEmptyBT(BTree) -> btree:isEmptyBT(BTree).


% equalBT: btree × btree → bool
equalBT(FirstBTree, SecondBTree) -> btree:equalBT(FirstBTree, SecondBTree).


% isBT: btree → bool
isBT(BTree) -> btree:isBT(BTree).


% insertBT: btree × elem → btree
insertBT(BTree, Element) -> btree:insertBT(BTree, Element).


% deleteBT: btree × elem → btree
deleteBT({{Element, 1}, {}, {}}, Element) -> {};

deleteBT({{Element, _}, LeftNode, RightNode}, Element) ->
  % using the height function instead of pattern matching secures
  % that this works even when one or both of the child nodes are empty
  LeftHeight = tree_helper:height(LeftNode),
  RightHeight = tree_helper:height(RightNode),
  if
    % getting the new parent from the partial tree with more levels secures that it works even when one partial tree is empty
    LeftHeight > RightHeight ->
      NewParent = tree_helper:maximum_node(LeftNode),
      NewLeftNode = deleteBT(LeftNode, NewParent),
      {{NewParent, tree_helper:max_height(NewLeftNode, RightNode) + 1}, NewLeftNode, RightNode};
    true ->
      NewParent = tree_helper:minimum_node(RightNode),
      NewRightNode = deleteBT(RightNode, NewParent),
      {{NewParent, tree_helper:max_height(LeftNode, NewRightNode) + 1}, LeftNode, NewRightNode}
  end;

deleteBT({{Parent, _}, LeftNode, RightNode}, Element) when Element < Parent ->
  NewLeftNode = deleteBT(LeftNode, Element),
  {{Parent, tree_helper:max_height(NewLeftNode, RightNode) + 1}, NewLeftNode, RightNode};

deleteBT({{Parent, _}, LeftNode, RightNode}, Element) when Element > Parent ->
  NewRightNode = deleteBT(RightNode, Element),
  {{Parent, tree_helper:max_height(LeftNode, NewRightNode) + 1}, LeftNode, NewRightNode};

% Element is not in tree, tree gets returned
deleteBT(Tree, _Element) -> Tree.


% findSBT implements normal search, returns only the height of the node which equals the element
% returns 0 when element not found (includes empty tree)
% findSBT: btree × elem → int
findSBT({}, _Element) -> 0;
findSBT({{Element, Height}, _LeftNode, _RightNode}, Element) -> Height;
findSBT({{Parent, _Height}, LeftNode, _RightNode}, Element) when Parent > Element -> findSBT(LeftNode, Element);
findSBT({{Parent, _Height}, _LeftNode, RightNode}, Element) when Parent < Element -> findSBT(RightNode, Element).


% findBT implements Move-To-Root strategy (after successful search, node is moved to root of the tree)
% returns old height and modified tree
% findBT: btree × elem → {int,btree}
findBT({}, _Element) -> {0, {}};

findBT({{Element, Height}, LeftNode, RightNode}, Element) ->
  {Height, {{Element, Height}, LeftNode, RightNode}};

findBT({ParentNode, {{Element, LeftHeight}, LeftLeftNode, LeftRightNode}, RightNode}, Element) ->
  NewTree = tree_helper:rotate_right({ParentNode, {{Element, LeftHeight}, LeftLeftNode, LeftRightNode}, RightNode}),
  {LeftHeight, NewTree};

findBT({ParentNode, LeftNode, {{Element, RightHeight}, RightLeftNode, RightRightNode}}, Element) ->
  NewTree = tree_helper:rotate_left({ParentNode, LeftNode, {{Element, RightHeight}, RightLeftNode, RightRightNode}}),
  {RightHeight, NewTree};

findBT({{Parent, _Height}, LeftNode, RightNode}, Element) when Parent > Element ->
  {ElementHeight, NewLeftNode} = findBT(LeftNode, Element),
  NewParentHeight = tree_helper:max_height(NewLeftNode, RightNode) + 1,
  AccuTree = {{Parent, NewParentHeight}, NewLeftNode, RightNode},
  {ElementHeight, tree_helper:rotate_right(AccuTree)};

findBT({{Parent, _Height}, LeftNode, RightNode}, Element) when Parent < Element ->
  {ElementHeight, NewRightNode} = findBT(RightNode, Element),
  NewParentHeight = tree_helper:max_height(LeftNode, NewRightNode) + 1,
  AccuTree = {{Parent, NewParentHeight}, LeftNode, NewRightNode},
  {ElementHeight, tree_helper:rotate_left(AccuTree)}.


% findTP implements the transpose strategy
% (after successful search, element is swapped with its immediate predecessor)
% returns old height and modified tree
% findTP: btree × elem → {int,btree}
findTP({}, _Element) -> {0, {}};

findTP({{Element, Height}, LeftNode, RightNode}, Element) ->
  {Height, {{Element, Height}, LeftNode, RightNode}};

findTP({ParentNode, {{Element, LeftHeight}, LeftLeftNode, LeftRightNode}, RightNode}, Element) ->
  NewTree = tree_helper:rotate_right({ParentNode, {{Element, LeftHeight}, LeftLeftNode, LeftRightNode}, RightNode}),
  {LeftHeight, NewTree};

findTP({ParentNode, LeftNode, {{Element, RightHeight}, RightLeftNode, RightRightNode}}, Element) ->
  NewTree = tree_helper:rotate_left({ParentNode, LeftNode, {{Element, RightHeight}, RightLeftNode, RightRightNode}}),
  {RightHeight, NewTree};

findTP({{Parent, Height}, LeftNode, RightNode}, Element) when Parent > Element ->
  {ElementHeight, NewLeftNode} = findTP(LeftNode, Element),
  LeftNodeUnchanged = equalBT(LeftNode, NewLeftNode),
  if
    LeftNodeUnchanged ->
      NewTree = tree_helper:rotate_right({{Parent, Height}, LeftNode, RightNode}),
      {ElementHeight, NewTree};
    true ->
      NewParentHeight = tree_helper:max_height(NewLeftNode, RightNode) + 1,
      {ElementHeight, {{Parent, NewParentHeight}, NewLeftNode, RightNode}}
  end;

findTP({{Parent, Height}, LeftNode, RightNode}, Element) when Parent < Element ->
  {ElementHeight, NewRightNode} = findTP(RightNode, Element),
  RightNodeUnchanged = equalBT(RightNode, NewRightNode),
  if
    RightNodeUnchanged ->
      NewTree = tree_helper:rotate_left({{Parent, Height}, LeftNode, RightNode}),
      {ElementHeight, NewTree};
    true ->
      NewParentHeight = tree_helper:max_height(LeftNode, NewRightNode) + 1,
      {ElementHeight, {{Parent, NewParentHeight}, LeftNode, NewRightNode}}
  end.


% printBT: btree × filename → dot
printBT(BTree, Filename) ->
  file:write_file(Filename, io_lib:fwrite("digraph avltree {  \n", []), [write]),
  tree_helper:write_to_file(BTree, Filename),
  file:write_file(Filename, io_lib:fwrite("} \n", []), [append]).
