-module(btree).
-export([initBT/0, isBT/1, insertBT/2, isEmptyBT/1, equalBT/2]).


% Pattern {{Parent, Height}, {LeftChild, Height}, {RightChild, Height}}
% Height of the tree: Empty tree = 0, only root = 1 and so on
% Attention: Height != Level! Height at each node is the height of the partial tree from this node on,
% so the height gets smaller towards the leafs

% valid trees:
% T1 = {{6,4}, {{3,1},{},{}}, {{10,3}, {{9,1},{},{}}, {{15,2},{{11,1},{},{}},{{34,1},{},{}}}}}.
% T2 = {}.
% T3 = {{7,1},{},{}}.
% T4 = {{7,2},{},{{10,1},{},{}}}.
% T5 = {{7,3},{},{{10,2},{},{{15,1},{},{}}}}.
% T6 = {{7,3},{{5,1},{},{}},{{10,2},{},{{15,1},{},{}}}}.
% T7 = {{6,4}, {{3,1},{},{}}, {{10,3}, {{9,1},{},{}}, {{15,2},{{11,1},{},{}},{{33,1},{},{}}}}}.
% invalid trees:
% TT1 = {{6,4}, {{3,1},{},{}}, {{10,3}, {{9,1},{},{}}, {{15,2},{{11,1},{},7},{{34,1},{},{}}}}}.
% TT2 = {{6,4}, {{3,1},{},{}}, {{10,3}, {{9,1},{},{}}, {{15,2},{{11,1},{}},{{34,1},{},{}}}}}.
% TT3 = {{6,5}, {{3,1},{},{}}, {{10,3}, {{9,1},{},{}}, {{15,2},{{11,1},{}},{{34,1},{},{}}}}}.
% TT4 = {{6,5}, {{3,1},{},{}}, {{10,3}, {{9,1},{},{}}, {{15,2},{{11,1},{},{}},{{34,1},{},{}}}}}.
% TT5 = {{6,4}, {{3,1},{},{}}, {{10,3}, {{9,1},{},{}}, {{15,2},{{20,1},{},{}},{{34,1},{},{}}}}}.
% TT6 = {{6,4}, {{3,1},{},{}}, {{10,3}, {{9,1},{},{}}, {{5,2},{{11,1},{},{}},{{34,1},{},{}}}}}.


% initBT: ∅ → btree
initBT() -> {}.


% isBT: btree → bool
% isBT checks if syntax, sorting and height are correct
% requirement: when checking, the tree must only be traversed once top-down and once bottom-up back from recursion
isBT({}) -> true;
isBT(Btree) -> isBT(Btree, true).


isBT({{_Parent, Height}, {}, {}}, Boolean) -> Boolean and (Height == 1);

isBT({{Parent, Height}, {}, RightChild}, Boolean) ->
  IsSortingCorrect = tree_helper:parent_element(RightChild) > Parent,
  IsHeightCorrect = (tree_helper:height(RightChild) + 1) == Height,
  IsTreeValid = IsSortingCorrect and IsHeightCorrect and Boolean,
  isBT(RightChild, IsTreeValid);

isBT({{Parent, Height}, LeftChild, {}}, Boolean) ->
  IsSortingCorrect = tree_helper:parent_element(LeftChild) < Parent,
  IsHeightCorrect = (tree_helper:height(LeftChild) + 1) == Height,
  IsTreeValid = IsSortingCorrect and IsHeightCorrect and Boolean,
  isBT(LeftChild, IsTreeValid);

isBT({{Parent, Height}, LeftChild, RightChild}, Boolean) ->
  IsSortingCorrect = (tree_helper:parent_element(LeftChild) < Parent) and (tree_helper:parent_element(RightChild) > Parent),
  IsHeightCorrect = (tree_helper:max_height(LeftChild, RightChild) + 1) == Height,
  IsTreeValid = IsSortingCorrect and IsHeightCorrect and Boolean,
  isBT(LeftChild, IsTreeValid) and isBT(RightChild, IsTreeValid);

isBT(_Object, _Boolean) -> false.


% insertBT: btree × elem → btree
% elements in the partial left tree are smaller and in the partial right tree greater than parent
% elements == parent are ignored
insertBT({}, Element) -> tree_helper:init_leaf(Element);

insertBT({{Parent, _Height}, {}, RightChild}, Element) when Element < Parent ->
  NewLeftChild = tree_helper:init_leaf(Element),
  NewParentHeight = tree_helper:max_height(NewLeftChild, RightChild) + 1,
  {{Parent, NewParentHeight}, NewLeftChild, RightChild};

insertBT({{Parent, _Height}, LeftChild, {}}, Element) when Element > Parent ->
  NewRightChild = tree_helper:init_leaf(Element),
  NewParentHeight = tree_helper:max_height(LeftChild, NewRightChild) + 1,
  {{Parent, NewParentHeight}, LeftChild, NewRightChild};

insertBT({{Parent, Height}, LeftChild, RightChild}, Element) when Element == Parent ->
  {{Parent, Height}, LeftChild, RightChild};

insertBT({{Parent, _Height}, LeftChild, RightChild}, Element) ->
  if
    Element < Parent ->
      NewLeftChild = insertBT(LeftChild, Element),
      NewParentHeight = tree_helper:max_height(NewLeftChild, RightChild) + 1,
      {{Parent, NewParentHeight}, NewLeftChild, RightChild};
    Element > Parent ->
      NewRightChild = insertBT(RightChild, Element),
      NewParentHeight = tree_helper:max_height(LeftChild, NewRightChild) + 1,
      {{Parent, NewParentHeight}, LeftChild, NewRightChild}
  end.


% isEmptyBT: btree → bool
isEmptyBT(Btree) -> Btree == {}.


% equalBT: btree × btree → bool
equalBT({}, {}) -> true;
equalBT({}, _SecondBtree) -> false;
equalBT(_FirstBtree, {}) -> false;
equalBT({FirstParent, FirstLeftChild, FirstRightChild}, {SecondParent, SecondLeftChild, SecondRightChild}) ->
  {FirstParentElem, FirstParentHeight} = FirstParent,
  {SecondParentElem, SecondParentHeight} = SecondParent,
  if
    (FirstParentElem == SecondParentElem) and (FirstParentHeight == SecondParentHeight) ->
      equalBT(FirstLeftChild, SecondLeftChild) and equalBT(FirstRightChild, SecondRightChild);
    true -> false
  end.
