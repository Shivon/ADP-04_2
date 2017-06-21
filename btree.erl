-module(btree).
-export([init_btree/0, is_btree/1, insert_node/2, is_empty/1, equal/2]).


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


% init_btree: ∅ → btree
init_btree() -> {}.


% is_btree: btree → bool
% is_btree checks if syntax, sorting and height are correct
% requirement: when checking, the tree must only be traversed once top-down and once bottom-up back from recursion
is_btree({}) -> true;
is_btree(Btree) -> is_btree(Btree, true).


is_btree({{_Parent, Height}, {}, {}}, Boolean) -> Boolean and (Height == 1);

is_btree({{Parent, Height}, {}, RightChild}, Boolean) ->
  IsSortingCorrect = tree_helper:parent_element(RightChild) > Parent,
  IsHeightCorrect = (tree_helper:height(RightChild) + 1) == Height,
  IsTreeValid = IsSortingCorrect and IsHeightCorrect and Boolean,
  is_btree(RightChild, IsTreeValid);

is_btree({{Parent, Height}, LeftChild, {}}, Boolean) ->
  IsSortingCorrect = tree_helper:parent_element(LeftChild) < Parent,
  IsHeightCorrect = (tree_helper:height(LeftChild) + 1) == Height,
  IsTreeValid = IsSortingCorrect and IsHeightCorrect and Boolean,
  is_btree(LeftChild, IsTreeValid);

is_btree({{Parent, Height}, LeftChild, RightChild}, Boolean) ->
  IsSortingCorrect = (tree_helper:parent_element(LeftChild) < Parent) and (tree_helper:parent_element(RightChild) > Parent),
  IsHeightCorrect = (tree_helper:max_height(LeftChild, RightChild) + 1) == Height,
  IsTreeValid = IsSortingCorrect and IsHeightCorrect and Boolean,
  is_btree(LeftChild, IsTreeValid) and is_btree(RightChild, IsTreeValid);

is_btree(_Object, _Boolean) -> false.


% insert_node: btree × elem → btree
% elements in the partial left tree are smaller and in the partial right tree greater than parent
% elements == parent are ignored
insert_node({}, Element) -> tree_helper:init_leaf(Element);

insert_node({{Parent, _Height}, {}, RightChild}, Element) when Element < Parent ->
  NewLeftChild = tree_helper:init_leaf(Element),
  NewParentHeight = tree_helper:max_height(NewLeftChild, RightChild) + 1,
  {{Parent, NewParentHeight}, NewLeftChild, RightChild};

insert_node({{Parent, _Height}, LeftChild, {}}, Element) when Element > Parent ->
  NewRightChild = tree_helper:init_leaf(Element),
  NewParentHeight = tree_helper:max_height(LeftChild, NewRightChild) + 1,
  {{Parent, NewParentHeight}, LeftChild, NewRightChild};

insert_node({{Parent, Height}, LeftChild, RightChild}, Element) when Element == Parent ->
  {{Parent, Height}, LeftChild, RightChild};

insert_node({{Parent, _Height}, LeftChild, RightChild}, Element) ->
  if
    Element < Parent ->
      NewLeftChild = insert_node(LeftChild, Element),
      NewParentHeight = tree_helper:max_height(NewLeftChild, RightChild) + 1,
      {{Parent, NewParentHeight}, NewLeftChild, RightChild};
    Element > Parent ->
      NewRightChild = insert_node(RightChild, Element),
      NewParentHeight = tree_helper:max_height(LeftChild, NewRightChild) + 1,
      {{Parent, NewParentHeight}, LeftChild, NewRightChild}
  end.


% is_empty: btree → bool
is_empty(Btree) -> Btree == {}.


% equal: btree × btree → bool
equal({}, {}) -> true;
equal({}, _SecondBtree) -> false;
equal(_FirstBtree, {}) -> false;
equal({FirstParent, FirstLeftChild, FirstRightChild}, {SecondParent, SecondLeftChild, SecondRightChild}) ->
  {FirstParentElem, FirstParentHeight} = FirstParent,
  {SecondParentElem, SecondParentHeight} = SecondParent,
  if
    (FirstParentElem == SecondParentElem) and (FirstParentHeight == SecondParentHeight) ->
      equal(FirstLeftChild, SecondLeftChild) and equal(FirstRightChild, SecondRightChild);
    true -> false
  end.
