-module(splaytree).
-export([initBT/0, isEmptyBT/1, equalBT/2, isBT/1, insertBT/2, deleteBT/2, findSBT/2, findBT/2, findTP/2, printBT/2]).

% TODO: rework btree to match Klauck's requirements!

% ADT Splay­Tree: Vorgabe:
% Funktional (nach außen)
%   1. Definition wie in der Vorlesung vorgestellt;
%   2. Die Elemente sind vom Typ „ganze Zahl“.
%   3. Duplikate von Elementen sind nicht zulässig.
%   4. Alle für die ADT BTree bestehenden Funktionen müssen auch auf dem Splay-Tree laufen und umgekehrt.
%   5. Die einfachen Rotationsfunktionen und printBT sind aus Aufgabe drei zu verwenden.
%
% Technisch (nach innen)
%   1. Die ADT Splay-Tree ist mittels ADT BTree zu realisieren, indem diese erweitert wird.
%   2. Analog zu isBT sind bei deleteBT, findBT, findTP der Baum nur einmal von oben nach unten zu durchlaufen und
%     wegen der Rekursion dann einmal von unten nach oben. Lediglich beim Löschen darf einmal von der löschenden Position
%     zu einem Blatt und zurück zusätzlich gelaufen werden.
%   3. Die zugehörige Datei heißt splaytree.erl


% initBT: ∅ → btree
% initBT()
initBT() -> btree:init_btree().


% isEmptyBT: btree → bool
% isEmptyBT(<BTree>)
isEmptyBT(BTree) -> btree:is_empty(BTree).


% equalBT: btree × btree → bool
% equalBT(<BTree>,<BTree>)
equalBT(FirstBTree, SecondBTree) -> btree:equal(FirstBTree, SecondBTree).


% isBT: btree → bool
% isBT(<BTree>)
isBT(BTree) -> btree:is_btree(BTree).


% insertBT: btree × elem → btree
% insertBT(<BTree>,<Element>)
insertBT(BTree, Element) -> btree:insert_node(BTree, Element).


% deleteBT: btree × elem → btree
% deleteBT(<BTree>,<Element>)
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
deleteBT(Tree, _Element) ->
  Tree.


% findSBT implements normal search, returns only the height of the node which equals the element
% returns 0 when element not found (includes empty tree)
% findSBT: btree × elem → int
% findSBT(<BTree>,<Element>)
findSBT({}, _Element) -> 0;
findSBT({{Element, Height}, _LeftNode, _RightNode}, Element) -> Height;
findSBT({{Parent, _Height}, LeftNode, _RightNode}, Element) when Parent > Element -> findSBT(LeftNode, Element);
findSBT({{Parent, _Height}, _LeftNode, RightNode}, Element) when Parent < Element -> findSBT(RightNode, Element).


% findBT implements Move-To-Root strategy (after successful search, node is moved to root of the tree)
% returns height and modified tree
% findBT: btree × elem → {int,btree}
% findBT(<BTree>,<Element>)
findBT({}, _Element) -> {0, {}};

findBT({{Element, Height}, LeftNode, RightNode}, Element) ->
  {Height, {{Element, Height}, LeftNode, RightNode}};

findBT({ParentNode, {{Element, LeftHeight}, LeftLeftNode, LeftRightNode}, RightNode}, Element) ->
  NewTree = tree_helper:rotate_right({ParentNode, {{Element, LeftHeight}, LeftLeftNode, LeftRightNode}, RightNode}),
  {LeftHeight, NewTree};
  % this uses new height of element
  % NewHeight = tree_helper:height(NewTree),
  % {NewHeight, NewTree};

findBT({ParentNode, LeftNode, {{Element, RightHeight}, RightLeftNode, RightRightNode}}, Element) ->
  NewTree = tree_helper:rotate_left({ParentNode, LeftNode, {{Element, RightHeight}, RightLeftNode, RightRightNode}}),
  {RightHeight, NewTree};
  % this uses new height of element
  % NewHeight = tree_helper:height(NewTree),
  % {NewHeight, NewTree};

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
% returns height and modified tree
% findTP: btree × elem → {int,btree}
% findTP(<BTree>,<Element>)
findTP({}, _Element) -> {0, {}};

findTP({{Element, Height}, LeftNode, RightNode}, Element) ->
  {Height, {{Element, Height}, LeftNode, RightNode}};

findTP({ParentNode, {{Element, LeftHeight}, LeftLeftNode, LeftRightNode}, RightNode}, Element) ->
  NewTree = tree_helper:rotate_right({ParentNode, {{Element, LeftHeight}, LeftLeftNode, LeftRightNode}, RightNode}),
  {LeftHeight, NewTree};
  % this uses new height of element
  % NewHeight = tree_helper:height(NewTree),
  % {NewHeight, NewTree};

findTP({ParentNode, LeftNode, {{Element, RightHeight}, RightLeftNode, RightRightNode}}, Element) ->
  NewTree = tree_helper:rotate_left({ParentNode, LeftNode, {{Element, RightHeight}, RightLeftNode, RightRightNode}}),
  {RightHeight, NewTree};
  % this uses new height of element
  % NewHeight = tree_helper:height(NewTree),
  % {NewHeight, NewTree};

findTP({{Parent, _Height}, LeftNode, RightNode}, Element) when Parent > Element ->
  {ElementHeight, NewLeftNode} = findTP(LeftNode, Element),
  NewParentHeight = tree_helper:max_height(NewLeftNode, RightNode) + 1,
  {ElementHeight, {{Parent, NewParentHeight}, NewLeftNode, RightNode}};

findTP({{Parent, _Height}, LeftNode, RightNode}, Element) when Parent < Element ->
  {ElementHeight, NewRightNode} = findTP(RightNode, Element),
  NewParentHeight = tree_helper:max_height(LeftNode, NewRightNode) + 1,
  {ElementHeight, {{Parent, NewParentHeight}, LeftNode, NewRightNode}}.


% printBT: btree × filename → dot
% printBT(<BTree>,<Filename>)
printBT(BTree, Filename) ->
  file:write_file(Filename, io_lib:fwrite("digraph avltree {  \n", []), [write]),
  tree_helper:write_to_file(BTree, Filename),
  file:write_file(Filename, io_lib:fwrite("} \n", []), [append]).
