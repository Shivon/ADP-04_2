-module(splaytree).
-compile(export_all).

% TODO: rework btree to match Klauck's requirements!
% -export([initBT/0, isEmptyBT/1, equalBT/2, isBT/1, insertBT/2, deleteBT/2, findSBT/2, findBT/2, findTP/2, printBT/2]).

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
  NewTree = rotateRight({ParentNode, {{Element, LeftHeight}, LeftLeftNode, LeftRightNode}, RightNode}),
  {LeftHeight, NewTree};
  % this uses new height of element
  % NewHeight = btree:height(NewTree),
  % {NewHeight, NewTree};

findBT({ParentNode, LeftNode, {{Element, RightHeight}, RightLeftNode, RightRightNode}}, Element) ->
  NewTree = rotateLeft({ParentNode, LeftNode, {{Element, RightHeight}, RightLeftNode, RightRightNode}}),
  {RightHeight, NewTree};
  % this uses new height of element
  % NewHeight = btree:height(NewTree),
  % {NewHeight, NewTree};

findBT({{Parent, _Height}, LeftNode, RightNode}, Element) when Parent > Element ->
  {ElementHeight, NewLeftNode} = findBT(LeftNode, Element),
  NewParentHeight = btree:max_height(NewLeftNode, RightNode) + 1,
  AccuTree = {{Parent, NewParentHeight}, NewLeftNode, RightNode},
  {ElementHeight, rotateRight(AccuTree)};

findBT({{Parent, _Height}, LeftNode, RightNode}, Element) when Parent < Element ->
  {ElementHeight, NewRightNode} = findBT(RightNode, Element),
  NewParentHeight = btree:max_height(LeftNode, NewRightNode) + 1,
  AccuTree = {{Parent, NewParentHeight}, LeftNode, NewRightNode},
  {ElementHeight, rotateLeft(AccuTree)}.


% findTP implements the transpose strategy
% (after successful search, element is swapped with its immediate predecessor)
% returns height and modified tree
% findTP: btree × elem → {int,btree}
% findTP(<BTree>,<Element>)
findTP({}, _Element) -> {0, {}};

findTP({{Element, Height}, LeftNode, RightNode}, Element) ->
  {Height, {{Element, Height}, LeftNode, RightNode}};

findTP({ParentNode, {{Element, LeftHeight}, LeftLeftNode, LeftRightNode}, RightNode}, Element) ->
  NewTree = rotateRight({ParentNode, {{Element, LeftHeight}, LeftLeftNode, LeftRightNode}, RightNode}),
  {LeftHeight, NewTree};
  % this uses new height of element
  % NewHeight = btree:height(NewTree),
  % {NewHeight, NewTree};

findTP({ParentNode, LeftNode, {{Element, RightHeight}, RightLeftNode, RightRightNode}}, Element) ->
  NewTree = rotateLeft({ParentNode, LeftNode, {{Element, RightHeight}, RightLeftNode, RightRightNode}}),
  {RightHeight, NewTree};
  % this uses new height of element
  % NewHeight = btree:height(NewTree),
  % {NewHeight, NewTree};

findTP({{Parent, _Height}, LeftNode, RightNode}, Element) when Parent > Element ->
  {ElementHeight, NewLeftNode} = findTP(LeftNode, Element),
  NewParentHeight = btree:max_height(NewLeftNode, RightNode) + 1,
  {ElementHeight, {{Parent, NewParentHeight}, NewLeftNode, RightNode}};

findTP({{Parent, _Height}, LeftNode, RightNode}, Element) when Parent < Element ->
  {ElementHeight, NewRightNode} = findTP(RightNode, Element),
  NewParentHeight = btree:max_height(LeftNode, NewRightNode) + 1,
  {ElementHeight, {{Parent, NewParentHeight}, LeftNode, NewRightNode}}.


% printBT: btree × filename → dot
% printBT(<BTree>,<Filename>)


% Helper
% rotate left = zack rotation
rotateLeft({{P1, _}, CL1, {{P2, _}, CL2, CR2}}) ->
  %% Update height P1 and P2 (all others remain the same)
  HeightP1 = btree:max_height(CL1, CL2) + 1,
  HeightP2 = erlang:max(btree:height(CR2), HeightP1) + 1,
  {{P2, HeightP2}, {{P1, HeightP1}, CL1, CL2}, CR2}.

% rotate right = zick rotation
rotateRight({{P1, _}, {{P2, _}, CL2, CR2}, CR1}) ->
  %% Update height P1 and P2 (all others remain the same)
  HeightP1 = btree:max_height(CR1, CR2) + 1,
  HeightP2 = erlang:max(btree:height(CL2), HeightP1) + 1,
  {{P2, HeightP2}, CL2, {{P1, HeightP1}, CR2, CR1}}.
