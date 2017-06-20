-module(splaytree).
-compile(export_all).
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


% isEmptyBT: btree → bool
% isEmptyBT(<BTree>)


% equalBT: btree × btree → bool
% equalBT(<BTree>,<BTree>)


% isBT: btree → bool
% isBT(<BTree>)


% insertBT: btree × elem → btree
% insertBT(<BTree>,<Element>)


% deleteBT: btree × elem → btree
% deleteBT(<BTree>,<Element>)


% findSBT implements normal search, returns only the height of the node which equals the element
% returns nil when element not found
% findSBT: btree × elem → int
% findSBT(<BTree>,<Element>)


% findBT implements Move-To-Root strategy (after successful search, node is moved to root of the tree)
% returns height and modified tree
% findBT: btree × elem → {int,btree}
% findBT(<BTree>,<Element>)


% findTP implements the transpose strategy
% (after successful search, element is swapped with its immediate predecessor)
% returns height and modified tree
% findTP: btree × elem → {int,btree}
% findTP(<BTree>,<Element>)


% printBT: btree × filename → dot
% printBT(<BTree>,<Filename>)
