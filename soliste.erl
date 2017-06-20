-module(soliste).
-export([create/0, isEmpty/1, isList/1, equal/2, laenge/1, insert/2, delete/2, finds/2, findmf/2, findtp/2, retrieve/2]).

% Vorgabe:
% Funktional (nach außen)
%   1. Die Liste beginnt bei Position 1.
%   2. Die Liste arbeitet nicht destruktiv.
%   3. Eingefügt wird immer an der ersten Position.
%   4. Beim löschen wird das erste Vorkommen in der Liste gelöscht.
%   5. equal testet auf strukturelle Gleichheit
% Technisch (nach innen)
%   1. Die Liste ist intern mittels dem Erlang Liste [ ] zu realisieren.
%   2. Die zugehörige Datei heißt soliste.erl

% L1 = [].
% L2 = [1,2].
% L3 = [1,2,3,4].
% L4 = [2,3,4,1].
% L5 = [9 | L3].
% L6 = [9 | L4].
% L7 = [9, L3].
% L8 = [9, L4].
% L9 = [9, [1, 2, 3]].
% L10 = [9, [1, [2, [3]]]].
% L11 = [9, [1, [2, [3, []]]]].
% L12 = [1,2,3,4,4,3,2,1,9].


% create: ∅ → list
% create()
create() -> [].


% isEmpty: list → bool
% isEmpty(<Liste>)
isEmpty(List) -> List == [].


% isList: list → bool
% isList(<Liste>)
% assumption: only the top level needs to be a list, the contained objects can be of any kind
isList([]) -> true;
% isList([_Head | Tail]) -> isList(Tail);
isList([_Head | _Tail]) -> true;
isList(_Non_List) -> false.


% equal: list × list → bool
% equal(<Liste>,<Liste>)
equal(FirstList, SecondList) -> FirstList == SecondList.


% laenge: list → int
% laenge(<Liste>)
laenge(List) -> length(List).


% insert: list × elem → list
% insert(<Liste>,<Element>)
insert([], Element) -> [Element];
insert([Head | Tail], Element) -> [Element, Head | Tail].
% for more elegance but less rubustness:
% insert(List, Element) -> [Element | List].


% delete: list × elem → list
% delete(<Liste>,<Element>)
delete([], _Element) -> [];
delete([Head | Tail], Element) when Head == Element -> Tail;
delete([Head | Tail], Element) -> [Head | delete(Tail, Element)].


% finds implements normal search, returns only the position of the element's first occurrence in the list
% returns nil when element not found
% finds: list × elem → pos
% finds(<Liste>,<Element>)
finds(List, Element) -> finds(List, Element, 1).

finds([], _Element, _AccuPosition) -> nil;
finds([Head | _Tail], Element, AccuPosition) when Head == Element -> AccuPosition;
finds([_Head | Tail], Element, AccuPosition) -> finds(Tail, Element, AccuPosition + 1).


% findmf implements Move-To-Front strategy (after successful search, element is moved to front of the list)
% returns position and modified list
% findmf: list × elem → {pos,list}
% findmf(<Liste>,<Element>)
findmf(List, Element) ->
  Position = finds(List, Element),
  ModifiedList = insert(delete(List, Element), Element),
  {Position, ModifiedList}.


% findtp implements the transpose strategy
% (after successful search, element is swapped with its immediate predecessor)
% returns position and modified list
% findtp: list × elem → {pos,list}
% findtp(<Liste>,<Element>)
findtp(List, Element) -> findtp(List, Element, 1).

findtp([], _Element, _AccuPosition) -> nil;
findtp([Head | Tail], Element, AccuPosition) when Head == Element ->
  {AccuPosition, [Head | Tail]};
findtp([First, Second | Tail], Element, AccuPosition) when Second == Element ->
  {AccuPosition + 1, [Second, First | Tail]};
findtp([Head | Tail], Element, AccuPosition) ->
  {Position, PartialList} = findtp(Tail, Element, AccuPosition + 1),
  {Position, [Head | PartialList]}.


% retrieve: list × pos → elem
% retrieve(<Liste>,<Position>)
retrieve([], _Position) -> nil;
retrieve([Head | _Tail], 1) -> Head;
retrieve([_Head | Tail], Position) -> retrieve(Tail, Position - 1).
