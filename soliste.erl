-module(soliste).
-compile(export_all).
% -export([create/0, isEmpty/1, isList/1, equal/2, laenge/1, insert/2, delete/2, finds/2, findmf/2, findtp/2, retrieve/2]).

%
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

% L3 = [1,2,3,4].
% L4 = [2,3,4,1].
% L5 = [9 | L3].
% L6 = [9 | L4].
% L7 = [9, L3].
% L8 = [9, L4].
% L9 = [9, [1, 2, 3]].
% L10 = [9, [1, [2, [3]]]].
% L11 = [9, [1, [2, [3, []]]]].


% create: ∅ → list
% create()
create() -> [].


% isEmpty: list → bool
% isEmpty(<Liste>)
isEmpty(List) -> List == [].


% isList: list → bool
% isList(<Liste>)
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
%
% finds implementiert dei ganz normale Suche, die als Resultat nur die Position zurück gibt.
% finds: list × elem → pos
% finds(<Liste>,<Element>)
%
% findmf soll sie Move-To-Front Strategie implementieren, gibt Position und die modifizierte Liste zurück.
% findmf: list × elem → {pos,list}
% findmf(<Liste>,<Element>)
%
% findtp die Transpose Strategie, gibt Position und die modifizierte Liste zurück.
% findtp: list × elem → {pos,list}
% findtp(<Liste>,<Element>)
%
% retrieve: list × pos → elem
% retrieve(<Liste>,<Position>)
