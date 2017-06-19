-module(list).
-compile({no_auto_import,[is_list/1]}).
-export([create/0, is_empty/1, len/1, is_list/1, equal/2, find/2, retrieve/2, delete/2, insert/3, concat/2, diff_list/2, even_odd_count/1]).

% List model: {1, {2, {3, {}}}} => Pos1 = 1, Pos2 = 2 etc
% test lists:
% L1 = {1, {2, {3, {}}}}.
% L2 = {}.
% L3 = {1, {2, {3, {}}}}.
% L4 = {4, {5, {6, {}}}}.
% L5 = {9, {}}.
% L6 = {99, {2, {101, {1, {2, {}}}}}}.

% create: ∅ → list
create() -> {}.


% is_empty: list → bool
% is_empty(List) -> List == {}.
is_empty(List) -> len(List) == 0.


% len: list → int
len(List) -> len(List, 0).

len({}, Accu) -> Accu;
len({_Head, Tail}, Accu) -> len(Tail, Accu + 1).


% is_list: list → bool
is_list({}) -> true;
is_list({_Head, Tail}) -> is_list(Tail);
is_list(_) -> false.


% equal: list × list → bool
% Equal tests for structural equality.
equal({}, {}) -> true;
equal({FirstHead, FirstTail}, {SecondHead, SecondTail}) when FirstHead == SecondHead ->
  equal(FirstTail, SecondTail);
equal(_, _) -> false.


% find: list × elem → pos
% returns nil when element is not found
find(List, Element) -> find(List, Element, 1).

find({}, _, _) -> nil;
find({Head, _Tail}, Element, AccuPosition) when Head == Element -> AccuPosition;
find({_Head, Tail}, Element, AccuPosition) -> find(Tail, Element, AccuPosition + 1).


% retrieve: list × pos → elem
% returns nil when position invalid
retrieve({}, _) -> nil;
retrieve({Head, _Tail}, 1) -> Head;
retrieve({_Head, Tail}, Position) -> retrieve(Tail, Position - 1).


% delete: list × pos → list
% returns unamended list when position is invalid
delete({}, _) -> {};
delete({_Head, Tail}, 1) -> Tail;
delete({Head, Tail}, Position) -> {Head, delete(Tail, Position - 1)}.


% insert:  list × pos × elem → list
% List starts at position 1.
% Insertion is non-destructive:
%   if element is inserted at a position, at which already another element is,
%   the other element is shifted by one position.
insert(List, 1, Element) -> {Element, List};
insert({Head, Tail}, Position, Element) -> {Head, insert(Tail, Position - 1, Element)};
insert({}, _, _) -> {}.


% concat: list × list → list
concat({}, List) -> List;
concat(List, {}) -> List;
concat(FirstList, {Head, Tail}) ->
  LengthFirstList = len(FirstList),
  AccuList = insert(FirstList, LengthFirstList + 1, Head),
  concat(AccuList, Tail).


% diff_liste: list × list → list
diff_list(FirstList, SecondList) -> diff_list(FirstList, SecondList, {}).

diff_list(List, {}, ResultList) -> concat(List, ResultList);
diff_list({}, List, ResultList) -> concat(List, ResultList);
diff_list(FirstList, SecondList, ResultList) ->
  {FirstHead, FirstTail} = FirstList,
  {SecondHead, SecondTail} = SecondList,
  FirstHeadInSecondList = element_in_list(FirstHead, SecondList),
  SecondHeadInFirstList = element_in_list(SecondHead, FirstList),

  if
    FirstHeadInSecondList and SecondHeadInFirstList ->
      NewFirstList = delete_all(FirstTail, SecondHead),
      NewSecondList = delete_all(SecondTail, FirstHead),
      diff_list(NewFirstList, NewSecondList, ResultList);

    FirstHeadInSecondList ->
      NewSecondList = delete_all(SecondTail, FirstHead),
      diff_list(FirstTail, NewSecondList, {SecondHead, ResultList});

    SecondHeadInFirstList ->
      NewFirstList = delete_all(FirstTail, SecondHead),
      diff_list(NewFirstList, SecondTail, {FirstHead, ResultList});

    true -> diff_list(FirstTail, SecondTail, {FirstHead, {SecondHead, ResultList}})
  end.


% even_odd_count: list → [int,int]
% even_odd_count(L) counts the lists with even and odd length within list L including list L itself. An empty list is considered to have
% even length. List L can include elements which are no lists. Return value is the tuple [<number or even lists>, <number of odd lists>]
% Note: the empty list, which indicates the end of the list, is not counted as element or as even list
even_odd_count(List) ->
  IsEven = has_even_length(List),
  if
    IsEven -> even_odd_count(List, [1, 0]);
    not(IsEven) -> even_odd_count(List, [0, 1])
  end.

even_odd_count({}, Result) -> Result;
even_odd_count(List, [Even, Odd]) ->
  {Head, Tail} = List,
  HeadIsList = is_list(Head),
  if
    not(HeadIsList) -> even_odd_count(Tail, [Even, Odd]);
    HeadIsList ->
      HeadIsEven = has_even_length(Head),
      if
        HeadIsEven -> even_odd_count(Tail, [Even + 1, Odd]);
        not(HeadIsEven) -> even_odd_count(Tail, [Even, Odd + 1])
      end
  end.
% {{1, {2, {}}}, {{1, {2, {}}}, {{101, {1, {2, {}}}}, {1, {2, {}}}}}}.
% {{1, {2, {}}}, {2, {{101, {1, {2, {}}}}, {1, {2, {}}}}}}.


% Helper
% element_in_list: elem × list → bool
element_in_list(_Element, {}) -> false;
element_in_list(Element, {Head, _Tail}) when Element == Head -> true;
element_in_list(Element, {_Head, Tail}) -> element_in_list(Element, Tail).


% delete_all: list × elem → list
% deletes all elements with the same value as given element from a given list and returns new list
delete_all({}, _) -> {};
delete_all({Head, Tail}, Element) when Head == Element -> delete_all(Tail, Element);
delete_all({Head, Tail}, Element) -> {Head, delete_all(Tail, Element)}.


% has_even_length: list → bool
has_even_length(List) -> is_even(len(List)).


% is_even: integer → bool
is_even(Integer) -> Integer rem 2 == 0.
