-module(tree_helper).
-export([init_leaf/1, parent_element/1, height/1, max_height/2, minimum_node/1, maximum_node/1, rotate_left/1, rotate_right/1, write_to_file/2]).


% init_leaf: elem → btree
init_leaf(Element) -> {{Element, 1}, {}, {}}.


% parent_element: btree → integer
% Get element of parent node of given (partial) tree
parent_element({{Parent, _Height}, _LeftChild, _RightChild}) -> Parent;
parent_element(_Object) -> nil.


% height: btree → integer
% Get height of parent node of given (partial) tree
height({{_Parent, Height}, _LeftChild, _RightChild}) -> Height;
height(_Object) -> 0.


% max_height: btree × btree → integer
% Get max. height of the two given (partial) trees
max_height({}, {}) -> 0;
max_height(FirstTree, SecondTree) -> max(height(FirstTree), height(SecondTree)).


% Find minimum value in tree
% No sub-tree on left side of parent => parent already minimum
minimum_node({{Parent, _Height}, {}, _ChildRight}) ->
  Parent;
% Still sub-tree on left side of parent => parent not minimum
minimum_node({_Parent, ChildLeft, _ChildRight}) ->
  minimum_node(ChildLeft).


% Find maximum value in tree
% No sub-tree on right side of parent => parent already maximum
maximum_node({{Parent, _Height}, _ChildLeft, {}}) ->
  Parent;
% Still sub-tree on right side of parent => parent not maximum
maximum_node({_Parent, _ChildLeft, ChildRight}) ->
  maximum_node(ChildRight).


% rotate left = zack rotation
rotate_left({ParentNode, {}, {}}) -> {ParentNode, {}, {}};
rotate_left({{P1, _}, CL1, {{P2, _}, CL2, CR2}}) ->
  % Update height P1 and P2 (all others remain the same)
  HeightP1 = max_height(CL1, CL2) + 1,
  HeightP2 = max(height(CR2), HeightP1) + 1,
  {{P2, HeightP2}, {{P1, HeightP1}, CL1, CL2}, CR2}.


% rotate right = zick rotation
rotate_right({ParentNode, {}, {}}) -> {ParentNode, {}, {}};
rotate_right({{P1, _}, {{P2, _}, CL2, CR2}, CR1}) ->
  % Update height P1 and P2 (all others remain the same)
  HeightP1 = max_height(CR1, CR2) + 1,
  HeightP2 = max(height(CL2), HeightP1) + 1,
  {{P2, HeightP2}, CL2, {{P1, HeightP1}, CR2, CR1}}.


% writes tree to given file
write_to_file({{_Parent, 1}, _LeftNode, _RightNode}, _Filename) ->
  ok;
write_to_file({{Parent, _Height}, {}, RightNode}, Filename) ->
  {{CR1, HR1},_,_} = RightNode,
  file:write_file(Filename, io_lib:fwrite("  ~p -> ~p [label = ~p];  \n",   [Parent, CR1, HR1+1]), [append]),
  write_to_file(RightNode, Filename);
write_to_file({{Parent, _Height}, LeftNode, {}}, Filename) ->
  {{CL1, HL1},_,_} = LeftNode,
  file:write_file(Filename, io_lib:fwrite("  ~p -> ~p [label = ~p];  \n",   [Parent, CL1, HL1+1]), [append]),
  write_to_file(LeftNode, Filename);
write_to_file({{Parent, _Height}, LeftNode, RightNode}, Filename) ->
  {{CL1, HL1},_,_} = LeftNode,
  {{CR1, HR1},_,_} = RightNode,
  file:write_file(Filename, io_lib:fwrite("  ~p -> ~p [label = ~p];  \n",   [Parent, CL1, HL1+1]), [append]),
  file:write_file(Filename, io_lib:fwrite("  ~p -> ~p [label = ~p];  \n",   [Parent, CR1, HR1+1]), [append]),
  write_to_file(LeftNode, Filename),
  write_to_file(RightNode, Filename).
