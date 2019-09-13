-module(map).
-export([new/0, update/3, reachable/2, all_nodes/1]).

new () ->
    [].

% Updates the Map to reflect that Node has directional 
% links to all nodes in the list Links. The old entry 
% is removed.
update (Node, Links, Map) ->
    NewTuple = {Node, Links},
    TupleFound = lists:keyfind(Node, 1, Map),  % Key, N, TupleList
    case TupleFound of
        false ->
            NewMap = lists:append(Map, NewTuple);
        {_, _} ->
            NewMap = lists:keyreplace(Node, 1, Map, NewTuple)
    end,
    NewMap.


% Returns the list of nodes directly reachable from Node.
reachable (Node, Map) ->
    NodeAndLink = lists:keyfind(Node, 1, Map),
    case NodeAndLink of
        false ->
            [];
        {_, _} ->
            {Node, Links}= NodeAndLink,
            Links
    end.

% Returns a list of all nodes in the map, also the 
% ones without outgoing links. So if berlin is linked 
% to london but london does not have any outgoing links 
% (and thus no entry in the list), london should still 
% be in the returned list.
all_nodes (Map) ->
    AllNodesDuplicates = lists:foldl(
        fun(X, AllNodesAndLinks) -> 
            {Node, Links} = X,
            NodesAndLinks = lists:append([Node], Links),
            lists:append(AllNodesAndLinks, NodesAndLinks)
        end,
        [], 
        Map
    ),
    lists:usort(AllNodesDuplicates).

