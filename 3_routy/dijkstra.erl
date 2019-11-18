-module(dijkstra).
-export([entry/2, replace/4, update/4, iterate/3, table/2, route/2]).

% Sorted list:
% [{NodeName, LengthOfPathToNode ,GatewayToUseToReachNode}, {...}]
% [{berlin, 2, paris}, {...}]
% The list is sorted based on length of path

% Routing table:
% [{berlin,madrid},{rome,paris},{madrid,madrid},{paris,paris}]
% If we want to send something to berlin we should send it to madrid.
% If we want to send something to madrid we should send it to madrid.


% Returns the length of the shortest path to the node or 0 
% if the node is not found.
entry(Node, Sorted) ->
    TupleFound = lists:keyfind(Node, 1, Sorted),
    case TupleFound of
        false ->
            0;
        {_, Length, _} ->
            Length
    end.

% Replaces the entry for Node in Sorted with a new entry 
% having a new length N and Gateway. The resulting list should 
% of course be sorted.
replace(Node, N, Gateway, Sorted) ->
    case entry(Node, Sorted) of
        0 ->
            Sorted;
        _ ->
            NewTuple = {Node, N, Gateway},
            Unsorted = lists:keyreplace(Node, 1, Sorted, NewTuple),
            lists:keysort(2,Unsorted)
    end.

% Update the list Sorted given the information that Node can 
% be reached in N hops using Gateway. If no entry is found then 
% no new entry is added. Only if we have a better (shorter) path 
% should we replace the existing entry.
update(Node, N, Gateway, Sorted) ->
    Length = entry(Node, Sorted),
    if
        N >= Length ->
            Sorted;
        true -> 
            replace(Node, N, Gateway, Sorted)
    end.

updateMultiStaticParams(NodeList, N, Gateway, Sorted) ->
    case NodeList of
        [] ->
            Sorted;
        _ ->
            [Node | RestNodeList]  = NodeList,
            NewSorted = update(Node, N, Gateway, Sorted),
            updateMultiStaticParams(RestNodeList, N, Gateway, NewSorted)
    end.

updateMultiGateways(GatewayList, Sorted) ->
    case GatewayList of
        [] ->
            Sorted;
        _ ->
            [Node | RestNodeList]  = GatewayList,
            NewSorted = update(Node, 0, Node, Sorted),
            updateMultiGateways(RestNodeList, NewSorted)
    end.

% Construct a table given a sorted list of nodes, a map and 
% a table constructed so far.
iterate(Sorted, Map, Table) ->
    case Sorted of
        [] ->
            Table;
        _ ->
            [{ Node, Length, Gateway } | _]  = Sorted,
            if
                Length == inf ->
                    Table;
                true ->
                    ReachableList = map:reachable (Node, Map),
                    NewSorted = updateMultiStaticParams(ReachableList, Length+1, Gateway, Sorted),
                    NewSortedDel = lists:keydelete(Node, 1, NewSorted),
                    NewTable = lists:append(Table, [{Node, Gateway}]),
                    iterate(NewSortedDel, Map, NewTable)
            end
    end.

    % dijkstra:iterate(
    %   Sorted=[{paris, 0, paris}, {berlin, inf, unknown}], 
    %   Map=[{paris, [berlin]}], 
    %   Table=[]
    % ).

    % Sorted = [{berlin, 3, paris}, {berlin, 4, stockholm}]
    % Map = [{berlin,[copenhagen]}, {madrid, [barcelona]}]
    % --> copenhagen can be reached in 4 hops using paris.

dummySorted(NodesList, Sorted) ->
    case NodesList of
        [] ->
            Sorted;
        _ ->
            [Node | Rest] = NodesList,
            NewSorted = lists:append(Sorted, [{Node, inf, unknown}]),
            dummySorted(Rest, NewSorted)
    end.

% Construct a routing table given the gate- ways and a map.
table(Gateways, Map) ->
    AllNodes = map:all_nodes(Map),
    SortedDummy = dummySorted(AllNodes, []),
    SortedGateways = updateMultiGateways(Gateways, SortedDummy),
    iterate(SortedGateways, Map, []).

% dijkstra:table(
%   Gateways=[paris, madrid], 
%   Map=[{madrid,[berlin]}, {paris, [rome,madrid]}]
% ).

% dijkstra:table([paris, madrid], [{madrid,[berlin]}, {paris, [rome,madrid]}]).

% Search the routing table and return the gateway suitable to 
% route messages to a node. If a gateway is found we should 
% return {ok, Gateway} otherwise we return notfound.
route(Node, Table) ->
    case lists:keyfind(Node, 1, Table) of
        false ->
            notfound;
        {_, GoToNode} ->
            if 
                GoToNode == Node ->
                    {ok, GoToNode};
                true ->
                    route(GoToNode, Table)
            end
    end.

% Table = [{paris,paris},{madrid,madrid},{rome,paris},{berlin,madrid}].
% dijkstra:route(berlin, Table).