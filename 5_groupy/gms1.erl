-module(gms1).
-compile(export_all).


leader(Id, Master, Slaves, Group) ->
    receive
        {mcast, Msg} ->
            bcast(Id, {msg, Msg}, Slaves),
            Master ! Msg,
            leader(Id, Master, Slaves, Group);
        {join, Wrk, Peer} ->
            Slaves2 = lists:append(Slaves, [Peer]),
            Group2 = lists:append(Group, [Wrk]),
            bcast(Id, {view, [self()|Slaves2], Group2}, Slaves2),
            Master ! {view, Group2},
            leader(Id, Master, Slaves2, Group2);
        stop -> 
            ok
end.


slave(Id, Master, Leader, Slaves, Group) ->
    receive
        {mcast, Msg} ->
            Leader ! {mcast, Msg},
            slave(Id, Master, Leader, Slaves, Group);
        {join, Wrk, Peer} ->
            % Wrk: process identifier of the application layer (Master)
            Leader ! {join, Wrk, Peer},
            slave(Id, Master, Leader, Slaves, Group);
        {msg, Msg} ->
            Master ! Msg,
            slave(Id, Master, Leader, Slaves, Group);
        {view, [Leader|Slaves2], Group2} ->
            Master ! {view, Group2},
            slave(Id, Master, Leader, Slaves2, Group2);
        stop ->
            ok 
    end.

start(Id) ->
    Self = self(),  % Pid of the application layer (Master)
    {ok, spawn_link(fun()-> init(Id, Self) end)}.

init(Id, Master) ->
    % leader(Id, Master, Slaves, Group)
    leader(Id, Master, [], [Master]).

start(Id, Grp) ->
    Self = self(),  % Pid of the application layer (Master)
    {ok, spawn_link(fun()-> init(Id, Grp, Self) end)}.
init(Id, Grp, Master) ->
    Self = self(),
    Grp ! {join, Master, Self},
    receive
        {view, [Leader|Slaves], Group} ->
            Master ! {view, Group},
            slave(Id, Master, Leader, Slaves, Group)
end.

% Msg: {view, [Leader|Slaves], Group}
bcast(Id, Msg, Nodes) ->
    lists:foreach(fun(Node) -> Node ! Msg end, Nodes).
