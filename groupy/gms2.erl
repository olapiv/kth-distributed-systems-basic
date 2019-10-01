-module(gms2).
-compile(export_all).


% Group process:
% - ??? The first node of a group (not called "first process" though).
% - It is initialized by an application process. 
% - Can be leader or slave of group. Initially slave though.
% - It is the process in the group that the application process
%   will be communicating to.

% Application process:
% - the master
% - Also "part of the group" by providing the group it's group process pid


start(Id) ->
    Rnd = random:uniform(1000),
    Self = self(),  % Pid of the application layer (Master)
    {ok, spawn_link(fun()-> init(Id, Rnd, Self) end)}.

init(Id, Rnd, Master) ->
    random:seed(Rnd, Rnd, Rnd),
    io:format("
Initialising leader
Self: ~w
    ", [self()]),
    leader(Id, Master, [], [Master]).

start(Id, Grp) ->
    Rnd = random:uniform(1000),
    Self = self(),  % Pid of the application layer (Master)
    {ok, spawn_link(fun()-> init(Id, Rnd, Grp, Self) end)}.
init(Id, Rnd, Grp, Master) ->
    random:seed(Rnd, Rnd, Rnd),
    Self = self(),
    io:format("
Initialising slave
Self: ~w
    ", [Self]),
    Grp ! {join, Master, Self},
    receive
        {view, [Leader|Slaves], Group} ->
            erlang:monitor(process, Leader),
%             io:format("
% Initialising slave, received first broadcast from leader.
% Leader: ~w
% Master: ~w
% Slaves: ~w
%             ", 
%             [Leader, Master, Slaves]),
            Master ! {view, Group},
            slave(Id, Master, Leader, Slaves, Group)
    after 400 ->
        Master ! {error, "no reply from leader"}
end.


leader(Id, Master, Slaves, Group) ->
%     io:format("
% Restarting leader.
% Id: ~w
% Master: ~w
% Slaves: ~w
% Group: ~w
% self(): ~w
%     ", [Id, Master, Slaves, Group, self()]),
    receive
        {mcast, Msg} ->
            bcast(Id, {msg, Msg}, Slaves),
            Master ! Msg,
            leader(Id, Master, Slaves, Group);
        {join, Wrk, Peer} ->
            Slaves2 = lists:append(Slaves, [Peer]),
            Group2 = lists:append(Group, [Wrk]),
            io:format("Group2: ~w~n", [Group2]),
            bcast(Id, {view, [self()|Slaves2], Group2}, Slaves2),
            Master ! {view, Group2},
            leader(Id, Master, Slaves2, Group2);
        stop -> 
            ok
end.


slave(Id, Master, Leader, Slaves, Group) ->
%     io:format("
% Restarting slave. 
% Id: ~w
% Leader: ~w
% Master: ~w
% Slaves: ~w
% Group: ~w
% self(): ~w
%     ", [Id, Leader, Master, Slaves, Group, self()]),
    receive
        {mcast, Msg} ->
            Leader ! {mcast, Msg},
            slave(Id, Master, Leader, Slaves, Group);
        {join, Wrk, Peer} ->
            % Wrk: process identifier of the application layer (Master)
%             io:format("
% Slave received join!
% Id: ~w,
% Slaves: ~w
% Group: ~w
%             ", [Id, Slaves, Group]),
            Leader ! {join, Wrk, Peer},
            slave(Id, Master, Leader, Slaves, Group);
        {msg, Msg} ->
            Master ! Msg,
            slave(Id, Master, Leader, Slaves, Group);
        {view, [Leader|Slaves2], Group2} ->
%             io:format("
% Slave received multicasted view from leader!
% Id: ~w
% Leader: ~w
% Slaves2: ~w
% Group2: ~w
%             ", [Id, Leader, Slaves2, Group2]),
            Master ! {view, Group2},
            slave(Id, Master, Leader, Slaves2, Group2);
        {'DOWN', _Ref, process, Leader, _Reason} ->
            election(Id, Master, Slaves, Group);
        stop ->
            ok
    end.


% Msg: {view, [Leader|Slaves], Group}
bcast(Id, Msg, Nodes) ->
    lists:foreach(fun(Node) -> Node ! Msg, crash(Id) end, Nodes).

election(Id, Master, Slaves, [_|Group]) ->
    Self = self(),
    case Slaves of
        [Self|Rest] ->
            io:format("
I am the new leader!
Id: ~w
self(): ~w
            ",
            [Id, self()]),
            bcast(Id, {view, Slaves, Group}, Rest),
            Master ! {view, Group},
            leader(Id, Master, Rest, Group);
        [Leader|Rest] ->
            io:format("
I am a slave again!
Id: ~w
            ",
            [Id]),
            erlang:monitor(process, Leader),
            slave(Id, Master, Leader, Rest, Group)
end.

crash(Id) ->
    case random:uniform(15) of
        15 ->
            io:format("leader ~w: crashed in pid: ~w~n", [Id, self()]),
            exit(no_luck);
        _ -> 
            ok
end.