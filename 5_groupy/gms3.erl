-module(gms3).
-compile(export_all).


-define(timeout, 800).
-define(arghh, 100).

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
    leader(Id, Master, 0, [], [Master]).

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
        {view, N, [Leader|Slaves], Group} ->
            erlang:monitor(process, Leader),
            Master ! {view, Group},
            slave(Id, Master, Leader, N, {view, N, [Leader|Slaves], Group}, Slaves, Group)
    after ?timeout ->
        Master ! {error, "no reply from leader"}
end.


leader(Id, Master, N, Slaves, Group) ->
    receive
        {mcast, Msg} ->
            bcast(Id, {msg, N+1, Msg}, Slaves),
            Master ! Msg,
            leader(Id, Master, N+1, Slaves, Group);
        {join, Wrk, Peer} ->
            Slaves2 = lists:append(Slaves, [Peer]),
            Group2 = lists:append(Group, [Wrk]),
            bcast(Id, {view, N+1, [self()|Slaves2], Group2}, Slaves2),
            Master ! {view, Group2},
            leader(Id, Master, N+1, Slaves2, Group2);
        stop -> 
            ok
end.


slave(Id, Master, Leader, N, Last, Slaves, Group) ->
    receive
        {mcast, Msg} ->
            Leader ! {mcast, Msg},
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        {join, Wrk, Peer} ->
            % Wrk: process identifier of the application layer (Master)
            Leader ! {join, Wrk, Peer},
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        {msg, I, _} when I =< N ->
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        {msg, I, Msg} ->
            Master ! Msg,
            slave(Id, Master, Leader, I, {msg, I, Msg}, Slaves, Group);
        {view, I, [Leader|Slaves2], Group2} ->
            Master ! {view, Group2},
            slave(Id, Master, Leader, I, {view, I, [Leader|Slaves2], Group2}, Slaves2, Group2);
        {'DOWN', _Ref, process, Leader, _Reason} ->
            io:format("Received down!~n", []),
            election(Id, Master, N, Last,  Slaves, Group);
        stop ->
            ok
    end.


% Msg: 
% {view, N, [Leader|Slaves], Group}
% OR
% {msg, N+1, Msg}
bcast(Id, Msg, Nodes) ->
    lists:foreach(fun(Node) -> Node ! Msg, crash(Id) end, Nodes).

election(Id, Master, N, Last,  Slaves, [_|Group]) ->
    Self = self(),
    case Slaves of
        [Self|Rest] ->
            io:format("
I am the new leader!
Id: ~w
self() ~w
            ",
            [Id, self()]),
            bcast(Id, Last, Rest),
            bcast(Id, {view, N, Slaves, Group}, Rest),
            Master ! {view, Group},
            leader(Id, Master, N, Rest, Group);
        [Leader|Rest] ->
            io:format("
I am a slave again!
Id: ~w
            ",
            [Id]),
            erlang:monitor(process, Leader),
            slave(Id, Master, Leader, N, Last, Rest, Group)
end.

crash(Id) ->
    case random:uniform(?arghh) of
        ?arghh ->
            io:format("leader ~w: crashed in pid: ~w~n", [Id, self()]),
            exit(no_luck);
        _ -> 
            ok
end.