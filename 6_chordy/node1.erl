-module(node1).
-compile(export_all).


-define(Stabilize, 1500).
-define(Timeout, 400).


start(Id) ->
    start(Id, nil).

start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
    Predecessor = nil,
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    node(Id, Predecessor, Successor).

connect(Id, nil) ->
    {ok, {Id, self()}};

connect(Id, Peer) ->
    Qref = make_ref(),  % Why?
    Peer ! {key, Qref, self()},
    receive
        {Qref, Skey} ->
               {ok, {Skey, Peer}}
    after ?Timeout ->
            io:format("Time out: no response~n",[])
end.

%  Called when a node is created:
schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).
    % --> self() ! stabilize

node(Id, Predecessor, Successor) ->
    % io:format("***~nOur pid: ~w~nOur Predecessor: ~w~nOur Successor: ~w~n***~n",[self(), Predecessor, Successor]),
    receive
        {key, Qref, Peer} ->
            io:format("Received a key! From Peer:~w~n", [Peer]),
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor);
        {notify, New} ->
            Pred = notify(New, Id, Predecessor),
            node(Id, Pred, Successor);
        {request, Peer} ->  % A predecessor needs to know our predecessor
            request(Peer, Predecessor),
            node(Id, Predecessor, Successor);
        {status, Pred} ->  % Our successor informs us about its predecessor
            % Pred: successors current predecessor
            Succ = stabilize(Pred, Id, Successor),
            node(Id, Predecessor, Succ);
        printStatus ->
            io:format("Node status:~nId:~w~nPredecessor:~w~n,Successor:~w~n", [Id, Predecessor, Successor]),
            node(Id, Predecessor, Successor);
        stop ->
            stop;
        stabilize ->
            stabilize(Successor),
            node(Id, Predecessor, Successor);
        probe ->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor);
        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor);
        {probe, Ref, Nodes, T} ->
            forward_probe(Ref, T, Nodes, Id, Successor),
            node(Id, Predecessor, Successor);
        WeirdMessage ->
            io:format("Weird message: ~w~n", [WeirdMessage])
end.

create_probe(Id, Successor) ->
    Time = erlang:system_time(micro_seconds),
    Nodes = [self()],
    {_, Spid} = Successor,
    Spid ! {probe, Id, Nodes, Time}.

remove_probe(T, Nodes) ->
    TimeNow = erlang:system_time(micro_seconds),
    TimeTaken = TimeNow - T,
    io:format("Nodes in system: ~w~nTime taken for probe: ~w~n",[Nodes, TimeTaken]).

forward_probe(Ref, T, Nodes, Id, Successor) ->
    Nodes2 = lists:append(Nodes, [self()]),
    {_, Spid} = Successor,
    Spid ! {probe, Ref, Nodes2, T}.

stabilize({_, Spid}) ->
    Spid ! {request, self()}.

stabilize(Pred, Id, Successor) ->
    {Skey, Spid} = Successor,
    % io:format("Our pid: ~w~nOur Succesor: ~w~nOur successor's predecessor: ~w~n",[self(), Successor, Pred]),
    case Pred of  % Pred of our successor
        nil ->
            Spid ! {notify, {Id, self()}},
            Successor;
        {Id, _} ->  % Pointing towards us; all is fine.
            Successor;
        Successor ->  % Pointing towards itself
            Spid ! {notify, {Id, self()}},
            Successor;
        {Xkey, Xpid} ->  % Our successor has a different predecessor than ourselves
            case key:between(Xkey, Id, Skey) of
                true ->  % Xkey is our new successor
                    % Xpid ! {notify, {Id, self()}},
                    {Xkey, Xpid};
                false ->
                    Spid ! {notify, {Id, self()}},
                    Successor
            end
    end.

request(Peer, Predecessor) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil};
        {Pkey, Ppid} ->
            Peer ! {status, {Pkey, Ppid}}
    end.

notify({Nkey, Npid}, Id, Predecessor) ->
    case Predecessor of
        nil ->
            {Nkey, Npid};
        {Pkey,  _} ->
            case key:between(Nkey, Pkey, Id) of
                true -> 
                    {Nkey, Npid};
                false -> 
                    Predecessor
            end
    end.
