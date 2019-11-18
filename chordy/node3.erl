-module(node3).
-compile(export_all).


-define(Stabilize, 50).
-define(Timeout, 400).


start(Id) ->
    start(Id, nil).

start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
    Predecessor = nil,
    Next = nil,
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    NewStore = storage:create(),
    node(Id, Predecessor, Successor, NewStore, Next).

connect(Id, nil) ->
    % Distributed: 
    % register(node, self()),
    % {ok, {Id, {node, node()}}};

    % Not distributed:
    {ok, {Id, nil, self()}};

connect(Id, Peer) ->
    Qref = make_ref(),  % Why?
    Peer ! {key, Qref, self()},
    receive
        {Qref, Skey} ->
            io:format("Monitoring successor~n", []),
            {ok, {Skey, monitor(Peer), Peer}}
    after ?Timeout ->
            io:format("Time out: no response~n",[])
end.

%  Called when a node is created:
schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

node(Id, Predecessor, Successor, Store, Next) ->
    receive
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor, Store, Next);
        {notify, New} ->
            {Pred, Store2} = notify(New, Id, Predecessor, Store),
            node(Id, Pred, Successor, Store2, Next);
        {request, Peer} ->
            request(Peer, Predecessor, Successor),
            node(Id, Predecessor, Successor, Store, Next);
        {status, Pred, Nx} ->
            % Pred: successors current predecessor
            % Nx: successors current successor
            {Succ, Nxt} = stabilize(Pred, Nx, Id, Successor),
            node(Id, Predecessor, Succ, Store, Nxt);
        printStatus ->
            io:format("Node status:~nId: ~w~nPredecessor: ~w~nSuccessor: ~w~nStore: ~w~n", [Id, Predecessor, Successor, Store]),
            node(Id, Predecessor, Successor, Store, Next);
        stop ->
            stop;
        stopAll ->
            {_, _, Spid} = Successor,
            case is_atom(Spid) of
                true ->
                    case whereis(Spid) of
                        undefined ->
                            stop;
                        _ ->
                            Spid ! stopAll
                    end;
                false ->
                    Spid ! stopAll
            end;
        stabilize ->
            stabilize(Successor),
            node(Id, Predecessor, Successor, Store, Next);
        probe ->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor, Store, Next);
        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor, Store, Next);
        {probe, Ref, Nodes, T} ->
            forward_probe(Ref, T, Nodes, Id, Successor),
            node(Id, Predecessor, Successor, Store, Next);

        {handover, Elements} ->
            Merged = storage:merge(Store, Elements),
            node(Id, Predecessor, Successor, Merged, Next);

        {add, Key, Value, Qref, Client} ->
            Added = add(Key, Value, Qref, Client,
                        Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Added, Next);

        % The Qref parameters will be used to tag the return message to the Client. 
        % This allows the client to identify the reply message and makes it easier 
        % to implement the client.
        {lookup, Key, Qref, Client} ->
            lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Store, Next);

        {'DOWN', Ref, process, _, _} ->
            {Pred, Succ, Nxt}  = down(Ref, Predecessor, Successor, Next),
            node(Id, Pred, Succ, Store, Nxt);

        WeirdMessage ->
            io:format("Weird message: ~w~n", [WeirdMessage]),
            node(Id, Predecessor, Successor, Store, Next)
end.

add(Key, Value, Qref, Client, Id, {Pkey, _, _}, {_, _, Spid}, Store) ->
    case key:between(Key, Pkey, Id)  of
        true ->
            Client ! {Qref, ok},
            storage:add(Key, Value, Store);
        false ->
            Spid ! {add, Key, Value, Qref, Client},
            Store
    end.

lookup(Key, Qref, Client, Id, {Pkey, _, _}, Successor, Store) ->
    case key:between(Key, Pkey, Id)  of
        true ->
            Result = storage:lookup(Key, Store),
            Client ! {Qref, Result};
        false ->
            {_, _, Spid} = Successor,
            Spid ! {lookup, Key, Qref, Client}
    end.

create_probe(Id, Successor) ->
    Time = erlang:system_time(micro_seconds),
    Nodes = [Id],
    {_, _, Spid} = Successor,
    Spid ! {probe, Id, Nodes, Time}.

remove_probe(T, Nodes) ->
    TimeNow = erlang:system_time(micro_seconds),
    TimeTaken = TimeNow - T,
    io:format("Nodes in system: ~w~nTime taken for probe: ~w~n",[Nodes, TimeTaken]).

forward_probe(Ref, T, Nodes, Id, Successor) ->
    Nodes2 = lists:append(Nodes, [Id]),
    {_, _, Spid} = Successor,
    Spid ! {probe, Ref, Nodes2, T}.

stabilize({_, _, Spid}) ->
    Spid ! {request, self()}.

stabilize(Pred, Next, Id, Successor) ->
    {Skey, Sref, Spid} = Successor,
    case Pred of
        nil ->
            Spid ! {notify, {Id, self()}},
            {Successor, Next};
        {Id, _, _} ->  % Pointing towards us; all is fine.
            {Successor, Next};
        {Skey, _, _} ->  % Pointing towards itself
            Spid ! {notify, {Id, self()}},
            {Successor, Next};
        {Xkey, _, Xpid} ->
            case key:between(Xkey, Id, Skey) of
                true ->  % Xkey is our new successor
                    Xpid ! {notify, {Id, self()}},
                    drop(Sref),
                    io:format("Monitoring successor~n", []),
                    {{Xkey, monitor(Xpid), Xpid}, nil};
                false ->
                    Spid ! {notify, {Id, self()}},
                    {Successor, Next}
            end
    end.

request(Peer, Predecessor, Successor) ->
    Peer ! {status, Predecessor, Successor}.

notify({Nkey, Npid}, Id, Predecessor, Store) ->
    case Predecessor of
        nil ->
            Keep = handover(Id, Store, Nkey, Npid),
            io:format("Monitoring predecessor~n", []),
            {{Nkey, monitor(Npid), Npid}, Keep};
        {Pkey, Pref, Ppid} ->
            case key:between(Nkey, Pkey, Id) of
                true ->
                    Keep = handover(Id, Store, Nkey, Npid),

                    drop(Pref),
                    % if 
                    %     Ppid == self() -> 
                    %         io:format("Avoiding to drop predecessor. he is me. self(): ~w~n", [self()]),
                    %         ok;
                    %     true -> 
                    %         io:format("Dropping predecessor~n", []),
                    %         drop(Pref)
                    % end,

                    io:format("Monitoring predecessor~n", []),
                    {{Nkey, monitor(Npid), Npid}, Keep};
                false ->
                    {Predecessor, Store}
            end
    end.

handover(Id, Store, Nkey, Npid) ->
    {Keep, Rest} = storage:split(Nkey, Id, Store),  % Switched Nkey and Id
    Npid ! {handover, Rest},
    Keep.

% Predecessor goes down:
down(Ref, {_, Ref, _}, Successor, Next) ->
    {nil, Successor, Next};

% Successor goes down:
down(Ref, Predecessor, {_, Ref, _}, {Nkey, _, Npid}) ->
    self() ! stabilize,
    io:format("Monitoring Successor~n", []),
    {Predecessor, {Nkey, monitor(Npid), Npid}, nil}.

monitor(Pid) ->
    io:format("In monitor~nself(): ~w~nPid: ~w~n", [self(), Pid]),
    erlang:monitor(process, Pid).

drop(nil) ->
    ok;

drop(Ref) ->
    io:format("In drop~nself(): ~w~nRef: ~w~n", [self(), Ref]),
    erlang:demonitor(Ref, [flush]).