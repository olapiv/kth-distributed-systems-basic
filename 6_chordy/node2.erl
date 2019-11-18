-module(node2).
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
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    NewStore = storage:create(),
    node(Id, Predecessor, Successor, NewStore).

connect(Id, nil) ->
    % Distributed: 
    % register(node, self()),
    % {ok, {Id, {node, node()}}};

    % Not distributed:
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

node(Id, Predecessor, Successor, Store) ->
    receive
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor, Store);
        {notify, New} ->
            {Pred, Store2} = notify(New, Id, Predecessor, Store),
            node(Id, Pred, Successor, Store2);
        {request, Peer} ->
            request(Peer, Predecessor),
            node(Id, Predecessor, Successor, Store);
        {status, Pred} ->
            % Pred: successors current predecessor
            Succ = stabilize(Pred, Id, Successor),
            node(Id, Predecessor, Succ, Store);
        printStatus ->
            io:format("Node status:~nId: ~w~nPredecessor: ~w~nSuccessor: ~w~nStore: ~w~n", [Id, Predecessor, Successor, Store]),
            node(Id, Predecessor, Successor, Store);
        stop ->
            stop;
        stopAll ->
            {_, Spid} = Successor,
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
            node(Id, Predecessor, Successor, Store);
        probe ->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor, Store);
        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor, Store);
        {probe, Ref, Nodes, T} ->
            forward_probe(Ref, T, Nodes, Id, Successor),
            node(Id, Predecessor, Successor, Store);

        {handover, Elements} ->
            Merged = storage:merge(Store, Elements),
            node(Id, Predecessor, Successor, Merged);

        {add, Key, Value, Qref, Client} ->
            Added = add(Key, Value, Qref, Client,
                        Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Added);

        % The Qref parameters will be used to tag the return message to the Client. 
        % This allows the client to identify the reply message and makes it easier 
        % to implement the client.
        {lookup, Key, Qref, Client} ->
            lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Store);

        WeirdMessage ->
            io:format("Weird message: ~w~n", [WeirdMessage]),
            node(Id, Predecessor, Successor, Store)
end.

add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
    case key:between(Key, Pkey, Id)  of
        true ->
            Client ! {Qref, ok},
            storage:add(Key, Value, Store);
        false ->
            Spid ! {add, Key, Value, Qref, Client},
            Store
    end.

lookup(Key, Qref, Client, Id, {Pkey, _}, Successor, Store) ->
    case key:between(Key, Pkey, Id)  of
        true ->
            % io:format("Data should be stored here: ~w~n", [Id]),
            Result = storage:lookup(Key, Store),
            Client ! {Qref, Result};
        false ->
            {_, Spid} = Successor,
            Spid ! {lookup, Key, Qref, Client}
    end.

create_probe(Id, Successor) ->
    Time = erlang:system_time(micro_seconds),
    Nodes = [Id],
    {_, Spid} = Successor,
    Spid ! {probe, Id, Nodes, Time}.

remove_probe(T, Nodes) ->
    TimeNow = erlang:system_time(micro_seconds),
    TimeTaken = TimeNow - T,
    io:format("Nodes in system: ~w~nTime taken for probe: ~w~n",[Nodes, TimeTaken]).

forward_probe(Ref, T, Nodes, Id, Successor) ->
    Nodes2 = lists:append(Nodes, [Id]),
    {_, Spid} = Successor,
    Spid ! {probe, Ref, Nodes2, T}.

stabilize({_, Spid}) ->
    Spid ! {request, self()}.

stabilize(Pred, Id, Successor) ->
    {Skey, Spid} = Successor,
    case Pred of
        nil ->
            Spid ! {notify, {Id, self()}},
            Successor;
        {Id, _} ->  % Pointing towards us; all is fine.
            Successor;
        {Skey, _} ->  % Pointing towards itself
            Spid ! {notify, {Id, self()}},
            Successor;
        {Xkey, Xpid} ->
            case key:between(Xkey, Id, Skey) of
                true ->  % Xkey is our new successor
                    Xpid ! {notify, {Id, self()}},
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

notify({Nkey, Npid}, Id, Predecessor, Store) ->
    case Predecessor of
        nil ->
            Keep = handover(Id, Store, Nkey, Npid),
            {{Nkey, Npid}, Keep};
        {Pkey, _} ->
            case key:between(Nkey, Pkey, Id) of
                true ->
                    Keep = handover(Id, Store, Nkey, Npid),
                    {{Nkey, Npid}, Keep};
                false ->
                    {Predecessor, Store}
            end
    end.

handover(Id, Store, Nkey, Npid) ->
    {Keep, Rest} = storage:split(Nkey, Id, Store),  % Switched Nkey and Id
    Npid ! {handover, Rest},
    Keep.
