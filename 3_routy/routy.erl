-module(routy).
-export([start/2, stop/1, init/1, router/6]).

start(Reg, Name) ->
    register(Reg, spawn(fun() -> init(Name) end)).

stop(Node) ->
    Node ! stop,
    unregister(Node).

init(Name) ->
    Intf = intf:new(),
    Map = map:new(),
    Table = dijkstra:table(Intf, Map),
    Hist = hist:new(Name),
    Msgs = [Hist],
    router(Name, 0, Msgs, Intf, Table, Map).

router(Name, N, Hist, Intf, Table, Map) ->
    % io:format("Called router function ~w~n", [Name]),
    io:format("----------~nCalled router function: ~w~nN: ~w~nHistory: ~w~nIntf: ~w~nTable: ~w~nMap: ~w~n----------~n", [Name, N, Hist, Intf, Table, Map]),
    receive

        update ->
            Table1 = dijkstra:table(intf:list(Intf), Map),
            router(Name, N, Hist, Intf, Table1, Map);

        % Manually order our router to broadcast a link-state message.
        broadcast ->
            Message = {links, Name, N, intf:list(Intf)},
            intf:broadcast(Message, Intf),
            router(Name, N+1, Hist, Intf, Table, Map);

        % When this is sent it is tagged with the counter value. 
        % The counter is then updates so subsequent messages will 
        % have a higher value. When receiving a links-state message 
        % a router must check if this is an old or new message.
        {links, Node, R, Links} ->
            io:format("Received broadcast from ~w~n", [Node]),
            case hist:update(Node, R, Hist) of
                {new, Hist1} ->
                    intf:broadcast({links, Node, R, Links}, Intf),
                    Map1 = map:update(Node, Links, Map),
                    router(Name, N, Hist1, Intf, Table, Map1);
                old ->
                    router(Name, N, Hist, Intf, Table, Map)
            end;

        {add, Node, Pid} ->  % {add, london, Pid}
            Ref = erlang:monitor(process,Pid),
            Intf1 = intf:add(Node, Ref, Pid, Intf),  % {SymbolicName, ProcessReference, Pid}
            router(Name, N, Hist, Intf1, Table, Map);
        {remove, Node} ->
            {ok, Ref} = intf:ref(Node, Intf),
            erlang:demonitor(Ref),
            Intf1 = intf:remove(Node, Intf),
            router(Name, N, Hist, Intf1, Table, Map);
        {'DOWN', Ref, process, _, _}  ->
            {ok, Down} = intf:name(Ref, Intf),
            io:format("~w: exit recived from ~w~n", [Name, Down]),
            Intf1 = intf:remove(Down, Intf),
            router(Name, N, Hist, Intf1, Table, Map);
        
        % Sends a status message to a process, 
        % receives the reply and displays the information.
        {send, To} ->
            io:format("In send; To: ~w~n", [To]),
            case intf:lookup(To, Intf) of
                notfound ->
                    io:format("notfound"),
                    router(Name, N, Hist, Intf, Table, Map);
                {ok, Pid} ->
                    io:format("Found Pid: ~w~n", [Pid]),
                    Pid ! {status, self()},
                    receive
                        {status, Data} ->
                            io:format("Received Data ~w~n", [Data])
                    end,
                    router(Name, N, Hist, Intf, Table, Map)
            end;

        % Can be used to do a pretty-print of the state
        {status, From} ->
            From ! {status, {Name, N, Hist, Intf, Table, Map}},
            router(Name, N, Hist, Intf, Table, Map);

        % If the message is not ours we should forward it. 
        % If we find a suitable gateway in the routing table we 
        % simply forward the message to the gateway.
        {route, To, From, Message} ->
            if 
                To == Name ->
                    io:format("~w: received message ~w ~n", [Name, Message]),
                    router(Name, N, Hist, Intf, Table, Map);
                true ->
                    io:format("~w: routing message ~w ~n", [Name, Message]),
                    case dijkstra:route(To, Table) of
                        {ok, Gw} ->
                            case intf:lookup(Gw, Intf) of
                            {ok, Pid} ->
                                Pid ! {route, To, From, Message};
                            notfound ->
                                ok
                            end;
                        notfound ->
                            ok 
                    end,
                    router(Name, N, Hist, Intf, Table, Map)
            end;


        % Local user can initiate the routing of a message without 
        % knowing the name of the local router.
        {send, To, Message} ->
            self() ! {route, To, Name, Message},
            router(Name, N, Hist, Intf, Table, Map);

        stop ->
            ok 
    end.
