-module(ping_module).
-export([ping/2]).
ping(MainProcess, MainNode) ->
    io:format("Received input-values are MainProcess: ~p and MainNode: ~p ~n", [MainProcess, MainNode]),
    PID = self(),  % Get own process identifier
    Node = node(),
    register(ping_reg, PID),
    io:format("Sending a message now. Own pid: ~p and Own Node: ~p~n", [PID, Node]),
    {MainProcess, MainNode} ! {{pid, ping_reg}, {node, Node}},
    receive {pid, X} -> 
        io:format("Received a message back! PID of other process: ~p~n", [X])
    end.