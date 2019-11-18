-module(ping_receiver).
-export([receive_ping/0]).
receive_ping() ->
    PID = self(),
    io:format("Started ping_receiver! Own PID is : ~p~n", [PID]),
    receive {{pid, X}, {node, Y}} -> 
        io:format("Received a message! PID of other process: ~p~n", [X]),
        io:format("Now sending PID back..!", []),
        {X, Y} ! {pid, PID}
    end,
    receive_ping().
