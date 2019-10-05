-module(chordy).
-compile(export_all).


-define(TestingMachines, 1).
-define(KeysPerTestingMachine, 20).
-define(NodesInRing, 4).

start(Module, TestingMachines, NodesInRing, KeysPerTestingMachine) ->
    register(chordy, test:start(Module)),
    Ns = lists:seq(2, NodesInRing),
    lists:foreach(fun(K) -> test:start(node2, chordy) end, Ns),
    timer:sleep(400),
    Ts = lists:seq(1,TestingMachines),
    Start = erlang:system_time(micro_seconds),
    lists:foreach(fun(K) -> initTestingMachine(KeysPerTestingMachine, self()) end, Ts),
    collect(TestingMachines),
    Finish = erlang:system_time(micro_seconds),
    TimeTotal = (Finish - Start) / 1000,
    TimePerLookup = TimeTotal / (TestingMachines * KeysPerTestingMachine),
    io:format("TestingMachines: ~w~nNodesInRing: ~w~nKeysPerTestingMachine: ~w~nTime taken: ~w ms~nTimePerLookup: ~w ms~n", [TestingMachines, NodesInRing, KeysPerTestingMachine, TimeTotal, TimePerLookup]),
    chordy ! stopAll.

initTestingMachine(KeysPerTestingMachine, Controller) ->
    spawn(fun() -> testingMachine(KeysPerTestingMachine, Controller) end).

testingMachine(KeysPerTestingMachine, Controller) ->
    Keys = test:keys(KeysPerTestingMachine),
    test:add(Keys, chordy),
    % timer:sleep(400),
    test:check(Keys, chordy),
    Controller ! ok.

collect(0) ->
    ok;
collect(N) ->    
    receive 
        ok ->
            collect(N-1)
    end.