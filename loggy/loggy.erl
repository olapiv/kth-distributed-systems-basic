-module(loggy).
-export([start/1, stop/1, log/3]).

start(Nodes) ->
    spawn_link(fun() ->init(Nodes) end).

stop(Logger) ->
   Logger ! stop.

init(Nodes) ->

    % Clock = time:clock(Nodes),
    Clock = vect:clock(Nodes),

    % Queue = queueLamp:new(),
    Queue = queueVect:new(),

    loop(Clock, Queue).

loop(Clock, Queue) ->
    receive
        {log, From, Time, Msg} ->

            Clock1 = vect:update(From, Time, Clock),
            % Clock1 = time:update(From, Time, Clock),

            Queue1 = queueVect:add(Queue, {From, Time, Msg}),
            % Queue1 = queueLamp:add(Queue, {From, Time, Msg}),

            Queue2 = queueVect:logQueueSafe(Clock1, Queue1),
            % Queue2 = queueLamp:logQueueSafe(Clock1, Queue1),

            loop(Clock1, Queue2);
        stop ->
            ok
        end.

log(From, Time, Msg) ->
    io:format("-LOG- Time: ~w; From: ~w; Message: ~p~n", [Time, From, Msg]).
