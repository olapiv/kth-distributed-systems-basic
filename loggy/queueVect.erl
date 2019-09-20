-module(queueVect).
-export([new/0, add/2, logQueueSafe/2]).


new() ->
    [].

% Add message to queue
% Message = {From, Time, Msg}
add(Queue, Message) ->
    sort([Message | Queue]).

% Print safe messages and return unsafe ones
logQueueSafe(Clock, Queue) ->
    UnsafeQueue = lists:dropwhile(
        fun({From, Time, Msg})->
            case vect:safe(Time, Clock) of
                true ->
                    loggy:log(From, Time, Msg),
                    true;
                false ->
                    false
            end 
        end,									
        Queue
    ),
    sort(UnsafeQueue).

sort(Queue) ->
    lists:keysort(2, Queue).