-module(time).
-export([clock/1, safe/2, inc/2, leq/2, update/3, merge/2, minTimeInClock/1]).

zero () ->
    0.

inc(Name, T) ->
    T + 1.

% Use this method to merge entire timestamps.
merge(Ti, Tj) ->
    erlang:max(Ti, Tj).

leq(Ti, Tj) ->
    if
        Ti =< Tj -> true;
        true -> false
    end.

minTimeInClock(Clock) ->
    lists:foldl(
        fun(ClockEntry, MinTime) -> 
            {_, Time} = ClockEntry,
            case leq(Time, MinTime) of
                true -> Time;
                false -> MinTime
            end
        end,
        inf,
        Clock
    ).

% Return a clock that can keep track of the nodes.
clock(Nodes) ->
    lists:map(fun(Node) -> {Node, zero()} end, Nodes).

% Returns a clock that has been updated given that we 
% have received a log message from a node at a given time.
update(Node, Time, Clock) ->
    case lists:keyfind(Node, 1, Clock) of
        false ->
            [{Node, Time} | Clock];
        {_, OldTime} ->
            NewEntry = {Node, erlang:max(Time, OldTime)},
            lists:keyreplace(Node, 1, Clock, NewEntry)
    end.

% Is it safe to log an event that happened at a given time, 
% true or false
safe(TimeToCheck, Clock) ->
    MinTime = minTimeInClock(Clock),
    leq(TimeToCheck, MinTime).
