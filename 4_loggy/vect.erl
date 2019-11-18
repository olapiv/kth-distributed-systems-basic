-module(vect).
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).


zero() -> 
    [].

% [{john, 3}, {ringo, 2}, {paul, 4}, {george, 1}]
inc(Name, Time) ->
    case lists:keyfind(Name, 1, Time) of
        {_, TimeNumber} ->
            lists:keyreplace(Name, 1, Time, {Name, TimeNumber + 1});
        false ->
            [{Name, 1} | Time]
end.

merge([], Time) ->
    Time;

merge([{Name, Ti}|Rest], Time) ->
    case lists:keyfind(Name, 1, Time) of
        {Name, Tj} ->
            [{Name, erlang:max(Ti, Tj)} |merge(Rest, lists:keydelete(Name, 1, Time))];
        false ->
            [{Name, Ti} | merge(Rest, Time)]
end.

leq([], _) -> true;

leq([{Name, Ti}|Rest],Time) ->
    case lists:keyfind(Name, 1, Time) of
        {Name, Tj} ->
            if
                Ti =< Tj ->
                    leq(Rest,Time);
                true -> false
            end; 
            false -> false
            end.

clock(_) ->
    [].

% Question 1: Why not take the largest one?
% Question 2: Why only take the time from "From"?
update(From, Time, Clock) ->
    FromTime = lists:keyfind(From, 1, Time),
    case lists:keyfind(From, 1, Clock) of
        {From, _} ->
            lists:keyreplace(From, 1, Clock, FromTime);
        false ->
             [FromTime| Clock]
    end.

safe(Time, Clock) ->
    leq(Time, Clock).