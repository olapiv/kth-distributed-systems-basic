-module(vectTut).
-export([zero/0, inc/2, merge/2, le/2, clock/1, update/3, safe/2]).


zero() -> 
    0.

inc(Name, Time) ->
    case lists:keyfind(Name, 1, Time) of
        ... ->
            lists:keyreplace(Name, 1, Time, ...);
        false ->
            [...|Time]
end.

merge([], Time) ->
    [].

merge([{Name, Ti}|Rest], Time) ->
    case lists:keyfind(Name, 1, Time) of
        {Name, Tj} ->
            [... |merge(Rest, lists:keydelete(Name, 1, Time))];
        false ->
            [... |merge(Rest, Time)]
end.

leq([], _) ->
    ...;

leq([{Name, Ti}|Rest],Time) ->
    case lists:keyfind(Name, 1, Time) of
        {Name, Tj} ->
            if
                Ti =< Tj ->
                   ...:
                true -> ...
            end; 
            false ->
                ... 
            end.

clock(_) ->
    [].

update(From, Time, Clock) ->
    ...  = lists:keyfind(From, 1, Time),
    case lists:keyfind(From, 1, Clock) of
        {From, _} ->
            lists:keyreplace(From, 1, Clock, ...);
        false ->
             [...| Clock]
    end.

safe(Time, Clock) ->
   ....