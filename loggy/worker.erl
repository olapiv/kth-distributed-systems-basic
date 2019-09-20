-module(worker).
-export([start/5, stop/1, peers/2]).


start(Name, Logger, Seed, Sleep, Jitter) ->
    spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker) ->
    Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter) ->
    random:seed(Seed, Seed, Seed),
    
    OwnTime = [{Name, 0}],
    % OwnTime = 0,
    
    receive
        {peers, Peers} ->
            loop(Name, Log, Peers, Sleep, Jitter, OwnTime);
        stop -> 
            ok
    end.

peers(Wrk, Peers) ->
   Wrk ! {peers, Peers}.

loop(Name, Log, Peers, Sleep, Jitter, OwnTime)->
    Wait = random:uniform(Sleep),
    receive
        {msg, Time, Msg} ->

            % MergedTime = time:merge(Time, OwnTime),
            MergedTime = vect:merge(Time, OwnTime),

            % MergedTimeInc = time:inc(Name, MergedTime),
            MergedTimeInc = vect:inc(Name, MergedTime),

            Log ! {log, Name, MergedTimeInc, {received, Msg}},
            loop(Name, Log, Peers, Sleep, Jitter, MergedTimeInc);

            % case time:leq(Time, OwnTime) of
            % % case vect:leq(Time, OwnTime) of
            %     true -> 
            %         OwnTime2 = time:inc(Name, OwnTime),
            %         % OwnTime2 = vect:inc(Name, OwnTime),
            %         Log ! {log, Name, OwnTime2, {received, Msg}},
            %         loop(Name, Log, Peers, Sleep, Jitter, OwnTime2);
            %     false -> 
            %         Time2 = time:inc(Name, Time),
            %         % Time2 = vect:inc(Name, Time),
            %         Log ! {log, Name, Time2, {received, Msg}},
            %         loop(Name, Log, Peers, Sleep, Jitter, Time2)
            % end;
        stop ->
            ok;
        Error ->
            Log ! {log, Name, time, {error, Error}}
    after Wait ->
        Selected = select(Peers),
        Message = {hello, random:uniform(100)},
        % OwnTime2 = time:inc(Name, OwnTime),
        OwnTime2 = vect:inc(Name, OwnTime),
        Selected ! {msg, OwnTime2, Message},
        jitter(Jitter),
        Log ! {log, Name, OwnTime2, {sending, Message}},
        loop(Name, Log, Peers, Sleep, Jitter, OwnTime2)
    end.


select(Peers) ->
    lists:nth(random:uniform(length(Peers)), Peers).

jitter(0) -> 
    ok;
jitter(Jitter) -> 
    timer:sleep(random:uniform(Jitter)).
