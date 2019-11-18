-module(intf).
-export([new/0, add/4, remove/2, lookup/2, ref/2, name/2, list/1, broadcast/2]).

% A router will also need to keep track of a set of interfaces.
% { Symbolic name (london), process reference, process identifier }

% Returns an empty set of interfaces.
new() ->
    [].

% Adds a new entry to the set and return
% the new set of interfaces.
add(Name, Ref, Pid, Intf) ->
    lists:append(Intf, [{Name, Ref, Pid}]).

% Removes an entry given a name of an 
% interface, return a new set of interfaces.
remove(Name, Intf) ->
    lists:keydelete(Name, 1, Intf).

% Finds the process identifier given a name, 
% return {ok, Pid} if found otherwise notfound.
lookup(Name, Intf) ->
    case lists:keyfind(Name, 1, Intf) of
        false ->
            notfound;
        {_, _, Pid} ->
            {ok, Pid}
    end.

% Finds the reference given a name and return 
% {ok, Ref} or notfound.
ref(Name, Intf) ->
    case lists:keyfind(Name, 1, Intf) of
        false ->
            notfound;
        {_, Ref, _} ->
            {ok, Ref}
    end.

% Finds the name of an entry given a reference 
% and return {ok, Name} or notfound.
name(Ref, Intf) ->
    case lists:keyfind(Ref, 2, Intf) of
        false ->
            notfound;
        {Name, _, _} ->
            {ok, Name}
    end.

% Returns a list with all names.
list(Intf) ->
    lists:foldl(
        fun(X, AllNames) -> 
            {Name, _, _} = X,
            lists:append(AllNames, [Name])
        end,
        [], 
        Intf
    ).

% Sends the message to all interface processes.
broadcast(Message, Intf) ->
    case Intf of
        [] ->
            ok;
        _ ->
            [{ _, _, Pid } | RestOfInterface] = Intf,
            Pid ! Message,
            broadcast(Message, RestOfInterface)
    end.