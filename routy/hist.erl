-module(hist).
-export([new/1, update/3]).

% Returns a new history, where messages from Name will 
% always be seen as old.
new(Name) ->
    {Name, 0}.

% Check if message number N from the Node is old or new. 
% If it is old then return old but if it new return 
% {new, Updated} where Updated is the updated history.
update(Node, N, History) ->
    case lists:keyfind(Node, 1, History) of
        false ->
            NewHistory = lists:append(History, [new(Node)]),
            {new, NewHistory};
            % update(Node, N, NewHistory);
        {_, Number} ->
            if
                N > Number ->
                    Updated = lists:keyreplace(Node, 1, History, {Node, N}),
                    {new, Updated};
                true ->
                    old
            end
    end.
