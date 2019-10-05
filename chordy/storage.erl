-module(storage).
-compile(export_all).


create() ->
    [].

add(Key, Value, Store) ->
    case lookup(Key, Store) of
        {_, ValueLookup} ->
            lists:keyreplace(Key, 1, Store, {Key, ValueLookup});
        false ->
            [{Key, Value}| Store]
    end.

% Returns a tuple {Key, Value} or the atom false
lookup(Key, Store) ->
    lists:keyfind(Key, 1, Store).

% Returns a tuple {Updated, Rest} where the updated store only contains the 
% key value pairs requested and the rest are found in a list of key-value pairs
split(From, To, Store) ->
    lists:partition(fun({KeyEntry, _}) -> key:between(KeyEntry, From, To) end, Store).

    % lists:foldl(
    %     fun({KeyEntry, ValueEntry}, {Updated, Rest}) ->
    %         case key:between(KeyEntry, From, To) of
    %             true ->
    %                 {[ {KeyEntry, ValueEntry} | Updated], Rest};
    %             false ->
    %                 { Updated, [{KeyEntry, ValueEntry} | Rest]}
    %         end
    %     end,
    %     {[], []},
    %     Store
    % ).

% Add a list of key-value pairs to a store
merge(Entries, Store) ->
    Entries ++ Store.
