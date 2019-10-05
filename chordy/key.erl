-module(key).
-compile(export_all).


generate() ->
    % Rnd = random:uniform(1000),
    {Rnd1, Rnd2, Rnd3} = os:timestamp(),
    random:seed(Rnd1, Rnd2, Rnd3),
    random:uniform(1000000000).

between(Key, From, To) ->
    if 
        
        From >= To ->
            if 
                Key > From ->
                    true;
                Key =< To ->
                    true;
                true ->
                    false
            end;

        Key > From ->
            if 
                Key =< To -> 
                    true;
                true ->
                    false
            end;

        true ->
            false

    end.
