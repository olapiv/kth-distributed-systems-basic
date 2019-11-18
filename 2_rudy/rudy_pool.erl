-module(rudy_pool).
-export([initMany/3, start/2, stop/1]).


start(Port, PoolSize) ->
    Pool = [],
    PoolAppended = initMany(Pool, Port, PoolSize),
    PoolAppended.
stop(Pool) ->
    LengthPool = lists:flatlength(Pool),
    if LengthPool == 0 -> ok;
    true ->
        FirstElement = lists:nth(1, Pool),
        exit(FirstElement, "time to die"),
        stop(lists:delete(FirstElement, Pool))
    end.


initMany(Pool, Port, PoolSize) ->
    if PoolSize == 0 ->
        Pool;
    true ->
        P = spawn(fun() -> initSingle(Port) end),
        PoolAppended = lists:append(Pool, [P]),
        initMany(PoolAppended, Port, PoolSize-1)
    end.

initSingle(Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of
        {ok, Listen} ->
            handler(Listen),
            gen_tcp:close(Listen),
            ok;
        {error, Error} ->
            error
    end.

handler(Listen) ->
    case gen_tcp:accept(Listen) of  % how we accept an incoming request. If it succeeds we will have a communication channel open to the client.
        {ok, Client} ->
            request(Client),
            handler(Listen);
        {error, Error} ->
            error
    end.

request(Client) ->
    Recv = gen_tcp:recv(Client, 0),  % The 0 augment 0, tells the system to read as much as possible.
    case Recv of
        {ok, Str} -> 
            {Request, Headers, Body} = http:parse_request(Str),
            Response = reply({Request, Headers, Body}),
            gen_tcp:send(Client, Response);
        {error, Error} ->
            io:format("rudy: error: ~w~n", [Error])
    end,
    gen_tcp:close(Client).

reply({{get, URI, _}, _, _}) ->
    timer:sleep(40),
    http:ok("A random response!").
