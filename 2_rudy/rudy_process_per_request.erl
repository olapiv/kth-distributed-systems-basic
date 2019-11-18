-module(rudy).
-export([init/1, start/1, stop/0]).

% -import(http, [parse_request/1]).

start(Port) ->
    register(rudy, spawn(fun() -> init(Port) end)).
stop() ->
    exit(whereis(rudy), "time to die").

% The procedure that will initialize the server, 
% takes a port number (for example 8080), opens a 
% listening socket and passes the socket to handler/1. 
% Once the request has been handled the socket will 
% be closed.
init(Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of
        {ok, Listen} ->
            io:format("Listening! Listen: ~p~n", [Listen]),
            handler(Listen),
            gen_tcp:close(Listen),
            ok;
        {error, Error} ->
            error
    end.

% Will listen to the socket for an incoming connection. 
% Once a client has connect it will pass the connection 
% to request/1. When the request is handled the connection 
% is closed.
handler(Listen) ->
    case gen_tcp:accept(Listen) of  % how we accept an incoming request. If it succeeds we will have a communication channel open to the client.
        {ok, Client} ->
            request(Client),
            handler(Listen);
        {error, Error} ->
            error
    end.

% Will read the request from the client connection and 
% parse it. It will then parse the request using your http 
% parser and pass the request to reply/1. The reply is 
% then sent back to the client.
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

% Where we decide what to reply, how to turn the reply 
% into a well formed HTTP reply.
reply({{get, URI, _}, _, _}) ->
    timer:sleep(40),
    http:ok("A random response!").
