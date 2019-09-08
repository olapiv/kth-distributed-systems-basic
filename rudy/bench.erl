-module(bench).
-export([benchStart/4]).

-import(erlang, [system_time/1]).
-import(http, [get/2]).
-import(gen_tcp, [connect/3]).
-import(gen_tcp, [close/1]).

% XVariablesList = [10, 20, 40, 80, 160, 320, 640, 1280].

benchStart(XVariablesList, FileName, Host, Port) ->
    DataListInitial = [],
    DataList = benchIterrate(DataListInitial, XVariablesList, FileName, Host, Port),
    save_data:save_2d_data_to_file(FileName, DataList).


benchIterrate(DataList, XVariablesList, FileName,Host, Port) ->
    LengthDataset = lists:flatlength(XVariablesList),
    if LengthDataset == 0 -> 
        DataList;
        true ->
            N = lists:nth(1, XVariablesList),
            TimeTaken = bench(N, Host, Port),
            TimeTakenSeconds = TimeTaken / 1000000,
            RequestsPerSecond = N / TimeTakenSeconds,
            DataListAppended = lists:append(DataList, [{N, RequestsPerSecond}]),
            benchIterrate(DataListAppended, lists:delete(N, XVariablesList), FileName,Host, Port)
    end.


bench(N, Host, Port) ->
    Start = erlang:system_time(micro_seconds),
    run(N, Host, Port),
    Finish = erlang:system_time(micro_seconds),
    Finish - Start.

run(N, Host, Port) ->
    if N == 0 -> ok;
    true ->
        request(Host, Port),
        run(N-1, Host, Port)
    end.

request(Host, Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    {ok, Server} = gen_tcp:connect(Host, Port, Opt),
    gen_tcp:send(Server, http:get("foo")),
    Recv = gen_tcp:recv(Server, 0),
    case Recv of
        {ok, _} ->
            ok
        ;
        {error, Error} ->
            io:format("test: error: ~w~n", [Error])
    end,
    gen_tcp:close(Server).