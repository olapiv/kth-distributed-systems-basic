-module(save_data).
-export([save_2d_data_to_file/2]).

save_2d_data_to_file(Path2File, DataList) ->
    LengthList = lists:flatlength(DataList),
    WriteFile = file:open(Path2File, [write]),
    case WriteFile of
        {ok, File} ->
            % Maybe write title here.
            write_datapoints_to_file(0, LengthList, File, DataList)
    end.


write_datapoints_to_file(N, LengthList, File, DataList) ->
    if N == LengthList ->
        io:format(File, "~n~n~n"),
        ok;
    true ->
        {X, Y} = lists:nth(N+1, DataList),
        io:format(File, "~p ~p~n", [X, Y]),
        write_datapoints_to_file(N+1, LengthList, File, DataList)
    end.


% DataExample = [{2, 3}, {3, 4}, {4, 5}],
% save_2d_data_to_file("foo.data", DataExample),