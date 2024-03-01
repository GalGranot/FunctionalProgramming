-module(matrix_server).
-import(matrix, [getZeroMat/2, getRow/2, getCol/2, setElementMat/4, getSizes/2]).
-export([mult/2, start_server/0, serverLoop/1, shutdown/0, explanation/0]).

scalarProduct(V1, V2) ->
    MultiplePairs = lists:zip(V1, V2),
    MultiplesList = [X * Y || {X, Y} <- MultiplePairs],
    lists:sum(MultiplesList).

vectorMultiply(V1, V2, RowNum, ColNum, ServerPid) ->
    Result = scalarProduct(tuple_to_list(V1), tuple_to_list(V2)),
    ServerPid ! {Result, RowNum, ColNum}.

mult(Mat1, Mat2, MsgRef, Pid) ->
    io:format("Here1~n"),
    {RowsNum, _, ColsNum} = getSizes(Mat1, Mat2),
    io:format("Here2~n"),
    MatResult = matrix:getZeroMat(RowsNum, ColsNum),
    io:format("Here3~n"),
    [spawn(?MODULE, vectorMultiply, % args for multiplication
    [getRow(Mat1, RowIndex), getCol(Mat2, ColIndex), RowIndex, ColIndex, self()])
    || RowIndex <- lists:seq(1, RowsNum), ColIndex <- lists:seq(1, ColsNum)],
    io:format("Here4~n"),
    JobsToDo = RowsNum * ColsNum,
    io:format("Here5~n"),
    gatherElements(MatResult, 0, JobsToDo, Pid, MsgRef).

gatherElements(MatResult, JobsDone, JobsMax, Pid, MsgRef) ->
    receive
        {Result, RowNum, ColNum} when JobsDone < JobsMax ->
            MatResult = matrix:setElementMat(RowNum, ColNum, MatResult, Result),
            gatherElements(MatResult, JobsDone + 1, JobsMax, Pid, MsgRef);
        {Result, RowNum, ColNum} ->
            MatResult = matrix:setElementMat(RowNum, ColNum, MatResult, Result),
            Pid ! {result, MsgRef, MatResult}
    end.

mult(Mat1, Mat2) ->
    MsgRef = make_ref(),
    matrix_server ! {self(), MsgRef, {multiple, Mat1, Mat2}},
    receive
        {result, MsgRef, Mat} -> Mat
    end.

start_server() ->
    spawn(matrix_supervisor, spawnServer, []),
    io:format("Hello from start_server~n").

shutdown() ->
    matrix_server ! shutdown.

get_version() ->
    MsgRef = make_ref(),
    matrix_server ! {self(), MsgRef, get_version},
    receive
        {MsgRef, Version} -> Version
    end.

serverLoop(VersionNum) ->
    Version = "version_" ++ VersionNum,
    % io:format(Version),
    % io:format("Hello from serverLoop, version ~s~n", [integer_to_list(VersionNum)]),
    receive
        shutdown ->
            unregister(matrix_server),
            ok;
        sw_upgrade -> 
            ?MODULE:serverLoop(VersionNum + 1);
        {Pid, MsgRef, {multiple, Mat1, Mat2}} ->
            io:format("Received multiplying command~n"),
            spawn(?MODULE, mult, [Mat1, Mat2, MsgRef, Pid]), %%problematic line
            io:format("Created proccess for mult"),
            serverLoop(Version);
        {Pid, MsgRef, get_version} ->
            Pid ! {MsgRef, Version},
            serverLoop(Version);
        _ ->
            io:format("Unknown message format, ignoring~n"),
            serverLoop(Version)
    end.

explanation() ->
    {ok, "To ensure the supervisor remains active even after handling multiple updates.
        In addition, Separating the supervisor module from the server module allows better fault tolerance and modularity in the system design."}.