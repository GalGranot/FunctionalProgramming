-module(matrix_server).
-import(matrix, [getZeroMat/2, getRow/2, getCol/2, setElementMat/4, getSizes/2]).
-export([scalarProduct/2, mult/4, start_server/0, serverLoop/1]).

scalarProduct(V1, V2) ->
    MultiplePairs = lists:zip(V1, V2),
    MultiplesList = [X * Y || {X, Y} <- MultiplePairs],
    lists:sum(MultiplesList).

vectorMultiply(V1, V2, RowNum, ColNum, ServerPid) ->
    Result = scalarProduct(tuple_to_list(V1), tuple_to_list(V2)),
    ServerPid ! {Result, RowNum, ColNum}.

mult(Mat1, Mat2, MsgRef, Pid) ->
    {RowsNum, _, ColsNum} = getSizes(Mat1, Mat2),
    MatResult = matrix:getZeroMat(RowsNum, ColsNum),
    [spawn(?MODULE, vectorMultiply, % args for multiplication
    [getRow(Mat1, RowIndex), getCol(Mat2, ColIndex), RowIndex, ColIndex, self()])
    || RowIndex <- lists:seq(1, RowsNum), ColIndex <- lists:seq(1, ColsNum)],
    JobsToDo = RowsNum * ColsNum,
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

% mult(Mat1, Mat2) ->
%     MsgRef = make_ref(),
%     matrix_server ! {self(), MsgRef, {multiple, Mat1, Mat2}},
%     receive
%         {result, MsgRef, Mat} -> Mat;
%         _ -> io:format("Unknown result format, aborting~n")
%     end.

start_server() ->
    spawn(matrix_supervisor, spawnServer, []),
    io:format("Starting server~n"),
    ok.

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
    io:format("Hello from serverLoop~n"),
    receive
        shutdown ->
            unregister(matrix_server),
            ok;
        sw_upgrade -> 
            ?MODULE:serverLoop(VersionNum + 1);
        {Pid, MsgRef, {mutliple, Mat1, Mat2}} ->
            spawn(?MODULE, mult, [Mat1, Mat2, MsgRef, Pid]),
            serverLoop(Version);
        {Pid, MsgRef, get_version} ->
            Pid ! {MsgRef, Version},
            serverLoop(Version);
        _ ->
            io:format("Unknown message format, ignoring"),
            serverLoop(Version)
    end.

