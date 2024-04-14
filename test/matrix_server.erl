-module(matrix_server).
-import(matrix, [getSizes/2, getZeroMat/2, setElementMat/4, getRow/2, getCol/2]).
-export([start_server/0, shutdown/0, mult/2]).

start_server() ->
    spawn(matrix_supervisor, server_restarter, []),
    ok.

shutdown() ->
    matrix_server ! shutdown.

mult(Mat1, Mat2) ->
    Ref = make_ref(),
    matrix_server ! {self(), Ref, {multiple, Mat1, Mat2}},
    receive
        {Ref, Mat} -> Mat
    end.

get_version() ->
    Ref = make_ref(),
    matrix_server ! {self(), Ref, get_version},
    receive
        {Ref, Version} -> Version
    end.

server() ->
    Version = version_1,
    receive
        {Pid, MsgRef, {multiple, Mat1, Mat2}} ->
            spawn(multiply, [Mat1, Mat2, Pid, MsgRef]),
            server();
        shutdown ->
            ok;
        {Pid, MsgRef, get_version} ->
            Pid ! {MsgRef, Version},
            server();
        sw_upgrade ->
            ?MODULE:server();
        _ ->
            server()
    end.

scalarProduct(V1, V2) ->
    MultiplePairs = lists:zip(V1, V2),
    MultiplesList = [X * Y || {X, Y} <- MultiplePairs],
    lists:sum(MultiplesList).

vectorMultiply(V1, V2, RowNum, ColNum, GathererPid) ->
    Result = scalarProduct(tuple_to_list(V1), tuple_to_list(V2)),
    GathererPid ! {Result, RowNum, ColNum}.

multiply(Mat1, Mat2, Pid, MsgRef) ->
    {Rows, _, Cols} = getSizes(Mat1, Mat2),
    MatResult = getZeroMat(Rows, Cols),
    GathererPid = spawn(gatherResults, [MatResult, 0, Rows * Cols, Pid, MsgRef]),
    [spawn(vectorMultiply, [getRow(Mat1, RowNum), getCol(Mat2, ColNum), GathererPid])
    || RowNum <- lists:seq(1, Rows), ColNum <- lists:seq(1, Cols)].

gatherResults(MatResult, JobsDone, JobsMax, Pid, MsgRef) ->
    receive
        {Result, Row, Col} when JobsDone < JobsMax - 1 ->
            NewResult = setElementMat(Row, Col, MatResult, Result),
            gatherResults(NewResult, JobsDone + 1, JobsMax, Pid, MsgRef);
        {Result, Row, Col} ->
            FinalMat = setElementMat(Row, Col, MatResult, Result),
            Pid ! {MsgRef, FinalMat}
    end.