-module(matrix_server).
-import(matrix,[getRow/2, getCol/2, getSizes/2]).
-import(matrix_supervisor, [start_link/0]).
-export([start_server/0, shutdown/0, mult/2, get_version/0, explanation/0, server_loop/0, scalarProduct/2, vectorMultiply/5, multiply/4, gatherElements/5]).


start_server() ->
    spawn(matrix_supervisor,start_link,[]),
    io:format("Hello, SERVER!~n"),
    ok.


server_loop() ->
    Version = version_1,
    receive
        {Pid, MsgRef, {multiple, Mat1, Mat2}}->
			spawn(fun() -> multiply(Mat1, Mat2, MsgRef, Pid) end), 
            server_loop();
        shutdown ->
            unregister(matrix_server),
            ok;  % Handle shutdown message
        {From, MsgRef, get_version} ->
            From ! {self(), MsgRef, Version},
            server_loop();
        sw_upgrade ->
            %the MODULE will shift us to another code version and there, the Version will have different value than version_1
            ?MODULE:server_loop(); 
        _ ->
            % Handle unrecognized messages
            server_loop()
    end.

shutdown() ->
    matrix_server ! shutdown.

mult(Mat1, Mat2) ->
    MsgRef = make_ref(),
    matrix_server ! {self(), MsgRef, {multiple, Mat1, Mat2}},
    receive
        {result, MsgRef, Mat} -> {MsgRef,Mat}
    end.

get_version() ->
    MsgRef = make_ref(),
    matrix_server ! {self(), MsgRef, get_version},
    receive
        {_, MsgRef, Version} ->
            {MsgRef,Version}
    end.

explanation() ->
    {ok, "To ensure the supervisor remains active even after handling multiple updates.
        In addition, Separating the supervisor module from the server module allows better fault tolerance and modularity in the system design."}.

scalarProduct(V1, V2) ->
    MultiplePairs = lists:zip(V1, V2),
    MultiplesList = [X * Y || {X, Y} <- MultiplePairs],
    lists:sum(MultiplesList).


vectorMultiply(V1, V2, RowNum, ColNum, ParentPid) ->
    Result = scalarProduct(tuple_to_list(V1), tuple_to_list(V2)),
    ParentPid ! {Result, RowNum, ColNum}.


multiply(Mat1, Mat2, MsgRef, Pid) ->
    {RowsNum, _, ColsNum} = getSizes(Mat1, Mat2),
    MatResult = matrix:getZeroMat(RowsNum, ColsNum),
    GatherElementsPid = spawn(?MODULE, gatherElements, [MatResult, 0, RowsNum * ColsNum, Pid, MsgRef]),
    [spawn(fun() -> vectorMultiply(getRow(Mat1, RowIndex), getCol(Mat2, ColIndex), RowIndex, ColIndex, GatherElementsPid) end)
    || RowIndex <- lists:seq(1, RowsNum), ColIndex <- lists:seq(1, ColsNum)].



gatherElements(MatResult, JobsDone, JobsMax, Pid, MsgRef) ->
    receive
        {Result, RowNum, ColNum} when JobsDone < JobsMax-1 ->
            Matnew = matrix:setElementMat(RowNum, ColNum, MatResult, Result),
            gatherElements(Matnew, JobsDone + 1, JobsMax, Pid, MsgRef);
        {Result, RowNum, ColNum} ->
            Matnew2 = matrix:setElementMat(RowNum, ColNum, MatResult, Result),
            Pid ! {result, MsgRef, Matnew2}
    end.