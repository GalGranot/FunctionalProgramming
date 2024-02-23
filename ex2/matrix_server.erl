-module(matrix_server).
-import(matrix,[getRow/2, getCol/2]).
-import(matrix_supervisor, [start_link/0]).
-export([start_server/0, shutdown/0, mult/2, get_version/0, explanation/0, server_loop/0, multiplier/4, vec_mul/5]).


start_server() ->
    spawn(matrix_supervisor,start_link,[]),
    io:format("Hello, SERVER!~n"),
    ok.


server_loop() ->
    Version = version_1,
    receive
        {Pid, MsgRef, {multiple, Mat1, Mat2}}->
			spawn(?MODULE,multiplier,[Pid,MsgRef,Mat1,Mat2]),
            server_loop();
        shutdown ->
            unregister(matrix_server),
            ok;  % Handle shutdown message
        {From, MsgRef, get_version} ->
            io:format("Hello, GET VERSION!~n"),
            From ! {self(), MsgRef, Version},
            server_loop();
        sw_upgrade ->
            io:format("Hi!~n"),
            io:format("newVersion~n"),
            %the MODULE will shift us to another code version and there, the Version will have different value than version_1
            ?MODULE:server_loop(); 
        _ ->
            % Handle unrecognized messages
            server_loop()
    end.

shutdown() ->
    matrix_server ! shutdown.

mult(Mat1,Mat2)->
	MsgRef = make_ref(),
	matrix_server ! {self(), MsgRef, {multiple, Mat1, Mat2}},
	receive 
		{MsgRef, Mat} -> Mat
	end.

get_version() ->
    io:format("Hello, GET VERSION2!~n"),
    MsgRef = "TRY",
    matrix_server ! {self(), MsgRef, get_version},
    io:format("Hello, GET VERSION3!~n"),
    receive
        {_, _, Version} ->
            Version
    end.

explanation() ->
    {ok, "To ensure the supervisor remains active even after handling multiple updates.
        In addition, Separating the supervisor module from the server module allows better fault tolerance and modularity in the system design."}.

multiplier(Pid,MsgRef,Mat1,Mat2)->
	{N,_K,M} = matrix:getSizes(Mat1,Mat2),%M1: N*K  M2: K*M
	Mat_ans = matrix:getZeroMat(N,M),
	[spawn(?MODULE, vec_mul, 
		[matrix:getRow(Mat1,Row_num), matrix:getCol(Mat2,Col_num),
		Row_num, Col_num,self()]) || 
		Row_num <- lists:seq(1,N) ,
		Col_num <- lists:seq(1,M) ],
	wait_for_all(0,N*M,Mat_ans,Pid, MsgRef).
	
wait_for_all(I,Max, Mat_ans, Pid, MsgRef)->
	receive
		{Val, Row_num, Col_num} when (I=/=(Max-1)) -> %not the last answer
				Mat_new = matrix:setElementMat(Row_num,Col_num,Mat_ans,Val),
				wait_for_all(I+1,Max, Mat_new, Pid, MsgRef);
		{Val, Row_num, Col_num} -> %last answer
				Pid ! {MsgRef,matrix:setElementMat(Row_num,Col_num,Mat_ans,Val)}
	end.
	
vec_mul(Row, Col,Row_num,Col_num,Pid)->
		Val = lists:sum([ X * Y || {X, Y} <- lists:zip(tuple_to_list(Row), tuple_to_list(Col)) ]),
		Pid ! {Val,Row_num,Col_num}.

