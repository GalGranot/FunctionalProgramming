-module(matrix_server).
-import(matrix,[getRow/2, getCol/2]).
-import(matrix_supervisor, [start_link/0]).
-export([start_server/0, shutdown/0, mult/2, get_version/0, explanation/0, server_loop/1]).


start_server() ->
    spawn(matrix_supervisor,start_link,[]),
    io:format("Hello, SERVER!~n"),
    ok.


server_loop(Version) ->
    receive
        {From, MsgRef, {multiple, Mat1, Mat2}} ->
            % Spawn a new process to perform matrix multiplication
            spawn(fun() -> handle_multiplication(From, MsgRef, Mat1, Mat2) end),
            % Continue listening for messages
            server_loop(Version);
        shutdown ->
            unregister(matrix_server),
            ok;  % Handle shutdown message
        {From, MsgRef, get_version} ->
            io:format("Hello, GET VERSION!~n"),
            From ! {self(), MsgRef, Version},
            server_loop(Version);
        {From, MsgRef, sw_upgrade} ->
            io:format("Hi!~n"),
            io:format("newVersion, ~w!~n", [From]),
            handle_sw_upgrade(),
            From ! {self(), MsgRef, {ok, "Code upgraded"}},
            %Version,
            NewVersion = Version + 1,
            %io:format("Lot, ~w!~n", [NewVersion]),
            server_loop(NewVersion);
        _ ->
            % Handle unrecognized messages
            server_loop(Version)
    end.

shutdown() ->
    matrix_server ! shutdown.

mult(Mat1, Mat2) ->
    self() ! {self(), {multiple, Mat1, Mat2}},
    receive
        {_, _, {ok, Result}} ->
            Result
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
    {ok, "To answer"}.

handle_sw_upgrade() ->
    code:purge(matrix_server),
    code:load_file(matrix_server).

handle_multiplication(From, MsgRef, Mat1, Mat2) ->
%fill code of multiply matrixes
