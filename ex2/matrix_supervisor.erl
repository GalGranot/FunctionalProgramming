-module(matrix_supervisor).
-import(matrix_server,[server_loop/1]).
-export([start_link/0]).

start_link() ->
    io:format("Hello, Supervisor!~n"),
    process_flag(trap_exit, true),
    Pid = spawn_link(fun() -> server_loop([]) end),
    register(matrix_server, Pid),
    PidString = erlang:pid_to_list(Pid),
    io:format("Hello, Supervisor2! ~s!~n", [PidString]),
    supervisor_loop(Pid).

supervisor_loop(Pid) ->
    receive
        {'EXIT', Pid, normal} -> ok;
        {'EXIT', Pid, _} ->
           unregister(matrix_server),
           exit(normal) 
    end.
