-module(matrix_supervisor).
-import(matrix_server,[server_loop/0]).
-export([start_link/0]).

start_link() ->
    process_flag(trap_exit, true),
    Pid = spawn_link(fun() -> server_loop() end),
    register(matrix_server, Pid),
    supervisor_loop(Pid).

supervisor_loop(Pid) ->
    receive
        {'EXIT', Pid, normal} -> ok;
        {'EXIT', Pid, _} ->
           unregister(matrix_server),
           exit(normal) 
    end.