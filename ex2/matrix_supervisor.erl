-module(matrix_supervisor).
-import(matrix_server,[serverLoop/1]).
-export([spawnServer/0]).

spawnServer() ->
    io:format("Starting supervisor~n"),
    process_flag(trap_exit, true),
    Pid = spawn_link(matrix_server,serverLoop, [1]),
    register(matrix_server, Pid),
    PidString = erlang:pid_to_list(Pid),
    io:format("Hello from supervisor! ~s~n", [PidString]),
    supervisorLoop(Pid).

supervisorLoop(Pid) ->
    receive
        {'EXIT', Pid, normal} -> ok;
        {'EXIT', Pid, _} ->
           unregister(matrix_server),
           exit(normal) 
    end. 