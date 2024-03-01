-module(matrix_supervisor).
-import(matrix_server,[serverLoop/1]).
-export([spawnServer/0]).

spawnServer() ->
    process_flag(trap_exit, true),
    ServerPid = spawn_link(fun() -> matrix_server:serverLoop(1) end),
    register(matrix_server, ServerPid),
    io:format("Hello from supervisor!~n"),
    supervisorLoop(ServerPid).

supervisorLoop(ServerPid) ->
    receive
        {'EXIT', ServerPid, normal} -> ok;
        {'EXIT', ServerPid, _} ->
            flushInbox(),
            supervisorLoop(ServerPid)
    end.

flushInbox() ->
    receive
        _ -> flushInbox()
    after 0 -> ok
    end.