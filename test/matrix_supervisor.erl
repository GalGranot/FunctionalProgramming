-module(matrix_supervisor).
-export([server_restarter/0]).

server_restarter() ->
    process_flag(trap_exit, true),
    ServerPid = spawn_link(matrix_server, server, []),
    register(matrix_server, ServerPid),
    receive
        {'EXIT', ServerPid, normal} ->
            unregister(matrix_server), ok;
        {'EXIT', ServerPid, _} -> % server crashed, restart
            flush_mailbox(), 
            server_restarter()
    end.

flush_mailbox() -> % clean mailbox after restarting to not handle old requests
    receive
        _ -> flush_mailbox()
    after 0  -> ok
    end.