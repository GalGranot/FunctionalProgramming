-module(q2).
-compile(export_all).

% details for each process:
% pid, name, function

start_supervisor() ->
    Sup = spawn(?MODULE, supervisor_init, []),
    register(sup, Sup).

supervisor_init() ->
    process_flag(trap_exit, true),
    sup_loop(dict:new()).

add_process(F, Name) -> sup ! {add, F, Name}.

remove_process(Name) -> sup ! {rmv, Name}.

sup_loop(Dict) ->
    receive
        {add, F, Name} ->
            Pid = spawn(?MODULE, F, []),
            dict:append(Pid, {Name, Pid, F}, Dict),
            sup_loop(Dict);
        {rmv, Name} ->
            {_Name, Pid, _F} = dict:fetch(Pid, Dict),
            exit(Pid, removed),
            sup_loop(Dict);
        {'EXIT', Pid, normal} ->
            dict:erase(Pid, Dict),
            sup_loop(Dict);
        {'EXIT', Pid, removed} -> 
            dict:erase(Pid, Dict),
            sup_loop(Dict);
        {'EXIT', Pid, _} -> %process crashed, restart all
            sup_loop(restart_all(Dict))
    end.

restart_all(Dict) ->
    Keys = dict:fetch_keys(Dict),
    restart_all(Keys, Dict, dict:new()).

restart_all([], _Dict, NewDict) -> NewDict;

restart_all([Key | T], Dict, NewDict) ->
    Value = dict:fetch(Key),
    {Name, _Pid, F} = Value,
    Pid = spawn(?MODULE, F, []),
    restart_all(T, Dict, dict:append({Name, Pid, F}, NewDict)).
