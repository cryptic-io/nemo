-module(ndev).
-author('Brian Picciano').
-include("nemo.hrl").
-compile(export_all).

reload_all() ->                
    ?MODULE:reload_all(?MODULE_LIST).
reload_all([]) -> ok;          
reload_all([T|C]) ->           
    io:fwrite("Purging ~w\n",[T]),  
    try_purge(T),              
    {module,T} = code:load_file(T), 
    ?MODULE:reload_all(C).     
  
try_purge(T) -> try_purge(T,1).
try_purge(T,Wait) ->           
    case code:soft_purge(T) of 
    true -> ok;
    false ->                   
        io:fwrite("* Waiting ~w seconds for ~w module\n",[Wait,T]),
        timer:sleep(Wait*1000),
        try_purge(T,Wait+1)
    end.

all_old_threads() ->
    [ old_threads(Module) || Module <- ?MODULE_LIST ].

old_threads(Module) ->
    OldThreads = lists:filter(fun(Pid) -> erlang:check_process_code(Pid,Module) end, erlang:processes()),
    {Module,length(OldThreads),OldThreads}.

