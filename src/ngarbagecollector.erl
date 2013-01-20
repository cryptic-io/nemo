-module(ngarbagecollector).
-author("Brian Picciano").
-include("nemo.hrl").
-compile(export_all).

-define(CLEANUPS,[
    {filekey,       fun(R) ->
                        nutil:timestamp() - R#filekey.ts =< ?FILEKEY_TTL
                    end},
    {file,          fun(R) ->
                        SpareFile = nfile:exists(R#file.filename)
                            andalso
                        case R#file.status of
                        {todelete,TS} -> nutil:timestamp() - TS =< ?FILE_TODELETE_TTL;
                        _ -> true
                        end,
                        if not SpareFile -> nfile:delete(R#file.filename); true -> ok end,
                        SpareFile
                    end}
]).


start() ->
    periodically:start_link({local,nemo_ngarbagecollector},60000,{?MODULE,loop,start}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Loops through each cleanup parameter and runs table_loop on it. If
%Table is a list of tables, run table_loop on each one
loop(start) -> ?MODULE:loop(?CLEANUPS);
loop([]) -> ok;
loop([{Table,Fun}|Cleanups]) ->
    case is_list(Table) of
    true  -> [?MODULE:table_loop(T,Fun) || T <- Table];
    false ->  ?MODULE:table_loop(Table,Fun)
    end,
    ?MODULE:loop(Cleanups).

%Checks Fun on each record in table, deletes any which don't return true
table_loop(Table,Fun)   -> ?MODULE:table_loop(Table,Fun,ndb:first(Table)).
table_loop(_,_,empty)   -> ok;
table_loop(Table,Fun,[F|_] = S) ->
    Next = ndb:next(Table,F),
    lists:foreach(fun(R) -> not Fun(R) andalso ndb:delete_object(Table,R) end,S),
    ?MODULE:table_loop(Table,Fun,Next);
table_loop(_,_,[]) -> done;
table_loop(Table,Fun,R) ->
    ?MODULE:table_loop(Table,Fun,[R]).
