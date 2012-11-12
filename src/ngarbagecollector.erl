-module(ngarbagecollector).
-author("Brian Picciano").
-include("nemo.hrl").
-compile(export_all).
-behavior(gen_server).

%internal
-export([init/1,handle_call/3,handle_cast/2,
         handle_info/2,terminate/2,code_change/3]).

-define(LOOP_TIMEOUT,60000).

-define(CLEANUPS,[
    {filekey,       fun(R) ->
                        nutil:timestamp() - R#filekey.ts =< ?FILEKEY_TTL
                    end},
    {file,          fun(R) ->
                        nfile:exists(R#file.filename) or (R#file.status /= whole)
                    end}
]).


start() ->
    gen_server:start_link({local,nemo_ngarbagecollector},?MODULE,'_',[]).

init(S) ->
    erlang:send_after(?LOOP_TIMEOUT,self(),go),
    {ok,S,?LOOP_TIMEOUT}.
  
handle_info(go,S) ->
    ?MODULE:loop(?CLEANUPS),
    erlang:send_after(?LOOP_TIMEOUT,self(),go),
    {noreply,S,?LOOP_TIMEOUT}; 

handle_info(timeout,S) ->
    {noreply,S,?LOOP_TIMEOUT};
handle_info(Wut,S) ->
    error_logger:error_msg("ngarbagecollector got info ~w\n",[Wut]),
    {noreply,S,?LOOP_TIMEOUT}.

handle_cast(Wut,S) ->
    error_logger:error_msg("ngarbagecollector got cast ~w\n",[Wut]),
    {noreply,S,?LOOP_TIMEOUT}.

handle_call(Wut,From,S) ->
    error_logger:error_msg("ngarbagecollector got call ~w from ~w\n",[Wut,From]),
    {noreply,S,?LOOP_TIMEOUT}.

%Nothing to see here, move along
code_change(_,State,_) ->
    {ok,State,?LOOP_TIMEOUT}.
terminate(_,_) -> oh_noooo.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Loops through each cleanup parameter and runs table_loop on it. If
%Table is a list of tables, run table_loop on each one
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
