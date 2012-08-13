-module(ngarbagecollector).
-author("Brian Picciano").
-include("nemo.hrl").
-compile(export_all).
-behavior(gen_server).

%internal
-export([init/1,handle_call/3,handle_cast/2,
         handle_info/2,terminate/2,code_change/3]).

-define(LOOP_TIMEOUT,60000).

start() -> gen_server:start_link({local,nemo_garbagecollector},?MODULE,'_',[]).
init(_) -> 
    erlang:send_after(?FILEKEY_TTL,self(),go),
    {ok,'_',?LOOP_TIMEOUT}.

handle_info(go,S) ->
    ?MODULE:loop(filekey),
    erlang:send_after(?FILEKEY_TTL,self(),go),
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

loop(Table) -> ?MODULE:loop(Table,ndb:first(Table)).
loop(_,empty) -> ok;
loop(Table,R) ->
    Next = ndb:next(Table,R#filekey.key),
    case nutil:timestamp() - R#filekey.ts > ?FILEKEY_TTL of
    true -> ndb:delete_key(R#filekey.key);
    false -> leave_it
    end,
    ?MODULE:loop(Table,Next).
