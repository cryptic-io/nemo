-module(nfilehunter).
-author("Brian Picciano").
-include("nemo.hrl").
-compile(export_all).
-behavior(gen_server).

%internal
-export([init/1,handle_call/3,handle_cast/2,
         handle_info/2,terminate/2,code_change/3]).

-define(LOOP_TIMEOUT,5000).

start() ->
    error_logger:info_msg("Getting initial list of files into mnesia\n"),
    ?MODULE:loop(),
    gen_server:start_link({local,nemo_nfilehunter},?MODULE,'_',[]).

init(S) ->
    erlang:send_after(?LOOP_TIMEOUT,self(),go),
    {ok,S,?LOOP_TIMEOUT}.
  
handle_info(go,S) ->
    ?MODULE:loop(),
    erlang:send_after(?LOOP_TIMEOUT,self(),go),
    {noreply,S,?LOOP_TIMEOUT}; 

handle_info(timeout,S) ->
    {noreply,S,?LOOP_TIMEOUT};
handle_info(Wut,S) ->
    error_logger:error_msg("nfilehunter got info ~w\n",[Wut]),
    {noreply,S,?LOOP_TIMEOUT}.

handle_cast(Wut,S) ->
    error_logger:error_msg("nfilehunter got cast ~w\n",[Wut]),
    {noreply,S,?LOOP_TIMEOUT}.

handle_call(Wut,From,S) ->
    error_logger:error_msg("nfilehunter got call ~w from ~w\n",[Wut,From]),
    {noreply,S,?LOOP_TIMEOUT}.

%Nothing to see here, move along
code_change(_,State,_) ->
    {ok,State,?LOOP_TIMEOUT}.
terminate(_,_) -> oh_noooo.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

loop() ->
    nfile:foreach_file(fun(Filename) -> 
        nfs:add_whole_file(Filename)
    end).
