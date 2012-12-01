-module(nfh).
-author("Brian Picciano").
-include("nemo.hrl").
-compile(export_all).
-behavior(gen_server).

%internal
-export([init/1,handle_call/3,handle_cast/2,
         handle_info/2,terminate/2,code_change/3]).

-define(LOOP_TIMEOUT,60000).
-record(state,{fh}).

open(FileName,Opts) ->
    gen_server:start(?MODULE,{nfile:full_path(FileName),Opts},[]).

read(Nfh,Amount) ->
    gen_server:call(Nfh,{read,Amount}).

close(Nfh) ->
    gen_server:call(Nfh,close).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init({FilePath,Opts}) -> 
    case file:open(FilePath,Opts) of
    {ok,FH}   -> {ok,#state{fh=FH},?LOOP_TIMEOUT};
    {error,E} -> {stop,E}
    end.

handle_call({read,Amount},From,S) ->
    {reply,file:read(S#state.fh,Amount),S,?LOOP_TIMEOUT};

handle_call(close,From,S) ->
    {reply,file:close(S#state.fh),S,?LOOP_TIMEOUT}.

handle_cast(Wut,S) ->
    error_logger:error_msg("nfh got cast ~w\n",[Wut]),
    {noreply,S,?LOOP_TIMEOUT}.

handle_info(timeout,S) ->
    {noreply,S,?LOOP_TIMEOUT};
handle_info(Wut,S) ->
    error_logger:error_msg("nfh got info ~w\n",[Wut]),
    {noreply,S,?LOOP_TIMEOUT}.

%Nothing to see here, move along
code_change(_,State,_) ->
    {ok,State,?LOOP_TIMEOUT}.
terminate(_,_) -> oh_noooo.
