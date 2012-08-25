-module(nfs).
-author('Brian Picciano').
-include("nemo.hrl").
-compile(export_all).
-behavior(gen_server).

%internal
-export([init/1,handle_call/3,handle_cast/2,
         handle_info/2,terminate/2,code_change/3]).

-define(LOOP_TIMEOUT,60000).

add_file(FileName) -> gen_server:call(nemo_nfs,{add_file,FileName}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() -> gen_server:start_link({local,nemo_nfs},?MODULE,'_',[]).
init(_) -> {ok,'_',?LOOP_TIMEOUT}.

handle_call({add_file,FileName},_From,S) ->
    Ret = 
        case nfile:size(FileName) of
        {error,E} -> {error,E};
        Size      -> 
            ndb:add_file(#file{filename=FileName,size=Size}),
            success
        end,

    {reply,Ret,S,?LOOP_TIMEOUT};

handle_call(Wut,From,S) ->
    error_logger:error_msg("nfs got call ~w from ~w\n",[Wut,From]),
    {noreply,S,?LOOP_TIMEOUT}.

handle_cast(Wut,S) ->
    error_logger:error_msg("nfs got cast ~w\n",[Wut]),
    {noreply,S,?LOOP_TIMEOUT}.

handle_info(Wut,S) ->
    error_logger:error_msg("nfs got info ~w\n",[Wut]),
    {noreply,S,?LOOP_TIMEOUT}.

%Nothing to see here, move along
code_change(_,State,_) -> {ok,State,?LOOP_TIMEOUT}.
terminate(_,_) -> oh_noooo.
