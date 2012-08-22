-module(nopt).
-author('Brian Picciano').
-include("nemo.hrl").
-compile(export_all).
-behavior(gen_server).

%internal
-export([init/1,handle_call/3,handle_cast/2,
         handle_info/2,terminate/2,code_change/3]).

-define(LOOP_TIMEOUT,60000).
-record(state,{options=[]}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Access global options in the server
global_getOpt(Key) -> gen_server:call(nemo_nopt,{getopt,Key}).
global_setOpt(Key,Value) -> gen_server:cast(nemo_nopt,{setopt,Key,Value}).

%Gets the value of Opt from the Opts structure, returns false if it's not there
getOpt(Opts,Opt) ->
    case lists:keyfind(Opt,1,Opts) of
    {Opt,Res} -> Res;
    false -> false
    end.

%Sets/resets the Val of Opt in the Opts structure
setOpt(Opts,Opt,Val) -> lists:keystore(Opt,1,Opts,{Opt,Val}).   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% NOpt server
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() -> gen_server:start_link({local,nemo_nopt},?MODULE,'_',[]).
init(_) -> 
    case file:script("config") of
    {ok, Options} -> {ok,#state{options=Options},?LOOP_TIMEOUT};
    {error,E}     -> error_logger:error_msg("Couldn't parse config: ~p\n",[E]),{stop, E}
    end.

handle_call({getopt,Key},_,S) ->
    #state{options=Opts} = S,
    {Key,Value} = lists:keyfind(Key,1,Opts),
    {reply,Value,S,?LOOP_TIMEOUT};
handle_call(Wut,From,S) ->
    error_logger:error_msg("nopt got call ~w from ~w\n",[Wut,From]),
    {noreply,S,?LOOP_TIMEOUT}.

handle_cast({setopt,Key,Value},S) ->
    #state{options=Opts} = S,
    NewOpts = lists:keystore(Key,1,Opts,{Key,Value}),
    {noreply,S#state{options=NewOpts},?LOOP_TIMEOUT};
handle_cast(Wut,S) ->
    error_logger:error_msg("nopt got cast ~w\n",[Wut]),
    {noreply,S,?LOOP_TIMEOUT}.

handle_info(timeout,S) -> {noreply,S,?LOOP_TIMEOUT};
handle_info(Wut,S) ->
    error_logger:error_msg("nopt got info ~w\n",[Wut]),
    {noreply,S,?LOOP_TIMEOUT}.

%Nothing to see here, move along
code_change(_,State,_) ->
    {ok,State}.
terminate(_,_) -> put_me_down.
