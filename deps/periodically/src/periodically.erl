-module(periodically).
-author("Brian Picciano").
-compile(export_all).
-behavior(gen_server).

%internal                      
-export([init/1,handle_call/3,handle_cast/2,
         handle_info/2,terminate/2,code_change/3]).

-record(state,{period,function}).
-define(LOOP_TIMEOUT,60000).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sample code:
%
sample_code() ->
    periodically:start({local,thing1},3000,fun() -> io:fwrite("thing1 says hi!\n") end),
    periodically:start({local,thing2},2000,{periodically,thing2_test,[]}).
thing2_test([]) -> io:fwrite("thing2 says hi!\n").
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Name,Period,Fun) ->
    gen_server:start(Name,?MODULE,#state{period=Period,function=Fun},[]).

start_link(Name,Period,Fun) ->
    gen_server:start_link(Name,?MODULE,#state{period=Period,function=Fun},[]).
    
init(S) ->
    erlang:send_after(S#state.period,self(),go),
    {ok,S,?LOOP_TIMEOUT}.      
    
handle_info(go,S) ->           
    case S#state.function of
    {Module,Method,Args} -> Module:Method(Args);
    Fun -> Fun()
    end,
    erlang:send_after(S#state.period,self(),go),
    {noreply,S,?LOOP_TIMEOUT}; 
  
handle_info(timeout,S) ->      
    {noreply,S,?LOOP_TIMEOUT}; 
handle_info(Wut,S) ->          
    error_logger:error_msg("periodically got info ~w\n",[Wut]),
    {noreply,S,?LOOP_TIMEOUT}. 

handle_cast(Wut,S) ->          
    error_logger:error_msg("periodically got cast ~w\n",[Wut]),
    {noreply,S,?LOOP_TIMEOUT}.

handle_call(Wut,From,S) ->
    error_logger:error_msg("periodically got call ~w from ~w\n",[Wut,From]),
    {noreply,S,?LOOP_TIMEOUT}.

%Nothing to see here, move along
code_change(_,State,_) ->
    {ok,State,?LOOP_TIMEOUT}.
terminate(_,_) -> oh_noooo.
