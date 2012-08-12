-module(gen_pool_child).
-author('Brian Picciano').
-behavior(gen_server).
-compile(export_all).

%internal
-export([init/1,handle_call/3,handle_cast/2,
         handle_info/2,terminate/2,code_change/3]).

-record(state, {
    module,
    childstate
}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server exports
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Module,Args) -> gen_server:start(?MODULE,{Module,Args},[]).
start_link(Module,Args) -> gen_server:start_link(?MODULE,{Module,Args},[]).

init({Module,Args}) ->
    case Module:child_init(Args) of
    {ok, S} -> {ok,#state{module=Module,childstate=S}};
    {ok,S,T} -> {ok,#state{module=Module,childstate=S},T}
    end.

handle_cast({'$forward',From,ToForward},S) ->
    ?MODULE:handle_call(ToForward,From,S);
handle_cast({'$forward',ToForward},S) ->
    ?MODULE:handle_cast(ToForward,S);
handle_cast(Message,S) ->
    #state{module=Module,childstate=CS} = S,
    case Module:child_handle_cast(Message,CS) of
    {noreply,NewCState} ->
        {noreply,S#state{childstate=NewCState}};
    {noreply,NewCState,Timeout} ->
        {noreply,S#state{childstate=NewCState},Timeout};
    {stop,Reason,NewCState} ->
        {stop,Reason,S#state{childstate=NewCState}}
    end.

handle_call(Message,From,S) ->
    #state{module=Module,childstate=CS} = S,
    case Module:child_handle_call(Message,From,CS) of
    {reply,Reply,NewCState} ->
        gen_server:reply(From,Reply),
        {noreply,S#state{childstate=NewCState}};
    {reply,Reply,NewCState,Timeout} ->
        gen_server:reply(From,Reply),
        {noreply,S#state{childstate=NewCState},Timeout};
    {noreply,NewCState} ->
        {noreply,S#state{childstate=NewCState}};
    {noreply,NewCState,Timeout} ->
        {noreply,S#state{childstate=NewCState},Timeout};
    {stop,Reason,NewCState} ->
        {stop,Reason,S#state{childstate=NewCState}}
    end.

handle_info(Message,S) ->
    #state{module=Module,childstate=CS} = S,
    case Module:child_handle_info(Message,CS) of
    {noreply,NewCState} ->
        {noreply,S#state{childstate=NewCState}};
    {noreply,NewCState,Timeout} ->
        {noreply,S#state{childstate=NewCState},Timeout};
    {stop,Reason,NewCState} ->
        {stop,Reason,S#state{childstate=NewCState}}
    end.

code_change(OldVsn,S,Extra) ->
    #state{module=Module,childstate=CState} = S,
    case Module:child_code_change(OldVsn,CState,Extra) of
    {error,R} -> {error,R};
    {ok,NewCState} -> {ok,S#state{childstate=NewCState}}
    end.

terminate(R,S) -> 
    #state{module=Module,childstate=CState} = S,
    Module:child_terminate(R,CState).

