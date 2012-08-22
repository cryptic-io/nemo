-module(gen_pool).
-author('Brian Picciano').
-behavior(gen_server).
-compile(export_all).

%internal
-export([init/1,handle_call/3,handle_cast/2,
         handle_info/2,terminate/2,code_change/3]).

-export([behavior_info/1,start/4,start_link/4]).

-record(state, {
    module,
    children,
    children_full,
    children_args,
    parentstate
}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Behavior definition
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

behavior_info(callbacks) -> [
    {init,1}, 
    {handle_call,3}, 
    {handle_cast,2},
    {handle_info,2},
    {terminate,2},
    {code_change,3},
    {child_init,1},
    {child_handle_call,3},
    {child_handle_cast,2},
    {child_handle_info,2},
    {child_terminate,2},
    {child_code_change,3}
];
behavior_info(_) -> undefined.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Module,Args,Opts) -> 
    gen_server:start(?MODULE,{Module,Args},Opts).
start_link(Module,Args,Opts) -> 
    gen_server:start_link(?MODULE,{Module,Args},Opts).

start(Name,Module,Args,Opts) -> 
    gen_server:start(Name,?MODULE,{Module,Args},Opts).
start_link(Name,Module,Args,Opts) -> 
    gen_server:start_link(Name,?MODULE,{Module,Args},Opts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server exports
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init({Module,Args}) ->
	process_flag(trap_exit,true),
    InitRet = Module:init(Args),
    {ok,{NumChildren,ChildArgs},ParentState,Timeout} = ?MODULE:handle_init_ret( InitRet ),
    Children = ?MODULE:spawn_children(Module,NumChildren,ChildArgs),
    {
        ok,
        #state{module=Module,children=Children,children_full=Children,parentstate=ParentState,children_args=ChildArgs},
        Timeout
    }.

handle_call(Call,From,S) ->
    case S of
    #state{children=[]} -> 
        ?MODULE:handle_call(Call,From,S#state{children=S#state.children_full});
    #state{module=Module,children=[C|Rest],parentstate=PState} ->
        case Module:handle_call(Call,From,PState) of
        {forward,ToForward,NewPState} ->
            gen_server:cast(C,{'$forward',From,ToForward}),
            {noreply,S#state{children=Rest,parentstate=NewPState}};
        {forward,ToForward,NewPState,Timeout} ->
            gen_server:cast(C,{'$forward',From,ToForward}),
            {noreply,S#state{children=Rest,parentstate=NewPState},Timeout};
        {reply,Reply,NewPState} ->
            {reply,Reply,S#state{parentstate=NewPState}};
        {reply,Reply,NewPState,Timeout} ->
            {reply,Reply,S#state{parentstate=NewPState},Timeout};
        {noreply,NewPState} ->
            {noreply,S#state{parentstate=NewPState}};
        {noreply,NewPState,Timeout} ->
            {noreply,S#state{parentstate=NewPState},Timeout};
        {stop,Reason,NewPState} ->
            {stop,Reason,S#state{parentstate=NewPState}}
        end
    end.

handle_cast(Cast,S) ->
    case S of
    #state{children=[]} -> 
        ?MODULE:handle_cast(Cast,S#state{children=S#state.children_full});
    #state{module=Module,children=[C|Rest],parentstate=PState} ->
        case Module:handle_cast(Cast,PState) of
        {forward,ToForward,NewPState} ->
            gen_server:cast(C,{'$forward',ToForward}),
            {noreply,S#state{children=Rest,parentstate=NewPState}};
        {forward,ToForward,NewPState,Timeout} ->
            gen_server:cast(C,{'$forward',ToForward}),
            {noreply,S#state{children=Rest,parentstate=NewPState},Timeout};
        {noreply,NewPState} ->
            {noreply,S#state{parentstate=NewPState}};
        {noreply,NewPState,Timeout} ->
            {noreply,S#state{parentstate=NewPState},Timeout};
        {stop,Reason,NewPState} ->
            {stop,Reason,S#state{parentstate=NewPState}}
        end
    end.

handle_info({'EXIT',DeadPid,Reason},S) ->
    case lists:member(DeadPid,S#state.children_full) of
    true ->
        #state{module=Module,children_args=ChildArgs} = S,
        NewChildren = [
            ?MODULE:spawn_child(Module,ChildArgs) |
            lists:filter(fun(X) -> X /= DeadPid end,S#state.children_full)
        ],
        {noreply,S#state{children=NewChildren,children_full=NewChildren}};
    false ->
        {stop,Reason,S}
    end;

handle_info(Info,S) ->
    case S of
    #state{children=[]} -> 
        ?MODULE:handle_info(Info,S#state{children=S#state.children_full});
    #state{module=Module,parentstate=PState} ->
        case Module:handle_info(Info,PState) of
        {noreply,NewPState} ->
            {noreply,S#state{parentstate=NewPState}};
        {noreply,NewPState,Timeout} ->
            {noreply,S#state{parentstate=NewPState},Timeout};
        {stop,Reason,NewPState} ->
            {stop,Reason,S#state{parentstate=NewPState}}
        end
    end.

code_change(OldVsn,S,Extra) ->
    #state{module=Module,parentstate=PState} = S,
    case Module:code_change(OldVsn,PState,Extra) of
    {error,R} -> {error,R};
    {ok,NewPState} -> {ok,S#state{parentstate=NewPState}}
    end.

terminate(R,S) ->
    #state{module=Module,parentstate=PState} = S,
    Module:terminate(R,PState).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% util
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_init_ret({ok,ChildOpts,ParentState}) ->
    {ok,?MODULE:handle_childopts(ChildOpts),ParentState,infinity};
handle_init_ret({ok,ChildOpts,ParentState,Timeout}) ->
    {ok,?MODULE:handle_childopts(ChildOpts),ParentState,Timeout}.

handle_childopts({NumChildren,ChildArgs}) -> {NumChildren,ChildArgs}.

spawn_children(Module,NumChildren,ChildArgs) ->
    lists:map(
        fun(_) -> ?MODULE:spawn_child(Module,ChildArgs) end,
        lists:seq(0,NumChildren-1)
    ).

spawn_child(Module,ChildArgs) ->
    {ok,Pid} = gen_pool_child:start_link(Module,ChildArgs),
    Pid.

