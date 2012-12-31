-module(nnodemon).
-author('Brian Picciano').
-include("nemo.hrl").
-compile(export_all).
-behavior(gen_server).

%internal
-export([init/1,handle_call/3,handle_cast/2,
         handle_info/2,terminate/2,code_change/3]).

-define(LOOP_TIMEOUT,60000).
-record(state,{priority,range}).

start(Priority,Range) -> gen_server:start_link({local,nemo_nnodemon},?MODULE,{Priority,Range},[]).

get_priority_range(Node) ->
    gen_server:call({nemo_nnodemon,Node},get_priority_range).

apply_priority_range(Node,Priority,Range) ->
    gen_server:cast({nemo_nnodemon,Node},{apply_priority_range,node(),Priority,Range}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init({Priority,Range}) -> 

    %Initialize nodedist table to blank, then add self
    ndistribute:init_nodedist(),
    ndistribute:add_to_nodedists(node(),Priority,Range),

    %Add all connected nodes, and tell them about us
    lists:foreach(
        fun(Node) ->
            {P,R} = ?MODULE:get_priority_range(Node),
            ndistribute:add_to_nodedists(Node,P,R),             %Add them to us
            ?MODULE:apply_priority_range(Node,Priority,Range)   %Add us to them
        end,
        nutil:nnodes_sans_me()
    ),
    
    {ok,#state{priority=Priority,range=Range},?LOOP_TIMEOUT}.

handle_call(get_priority_range,_,#state{priority=P,range=R} = S) ->
    {reply,{P,R},S,?LOOP_TIMEOUT};

handle_call(Wut,From,S) ->
    error_logger:error_msg("nnodemon got call ~w from ~w\n",[Wut,From]),
    {noreply,S,?LOOP_TIMEOUT}.

handle_cast({apply_priority_range,N,P,R},S) ->
    ndistribute:remove_from_nodedists(N),
    ndistribute:add_to_nodedists(N,P,R),
    {noreply,S,?LOOP_TIMEOUT};

handle_cast(Wut,S) ->
    error_logger:error_msg("nnodemon got cast ~w\n",[Wut]),
    {noreply,S,?LOOP_TIMEOUT}.

handle_info(timeout,S) ->
    {noreply,S,?LOOP_TIMEOUT};
handle_info(Wut,S) ->
    error_logger:error_msg("nnodemon got info ~w\n",[Wut]),
    {noreply,S,?LOOP_TIMEOUT}.

%Nothing to see here, move along
code_change(_,State,_) ->
    {ok,State,?LOOP_TIMEOUT}.
terminate(_,_) -> oh_noooo.
