-module(nnodemon).
-author('Brian Picciano').
-include("nemo.hrl").
-compile(export_all).
-behavior(gen_server).

%internal
-export([init/1,handle_call/3,handle_cast/2,
         handle_info/2,terminate/2,code_change/3]).

-define(LOOP_TIMEOUT,60000).

start() -> gen_server:start_link({local,nemo_nnodemon},?MODULE,'_',[]).

apply_priority_range(Node,NodeToApply,Priority,Range) ->
    gen_server:cast({nemo_nnodemon,Node},{apply_priority_range,NodeToApply,Priority,Range}).

remove_from_pool(Node,NodeToRemove) ->
    gen_server:cast({nemo_nnodemon,Node},{remove_from_pool,NodeToRemove}).

node_summary(Node) ->
    gen_server:call({nemo_nnodemon,Node},node_summary).

broadcast_apply(Node,Priority,Range) ->
    gen_server:cast({nemo_nnodemon,node()},{broadcast_apply,Node,Priority,Range}).

broadcast_remove(Node) ->
    gen_server:cast({nemo_nnodemon,node()},{broadcast_remove,Node}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_) -> 

    ndistribute:init_nodedist(),
    case nutil:nnodes_sans_me() of
    [] -> oh_well;
    [Node|_] -> %TODO Should probably do some checking to make sure all node's summaries are correct
        Summary = ?MODULE:node_summary(Node),
        lists:foreach(
            fun({N,P,R}) -> ndistribute:add_to_nodedists(N,P,R) end,
            Summary
        )
    end,

    {ok,'_',?LOOP_TIMEOUT}.

handle_call(node_summary,_,S) ->
    {reply,ndistribute:nodedists_summary(),S,?LOOP_TIMEOUT};

handle_call(Wut,From,S) ->
    error_logger:error_msg("nnodemon got call ~w from ~w\n",[Wut,From]),
    {noreply,S,?LOOP_TIMEOUT}.

handle_cast({apply_priority_range,N,P,R},S) ->
    ndistribute:remove_from_nodedists(N),
    ndistribute:add_to_nodedists(N,P,R),
    {noreply,S,?LOOP_TIMEOUT};

handle_cast({remove_from_pool,N},S) ->
    ndistribute:remove_from_nodedists(N),
    {noreply,S,?LOOP_TIMEOUT};

handle_cast({broadcast_apply,N,P,R},S) ->
    lists:foreach(
        fun(Node) -> ?MODULE:apply_priority_range(Node,N,P,R) end,
        nutil:nnodes_sans_me()
    ),
    ?MODULE:handle_cast({apply_priority_range,N,P,R},S);

handle_cast({broadcast_remove,N},S) ->
    lists:foreach(
        fun(Node) -> ?MODULE:remove_from_pool(Node,N) end,
        nutil:nnodes_sans_me()
    ),
    ?MODULE:handle_cast({remove_from_pool,N},S);

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
