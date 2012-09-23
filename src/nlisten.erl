-module(nlisten).
-author('Brian Picciano').
-include("nemo.hrl").
-compile(export_all).

-define(LOOP_TIMEOUT,60000).
-define(CHILD_LOOP_TIMEOUT,1).
-define(TCP_OPTS, [binary, {packet, raw}, {nodelay, true}, {reuseaddr, true}, {active, false},{keepalive,true},{backlog,500},{send_timeout,60000},{send_timeout_close,true}]).

-record(child_state,{l,cbmodule,handler}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parent
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(CBModule,PortOpt,Handler) -> 
    gen_pool:start_link(?MODULE,{CBModule,PortOpt,Handler},[]).

init({CBModule,Port,Handler}) ->
	%Gets the listen socket, generates acceptor threads
	case gen_tcp:listen(Port, ?TCP_OPTS) of
	{ok, Listen} -> {ok,
                       {?NLISTEN_CHILDREN,
                        #child_state{l=Listen,
                                     cbmodule=CBModule,
                                     handler=Handler}},
                     Listen,
                     ?LOOP_TIMEOUT};
    E -> {stop,E}
	end.

%Shouldn't happen
handle_call(Wut,From,S) ->
   error_logger:error_msg("nlisten got call ~w from ~w\n",[Wut,From]),
   {noreply,S,?LOOP_TIMEOUT}.

%Shouldn't happen
handle_cast(Wut,S) ->
   error_logger:error_msg("nlisten got cast ~w\n",[Wut]),
   {noreply,S,?LOOP_TIMEOUT}.

%Shouldn't happen
handle_info(timeout,S) -> {noreply,S,?LOOP_TIMEOUT};
handle_info(Wut,S) ->
   error_logger:error_msg("nlisten got info ~w\n",[Wut]),
   {noreply,S,?LOOP_TIMEOUT}.

%Nothing here
code_change(_,State,_) ->
    {ok,State}.
terminate(_,_) -> flips_the_table.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Child
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

child_init(S) -> {ok,S,?CHILD_LOOP_TIMEOUT}.

child_handle_info(timeout, #child_state{l=L,cbmodule=CBModule,handler=Handler} = S) ->
	case gen_tcp:accept(L,60000) of
    {ok, Socket} ->
	    Pid = spawn(CBModule,get_sock,[Handler]),
	    gen_tcp:controlling_process(Socket,Pid),
        Pid!{ohaithar,Socket};
    {error,_} -> fuck_em
    end,
    {noreply,S,?CHILD_LOOP_TIMEOUT};

child_handle_info(Wut,S) ->
    error_logger:error_msg("nlisten_child got info ~w\n",[Wut]),
    {noreply,S,?CHILD_LOOP_TIMEOUT}.

%Shouldn't ever get this
child_handle_cast(Wut,S) ->
    error_logger:error_msg("nlisten_child got cast ~w\n",[Wut]),
    {noreply,S,?CHILD_LOOP_TIMEOUT}.

child_handle_call(Wut,From,S) ->
   error_logger:error_msg("nlisten_child got call ~w from ~w\n",[Wut,From]),
   {noreply,S,?CHILD_LOOP_TIMEOUT}.

%Nothing here
child_code_change(_,State,_) ->
    {ok,State}.
child_terminate(_,_) -> weeeeeeeeeeee.





