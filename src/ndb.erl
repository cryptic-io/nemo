-module(ndb).
-include("nemo.hrl").
-author('Brian Picciano').
-compile(export_all).

%internal
-export([init/1,handle_call/3,handle_cast/2,
         handle_info/2,terminate/2,code_change/3]).

%external
-export([start/0,call/1,spawn_child/0]).

-define(LOOP_TIMEOUT,60000).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Doing calls
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_key(FileName,Key) -> ndb:call({addkey,FileName,Key}).
delete_key(Key)       -> ndb:call({deletekey,Key}).
get_file_for_key(Key) -> ndb:call({getfileforkey,Key}).
add_file(FileRec)     -> ndb:call({addfile,FileRec}).
file_exists(FileName) -> ndb:call({fileexists,FileName}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Carrying out calls
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

perform_call({addkey,FileName,Key}) ->
    mnesia:dirty_write(#filekey{filename=FileName,key=Key,ts=nutil:timestamp()});

perform_call({deletekey,Key}) ->
    mnesia:dirty_delete({filekey,Key});

perform_call({getfileforkey,Key}) ->
    case ?MODULE:select_full(filekey,Key) of
    empty -> {error,bad_key};
    FileKey -> FileKey#filekey.filename
    end;

perform_call({addfile,FileRec}) ->
   mnesia:dirty_write(FileRec);

perform_call({fileexists,FileName}) ->
    case ?MODULE:select_full(file,FileName) of
    empty -> false;
    _     -> true
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parent
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() -> gen_pool:start_link({local,nemo_gdb},?MODULE,'_',[]).

%Performs a db call and receives the result, returning it
call(Call) ->
    gen_server:call(nemo_gdb,Call).

init(_) -> {ok,{?NDB_CHILDREN,[]},'_',?LOOP_TIMEOUT}.

handle_call(Call,_,S) -> {forward,Call,S,?LOOP_TIMEOUT}.

handle_cast(Wut,S) ->
    error_logger:error_msg("ndb_server got cast ~w\n",[Wut]),
    {noreply,S,?LOOP_TIMEOUT}.

handle_info(timeout,S) -> {noreply,S,?LOOP_TIMEOUT};
handle_info(Wut,S) ->
    error_logger:error_msg("ndb_server got info ~w\n",[Wut]),
    {noreply,S,?LOOP_TIMEOUT}.

%Nothing to see here, move along
code_change(_,State,_) ->
    {ok,State}.
terminate(_,_) -> oh_noooo.

%Spawns a child and returns the pid
spawn_child() -> 
    {ok,Pid} = ndb_child:start(),
    Pid.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Child
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

child_init(_) -> {ok,'_'}.

%Handle commands from ndb_server
child_handle_call(Call,_,S) -> {reply,ndb:perform_call(Call),S,?LOOP_TIMEOUT}.

%Shouldn't ever get this
child_handle_cast(Wut,S) ->
    error_logger:error_msg("ndb_child got cast ~w\n",[Wut]),
    {noreply,S,?LOOP_TIMEOUT}.

child_handle_info(timeout,S) -> {noreply,S,?LOOP_TIMEOUT};
%Shouldn't ever get this
child_handle_info(Wut,S) ->
    error_logger:error_msg("ndb_child got info ~w\n",[Wut]),
    {noreply,S,?LOOP_TIMEOUT}.

%Nothing here
child_code_change(_,State,_) ->
    {ok,State,?LOOP_TIMEOUT}.
child_terminate(_,_) -> i_dont_blame_you.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Util
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Returns the first record in the db
first(Table) ->
	case mnesia:dirty_first(Table) of
	'$end_of_file'-> empty;
	Key -> ?MODULE:select_full(Table,Key) 
	end.

%Returns the record in the db just after Key
next(Table,Key) ->
	case mnesia:dirty_next(Table,Key) of
	'$end_of_file' -> empty;
	Rkey -> ?MODULE:select_full(Table,Rkey)
	end.

%A simple select, returns the full record, or empty
select_full(Table,Name) ->
    case mnesia:dirty_read(Table,Name) of
    [S] -> S;
    [] -> empty
    end.

%Prints out all records in the local db
select_all(Table) -> ?MODULE:select_all(Table, ?MODULE:first(Table) ).
select_all(_,empty) -> done;
select_all(Table,R) ->
    io:fwrite("~w\n",[R]),
    ?MODULE:select_all( Table,?MODULE:next(Table,lists:nth(2,tuple_to_list(R))) ).

