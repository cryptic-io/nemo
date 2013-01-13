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

get_file(FileName)           -> ?MODULE:call({get_file,FileName}).
add_key(FileName,Key)        -> ?MODULE:call({addkey,FileName,Key}).
delete_key(Key)              -> ?MODULE:call({deletekey,Key}).
get_file_for_key(Key)        -> ?MODULE:call({getfileforkey,Key}).
add_file(FileRec)            -> ?MODULE:call({addfile,FileRec}).
add_file_unless(FileRec,Fun) -> ?MODULE:call({addfileunless,FileRec,Fun}).
delete_file(FileName)        -> ?MODULE:call({deletefile,FileName}).
file_exists(FileName)        -> ?MODULE:call({fileexists,FileName}).
file_is_whole(FileName)      -> ?MODULE:call({fileiswhole,FileName}).
reserve_file()               -> ?MODULE:call(reserve_file).
set_nodedist(I,L)            -> mnesia:dirty_write(#nodedist{i=I,nodeprios=L}).
get_nodedist(I)              -> ?MODULE:select_full(nodedist,I).
remove_from_nodedists(Node)  -> ?MODULE:call({remove_from_nodedists,Node}).
dump_nodedists()             -> ?MODULE:call(dump_nodedists).

insert_partial(FileName) ->
    ?MODULE:add_file_unless(#file{filename=FileName,size=0,status=partial},
                                  fun() -> mnesia:read(file,FileName) /= []  end).

try_add_file(FileRec) ->
    ?MODULE:add_file_unless(FileRec, fun() -> case mnesia:read(file,FileRec#file.filename) of
                                              [#file{status={todelete,_}}] -> true;
                                              _ -> false
                                              end end).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Carrying out calls
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

perform_call({get_file,FileName}) ->
    ?MODULE:select_full(file,FileName);

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

perform_call({addfileunless,FileRec,Fun}) ->
    {atomic,F} = mnesia:transaction(fun() ->
        case Fun() of
        true -> stopped;
        _    -> mnesia:write(FileRec)
        end
    end),
    F;

perform_call({deletefile,FileName}) ->
    {atomic,F} = mnesia:transaction(fun() ->
        case mnesia:read(file,FileName) of
        [FileRec] -> mnesia:write(FileRec#file{status={todelete,nutil:timestamp()}});
        [] -> ok
        end
    end),
    F;

perform_call({fileexists,FileName}) ->
    ?MODULE:perform_call({get_file,FileName}) /= empty;

perform_call({fileiswhole,FileName}) ->
    case ?MODULE:perform_call({get_file,FileName}) of
    empty -> false;
    FileRec when FileRec#file.status == whole -> true;
    _ -> false
    end;

perform_call(reserve_file) ->
    {atomic,F} = mnesia:transaction(fun() ->
        FileName = ?MODULE:get_unique_name(),
        File = #file{filename=FileName,
                     size=0,
                     status=reserved},
        mnesia:write(File),
        FileName
    end),
    F;

perform_call({remove_from_nodedists,Node}) ->
    {atomic,F} = mnesia:transaction(fun() ->
        lists:foreach(
            fun(I) ->
                [NodeDist] = mnesia:read(nodedist,I),
                NewNodePrios = lists:filter(
                                fun(NodePrio) -> NodePrio#nodeprio.node /= Node end,
                                NodeDist#nodedist.nodeprios
                               ),
                mnesia:write(NodeDist#nodedist{nodeprios=NewNodePrios})
            end,
            lists:seq(0,99)
        )
    end),
    F;

perform_call(dump_nodedists) ->
    lists:map(
        fun(I) -> ?MODULE:select_full(nodedist,I) end,
        lists:seq(0,99)
    ).

%Returns unique filename, can only be called from within transaction
get_unique_name() ->
    FileName = nutil:random_string(32),
    case mnesia:read(file,FileName) of
    [] -> FileName;
    _  -> ?MODULE:get_unique_name()
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parent
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() -> gen_pool:start_link({local,nemo_ndb},?MODULE,'_',[]).

%Performs a db call and receives the result, returning it
call(Call) ->
    gen_server:call(nemo_ndb,Call).

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

%Deletes a row
delete(Table,Name) ->
    ok = mnesia:dirty_delete(Table,Name).
delete_object(Table,R) ->
    ok = mnesia:dirty_delete_object(Table,R).

%Returns the first record in the db
first(Table) ->
	case mnesia:dirty_first(Table) of
	'$end_of_file'-> empty;
	Key -> ?MODULE:select_full(Table,Key) 
	end.

%Returns the record in the db just after Key
next(Table,R) ->
    Key = ?MODULE:primary(R),
	case mnesia:dirty_next(Table,Key) of
	'$end_of_file' -> empty;
	Rkey -> ?MODULE:select_full(Table,Rkey)
	end.

%A simple select, returns the full record, or empty
select_full(Table,Name) ->
    case mnesia:dirty_read(Table,Name) of
    [S] -> S;
    [] -> empty;
    S -> S
    end.

primary(R) ->
    lists:nth(2,tuple_to_list(R)).

%Prints out all records in the local db
select_all(Table) -> ?MODULE:select_all(Table, ?MODULE:first(Table) ).
select_all(_,empty) -> done;
select_all(Table,[F|_] = S) ->
    lists:foreach(fun(R) -> io:fwrite("~p\n",[R]) end,S),
    ?MODULE:select_all( Table,?MODULE:next(Table,F) ); 
select_all(_,[]) -> done;
select_all(Table,R) ->
    ?MODULE:select_all( Table,[R] ).

