%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Library of function used to communicate file
% information across all the nemo nodes. This is
% as opposed to nfile which is used to get
% information about the physical files on this disk
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(nfs).
-author('Brian Picciano').
-include("nemo.hrl").
-compile(export_all).

-define(LOOP_TIMEOUT,60000).

%Attempts to add a record for the file to the local file db. If successful it
%will also use nnodemon to broadcast to other nodes that we have this file.
%Returns success if successful, {error,deleted} if the file is up for deletion
%(in which case it won't broadcast to other nodes), or {error,E}
add_whole_file(FileName) ->
    case nfile:size(FileName) of
    {error,E} -> {error,E};
    Size      -> 
        case ndb:try_add_file(#file{filename=FileName,size=Size,status=whole}) of
        stopped -> {error,deleted};
        ok      -> nnodemon:broadcast_consider(FileName), success
        end
    end.

%Goes through all the nodes a file is found on and marks it to
%be deleted
remove_file(FileName) ->
    lists:foreach(
        fun(Node) -> ok = rpc:call(Node,ndb,delete_file,[FileName]) end, 
        ?MODULE:exists_on_which_nodes(FileName)).

%Returns if a file exists here (bool)
exists_locally(FileName) ->
    ndb:file_exists(FileName).

%Returns if a file exists on another given node (bool)
exists_on_node(FileName,Node) ->
    rpc:call(Node,?MODULE,exists_locally,[FileName]).

%Returns a list of all the nodes a file exists on
exists_on_which_nodes(FileName) -> ?MODULE:exists_on_which_nodes(FileName,nutil:nnodes()).
exists_on_which_nodes(FileName,Nodes) ->
    nutil:filter_map(fun(Node) ->
         ?MODULE:exists_on_node(FileName,Node) andalso Node
    end,Nodes).

%Returns a list of all the nodes (except this one) a file exists on
exists_on_which_other_nodes(FileName) -> 
    ?MODULE:exists_on_which_nodes(FileName,nutil:nnodes_sans_me()).

%Returns whether or not the file is marked whole in the database on this or another node
is_file_whole(FileName)      -> ndb:file_is_whole(FileName).
is_file_whole(FileName,Node) -> rpc:call(Node,?MODULE,is_file_whole,[FileName]).

%Returns list of nodes where the file is marked as whole
where_is_whole(FileName) -> ?MODULE:where_is_whole(FileName,nutil:nnodes()).
where_is_whole(FileName,Nodes) ->
    nutil:filter_map(fun(Node) ->
        ?MODULE:is_file_whole(FileName,Node) andalso Node
    end,Nodes).

%Returns list of nodes (except this one) where the file is marked whole
where_else_is_whole(FileName) -> ?MODULE:where_is_whole(FileName,nutil:nnodes_sans_me()).

%Returns the name of a node which has this file as whole. Prefers this node,
%but if file isn't whole here returns random other node where it is. If not
%whole anywhere, returns false
node_where_is_whole(FileName) ->
    case ?MODULE:is_file_whole(FileName) of
    true  -> node();
    false -> ?MODULE:other_node_where_is_whole(FileName)
    end.

%Returns the name of a random node (except this one) where the file
%is whole, or false it isn't whole anywhere
other_node_where_is_whole(FileName) ->
    nutil:rand_from_list(?MODULE:where_else_is_whole(FileName)).

%Returns the size of the file, wherever it is. Tries to look on this node before anywhere else.
%May return {error,E} if there's an error.
size(FileName) ->
    ThisNode = node(),
    case ?MODULE:node_where_is_whole(FileName) of
    false    -> {error,file_dne};
    ThisNode -> nfile:size(FileName);
    Node     -> rpc:call(Node,nfile,size,[FileName])
    end.

%Given the filename, retrieves it from some other node and stores it here. If the file
%already has an entry of any sort on this node don't download anything. Returns {error,E}
%or success
retrieve_file(FileName) ->
    case ndb:insert_partial(FileName) of
    stopped -> {error,file_exists};
    ok      ->
        case {?MODULE:other_node_where_is_whole(FileName),?MODULE:size(FileName)} of
        {false,_}     -> {error,file_dne};
        {_,{error,E}} -> {error,E};
        {Node,Size}   ->
            case ndiplomat:file_on_sock(Node,FileName) of
            {error,E} -> {error,E};
            Sock ->
                case nfile:pipe_sock_to_file(Sock,FileName,Size) of
                {error,E} -> {error,pipe,E};
                ok ->
                    gsock:close(Sock),
                    ?MODULE:add_whole_file(FileName)
                end
            end
        end
    end.

%Reserves a file and returns the reserved file's name (binary)
reserve_file() ->
    ndb:reserve_file().
