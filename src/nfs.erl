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

add_whole_file(FileName) ->
    case nfile:size(FileName) of
    {error,E} -> {error,E};
    Size      -> 
        ndb:add_file(#file{filename=FileName,size=Size,status=whole}),
        success
    end.

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
    case is_file_whole(FileName) of
    true  -> node();
    false -> ?MODULE:other_node_where_is_whole(FileName)
    end.

%Returns the name of a random node (except this one) where the file
%is whole, or false it isn't whole anywhere
other_node_where_is_whole(FileName) ->
    nutil:rand_from_list(?MODULE:where_else_is_whole(FileName)).

%Reserves a file and returns the reserved file's name (binary)
reserve_file() ->
    ndb:reserve_file().
