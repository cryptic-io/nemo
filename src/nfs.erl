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
exists_on_node(Node,FileName) ->
    rpc:call(Node,?MODULE,exists_locally,[FileName]).

%Returns a list of all the nodes a file exists on
exists_on_which_nodes(FileName) -> ?MODULE:exists_on_which_nodes(FileName,nutil:nnodes()).
exists_on_which_nodes(FileName,Nodes) ->
    nutil:filter_map(fun(Node) ->
         ?MODULE:exists_on_node(Node,FileName) andalso Node
    end,Nodes).

%Returns a list of all the nodes (except this one) a file exists on
exists_on_which_other_nodes(FileName) -> 
    ?MODULE:exists_on_which_nodes(FileName,nutil:nnodes_sans_me()).

%Reserves a file and returns the reserved file's name (binary)
reserve_file() ->
    ndb:reserve_file().
