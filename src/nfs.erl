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

exists(FileName) ->
    ndb:file_exists(FileName).

reserve_file() ->
    ndb:reserve_file().
