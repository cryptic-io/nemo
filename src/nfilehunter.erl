-module(nfilehunter).
-author("Brian Picciano").
-include("nemo.hrl").
-compile(export_all).

start() ->
    error_logger:info_msg("Getting initial list of files into mnesia"),
    ?MODULE:loop(),
    periodically:start_link({local,nemo_nfilehunter}, 60000, {?MODULE,loop,'_'}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%add_whole_file will send out a broadcast to other nodes that we have
%the file (if it's not up for deletion). So this serves as a way of
%periodically sending out what we've got to other nodes, as well as
%finding new files.
loop(_) -> ?MODULE:loop().
loop()  ->
    nfile:foreach_file(fun(FileName) -> 
        case nfile:hash(FileName) of
        {error,E} -> error_logger:error_msg("nfilehunter wasn't able to hash ~p, ~p",[FileName,E]);
        Hash -> nfs:add_whole_file(FileName,Hash)
        end
    end).
