-module(nfile).
-author('Brian Picciano').
-include("nemo.hrl").
-compile(export_all).

pipe_file(Socket,FileName) ->
    case byte_size(FileName) < 2 of
    true -> {error,filename_too_small};
    false ->
        FullPath = ?MODULE:full_path(FileName),
        try file:sendfile(FullPath,Socket) of
        {ok,_} -> success;
        {error,E} -> {error,E}
        catch
        _:_ -> {error,file_exception}
        end
    end.

full_path(<<A,B,_/binary>> = FileName) ->
    <<?FILE_LOCATION/binary,A,$/,B,$/,FileName/binary>>.
