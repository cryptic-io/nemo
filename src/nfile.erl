-module(nfile).
-author('Brian Picciano').
-include("nemo.hrl").
-include_lib("kernel/include/file.hrl").
-compile(export_all).

pipe_file(Socket,FileName) ->
    case byte_size(FileName) < 2 of
    true -> {error,filename_too_small};
    false ->
        FullPath = ?MODULE:full_path(FileName),
        try file:sendfile(FullPath,Socket) of
        {ok,_} -> success;
        {error,_} -> {error,file_error}
        catch
        _:_ -> {error,file_exception}
        end
    end.

file_size(FileName) ->
    case file:read_file_info(?MODULE:full_path(FileName)) of
    {ok, FileInfo} -> FileInfo#file_info.size;
    {error,E} -> {error,E}
    end.


full_path(<<A,B,_/binary>> = FileName) ->
    <<?FILE_LOCATION/binary,A,$/,B,$/,FileName/binary>>.
