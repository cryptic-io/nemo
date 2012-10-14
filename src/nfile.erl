-module(nfile).
-author('Brian Picciano').
-include("nemo.hrl").
-include_lib("kernel/include/file.hrl").
-compile(export_all).

pipe(Socket,FileName) ->
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

size(FileName) ->
    case file:read_file_info(?MODULE:full_path(FileName)) of
    {ok, FileInfo} -> FileInfo#file_info.size;
    {error,enoent} -> {error,file_dne};
    {error,E}      -> {error,E}
    end.

exists(FileName) ->
    filelib:is_file(?MODULE:full_path(FileName)).

full_path(<<A,B,_/binary>> = FileName) ->
    <<?FILE_LOCATION/binary,A,$/,B,$/,FileName/binary>>.

%Performs Fun on each file in under ?FILE_LOCATION, where the
%param passed into fun is the name of the file (not full path)
foreach_file(Fun) ->
    FileLoc = binary_to_list(?FILE_LOCATION),
    Dirs = filelib:wildcard(FileLoc++[$*]),
    lists:foreach(fun(InDir) ->
        Dirs2 = filelib:wildcard(InDir++[$/,$*]),
        lists:foreach(fun(InDir2) ->
            {ok,Files} = file:list_dir(InDir2),
            lists:foreach(fun(File) -> Fun(list_to_binary(File)) end,Files)
        end,Dirs2)
    end,Dirs).
