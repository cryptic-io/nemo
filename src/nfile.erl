%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Library of functions used to interact with files
% on this disk
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(nfile).
-author('Brian Picciano').
-include("nemo.hrl").
-include_lib("kernel/include/file.hrl").
-compile(export_all).

-define(CHUNK_SIZE,10240).

pipe_file_to_sock(FileName,Sock) ->
    case ?MODULE:full_path(FileName) of
    {error,E} -> {error,E};
    FullPath ->
        try file:sendfile(FullPath,Sock) of
        {ok,_} -> success;
        {error,_} -> {error,file_error}
        catch
        _:_ -> {error,file_exception}
        end
    end.

pipe_sock_to_file(Sock,FileName,Size) ->
    case ?MODULE:full_path(FileName) of
    {error,E} -> {error,E};
    FullPath ->
        case { ?MODULE:mkdir_p(FileName),
               file:open(FullPath,[write,binary,raw]) } of
        {{error,E},_} -> {error,E};
        {_,{error,E}} -> {error,E};
        {ok,{ok, FH}} -> ?MODULE:pipe_sock_to_file_loop(Sock,FH,Size)
        end
    end.

pipe_sock_to_file_loop(_Sock,_FH,Size) when (Size =< 0) -> ok;
pipe_sock_to_file_loop(Sock,FH,Size) ->
    ToPull = min(?CHUNK_SIZE,Size),
    case gen_tcp:recv(Sock,ToPull,1000) of
    {ok,Data} ->
        case file:write(FH,Data) of
        ok -> ?MODULE:pipe_sock_to_file_loop(Sock,FH,Size-ToPull);
        {error,E} -> {error,E}
        end;
    {error,E} -> {error,E}
    end.

size(FileName) ->
    case ?MODULE:full_path(FileName) of
    {error,E} -> {error,E};
    FullName ->
        case file:read_file_info(FullName) of
        {ok, FileInfo} -> FileInfo#file_info.size;
        {error,enoent} -> {error,file_dne};
        {error,E}      -> {error,E}
        end
    end.

%Checks if the file physically exists on this box, not
%if it exists somewhere in lala land (which is what nfs:exists
%checks)
exists(FileName) ->
    case ?MODULE:full_path(FileName) of
    {error,_} -> false;
    FullName  -> filelib:is_file(FullName)
    end.

full_path(<<A,B,_,_/binary>> = FileName) ->
    <<?FILE_LOCATION/binary,A,$/,B,$/,FileName/binary>>;
full_path(_) -> {error,filename_is_bad_and_you_should_feel_bad}.

mkdir_p(<<A,B,_,_/binary>>) ->
    case file:make_dir(<<?FILE_LOCATION/binary,A>>) of
    {error,E} when E /= eexist -> {error,E};
    _ ->
        case file:make_dir(<<?FILE_LOCATION/binary,A,$/,B>>) of
        {error,E} when E /= eexist -> {error,E};
        _ -> ok
        end
    end;
mkdir_p(_) -> {error,filename_is_bad_and_you_should_feel_bad}.

delete(FileName) ->
    case ?MODULE:full_path(FileName) of
    {error,E} -> {error,E};
    FullName  -> file:delete(FullName)
    end.

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
