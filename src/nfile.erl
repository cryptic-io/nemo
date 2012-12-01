%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Library of functions used to interact with files
% on this disk
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(nfile).
-author('Brian Picciano').
-include("nemo.hrl").
-include_lib("kernel/include/file.hrl").
-compile(export_all).

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

pipe_nfh_to_file(Nfh,FileName) ->
    case ?MODULE:full_path(FileName) of
    {error,E} -> {error,E};
    FullPath ->
        case ?MODULE:mkdir_p(FileName) of
        {error,E} -> {error,E};
        ok ->
            case file:open(FullPath,[write,binary]) of
            {error,E} -> {error,E};
            {ok,Fh}   -> ?MODULE:gen_pipe(Nfh,(fun nfh:read/2),Fh,(fun file:write/2))
            end
        end
    end.

gen_pipe(ReadFH,ReadFun,WriteFH,WriteFun) ->
    case ReadFun(ReadFH,10240) of
    {error,E} -> {error,E};
    eof       -> ok;
    {ok,Data} ->
        case WriteFun(WriteFH,Data) of
        {error,E} -> {error,E};
        ok        -> ?MODULE:gen_pipe(ReadFH,ReadFun,WriteFH,WriteFun)
        end
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
