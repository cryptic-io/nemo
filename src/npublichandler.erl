-module(npublichandler).
-author('Brian Picciano').
-include("nemo.hrl").
-compile(export_all).

command_dispatch(Command,Struct,S) ->
    case {S#conn_state.sudo,Command} of
    {_,<<"downloadFile">>}  ->  ?MODULE:command_downloadFile(Struct,S);
    _ ->                        {S, {error, unknown_command}}
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(DOWNLOADFILE_EXTRACT,[
                                {<<"filename">>,{binary,required}},
                                {<<"key">>,{binary,required}}
                             ]).
command_downloadFile(Struct,S) ->
    Ret = 
        case nrpc:extract(Struct,?DOWNLOADFILE_EXTRACT) of
        [{_,Key},{_,FileName}] -> 
            case ndb:get_file_for_key(Key) of
            FileName ->
                case nfile:file_size(FileName) of
                {error,E} -> {error,E};
                Size ->
                    spawn(ndb,delete_key,[Key]),
                    {pipe,FileName,Size}
                end;
            _ -> {error,bad_key}
            end;
        {error,Error,Extra} -> {error,Error,Extra}
        end,
    {S,Ret}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
