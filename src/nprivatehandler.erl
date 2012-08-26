-module(nprivatehandler).
-author('Brian Picciano').
-include("nemo.hrl").
-compile(export_all).

command_dispatch(Command,Struct,S) ->
    case {S#conn_state.sudo,Command} of
    {true,<<"addFileKeys">>} ->  ?MODULE:command_addFileKeys(Struct,S);
    {true,<<"addFile">>}     ->  ?MODULE:command_addFile(Struct,S);
    _ ->                        {S, {error, unknown_command}}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(ADDFILEKEYS_EXTRACT,[
                                {<<"filekeys">>,{list,required}}
                            ]).
-define(ADDFILEKEY_EXTRACT,[
                                {<<"filename">>,{binary,required}},
                                {<<"key">>,{binary,required}}
                           ]).
command_addFileKeys(Struct,S) ->
    Ret = 
        case nrpc:extract(Struct,?ADDFILEKEYS_EXTRACT) of
        {error,Error,Extra} -> {error,Error,Extra};
        [{_,FileKeys}] ->
            RetList = lists:map(fun(FileKey) ->
                case nrpc:extract(FileKey,?ADDFILEKEY_EXTRACT) of
                {error,Error,Extra} -> 
                    {struct,[{error,Error}|Extra]};
                [{_,Key},{_,Filename}] ->
                    ndb:add_key(Filename,Key),
                    {struct,[{filename,Filename},{success,ok}]}
                end
            end,FileKeys),
            {return,RetList}
        end,
    {S,Ret}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(ADDFILE_EXTRACT,[
                            {<<"filename">>,{binary,required}}
                        ]).
command_addFile(Struct,S) ->
    Ret = 
        case nrpc:extract(Struct,?ADDFILE_EXTRACT) of
        {error,Error,Extra} -> {error,Error,Extra};
        [{_,FileName}] ->
            case nfs:add_file(FileName) of
            {error,E} -> {error,E};
            success   -> {success,<<"addFile">>}
            end
        end,
    {S,Ret}.
