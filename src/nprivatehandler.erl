-module(nprivatehandler).
-author('Brian Picciano').
-include("nemo.hrl").
-compile(export_all).

command_dispatch(Command,Struct,S) ->
    case {S#conn_state.sudo,Command} of
    {true,<<"addFileKeys">>}     -> ?MODULE:command_addFileKeys(Struct,S);
    {true,<<"addFile">>}         -> ?MODULE:command_addFile(Struct,S);
    {true,<<"reserveFile">>}     -> ?MODULE:command_reserveFile(Struct,S);
    {true,<<"reload">>}          -> ?MODULE:command_reload(Struct,S);
    {true,<<"applyNodeChange">>} -> ?MODULE:command_applyNodeChange(Struct,S);
    {true,<<"removeNode">>}      -> ?MODULE:command_removeNode(Struct,S);
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
            RetList = 
                lists:map(fun(FileKey) ->
                    case nrpc:extract(FileKey,?ADDFILEKEY_EXTRACT) of
                    {error,Error,Extra} -> 
                        {struct,[{error,Error}|Extra]};
                    [{_,Key},{_,Filename}] ->
                        ndb:add_key(Filename,Key),
                        {struct,[{filename,Filename},{key,Key}]}
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
            case nfs:add_whole_file(FileName) of
            {error,E} -> {error,E};
            success   -> {success,<<"addFile">>}
            end
        end,
    {S,Ret}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

command_reserveFile(_Struct,S) ->
    FileName = nfs:reserve_file(),
    {S,{success,FileName}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

command_reload(_,S) ->
    ndev:reload_all(),
    {S,{success,<<"reload">>}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(APPLYNODECHANGE_EXTRACT,[
                                    {<<"node">>,{binary,required}},
                                    {<<"priority">>,{float,required}},
                                    {<<"rangeStart">>,{int,required}},
                                    {<<"rangeEnd">>,{int,required}}
                                ]).
command_applyNodeChange(Struct,S) ->
    Ret =
        case nrpc:extract(Struct,?APPLYNODECHANGE_EXTRACT) of
        {error,Error,Extra} -> {error,Error,Extra};
        [{_,End},{_,Start},{_,Priority},{_,NodeStr}] ->
            try binary_to_existing_atom(NodeStr,utf8) of
            Node ->
                case lists:member(Node,?NODE_LIST) of
                true  -> 
                    nnodemon:broadcast_apply(Node,Priority,{Start,End}),
                    {success,<<"applyNodeChange">>};
                false -> {error,node_dne}
                end
            catch _:_ -> {error,node_dne}
            end
        end,
    {S,Ret}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(REMOVENODE_EXTRACT,[
                                {<<"node">>,{binary,required}}
                           ]).
command_removeNode(Struct,S) ->
    Ret =
        case nrpc:extract(Struct,?REMOVENODE_EXTRACT) of
        {error,Error,Extra} -> {error,Error,Extra};
        [{_,NodeStr}] ->
            try binary_to_existing_atom(NodeStr,utf8) of
            Node ->
                case lists:member(Node,?NODE_LIST) of
                true  -> 
                    nnodemon:broadcast_remove(Node),
                    {success,<<"removeNode">>};
                false -> {error,node_dne}
                end
            catch _:_ -> {error,node_dne}
            end
        end,
    {S,Ret}.
