-module(nprivatehandler).
-author('Brian Picciano').
-include("nemo.hrl").
-compile(export_all).

command_dispatch(Command,Struct,S) ->
    case {S#conn_state.sudo,Command} of
    {true,<<"addFileKey">>} ->  ?MODULE:command_addFileKey(Struct,S);
    _ ->                        {S, {error, unknown_command}}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(ADDFILEKEY_EXTRACT,[
                                {<<"filename">>,{binary,required}},
                                {<<"key">>,{binary,required}}
                           ]).
command_addFileKey(Struct,S) ->
    Ret = case nrpc:extract(Struct,?ADDFILEKEY_EXTRACT) of
    {error,Error,Extra} -> {error,Error,Extra};
    [{_,Key},{_,FileName}] ->
        ndb:add_key(FileName,Key),
        {success,<<"addFileKey">>}
    end,
    {S,Ret}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

