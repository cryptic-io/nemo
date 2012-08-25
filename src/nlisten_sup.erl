-module(nlisten_sup).
-author('Brian Picciano').
-include("nemo.hrl").
-behavior(supervisor).

%internal
-export([start/0]).
%external
-export([init/1]).

-define(WORKER(Name,Mod,Params), {Name, {Mod, start, Params}, permanent, 1000, worker, [Mod] }).

start() -> 
    supervisor:start_link({local,nemo_nlisten_sup},?MODULE,'_').

init(_) -> 
    {ok, {
        {one_for_one, 100, 1000},
        [
            ?WORKER(private_port,nlisten,[nconnection,privateport,nprivatehandler]),
            ?WORKER(public_port,nlisten,[nconnection,publicport,npublichandler])
        ]
    }}.

