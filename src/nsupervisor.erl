-module(nsupervisor).
-author('Brian Picciano').
-include("nemo.hrl").
-behavior(supervisor).

%internal
-export([start/0]).
%external
-export([init/1]).

-define(WORKER(Mod,Params), {Mod, {Mod, start, Params}, permanent, 1000, worker, [Mod] }).

start() -> {ok,_} = supervisor:start_link({local,?MODULE},?MODULE,'_').

init(_) -> 
    {ok, {
        {one_for_one, 100, 1000},
        [
            ?WORKER(ndb,[]),
            ?WORKER(nopt,[]),
            ?WORKER(nlisten,[])
        ]
    }}.
