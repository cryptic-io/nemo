%This is included from ccetc, support/include.mk handles figuring out
%where ccetc actually is
-include("nemo_config.hrl").

%Used for code reloading, doesn't matter much other then that
-define(MODULE_LIST, [
    gen_pool,
    gen_pool_child,
    mochijson2,
    nconnection,
    ndb,
    ndistribute,
    nfile,
    nfs,
    ngarbagecollector,
    nlisten,
    nlisten_sup,
    nmain,
    nnodemon,
    nprivatehandler,
    npublichandler,
    nrpc,
    nsock,
    nsupervisor,
    nutil
]).

%Max time a filekey can be used before expiring, in seconds
-define(FILEKEY_TTL, 60).

%Define number of children various processes have to distribute work amongst
-define(NLISTEN_CHILDREN,30).
-define(NDB_CHILDREN,30).

%Record to define connection state. Used by gconnection mainly, but passed to others
-record(conn_state,{
    ip= false,
    sudo= false,
    handler= false
}).

-record(filekey,{key,filename,ts}).

-record(nodeprio,{node,priority}).
-record(nodedist,{i,nodeprios=[]}).

%status can be:
%   whole
%   partial
%   reserved
%   {todelete,TS}
-record(file,   {filename,size,status}).

%Hack to turn a record into a tuplelist
-define(record_to_tuplelist(Rec, Ref), lists:zip(record_info(fields, Rec),tl(tuple_to_list(Ref)))).
