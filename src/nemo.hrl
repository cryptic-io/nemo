-include("nodelist.hrl").

%Used for code reloading, doesn't matter much other then that
-define(MODULE_LIST, [ ]).

%IPs with sudo privileges
-define(SUDO_IPS, [
    {127,0,0,1}
]).

%Define number of children various processes have to distribute work amongst
-define(NLISTEN_CHILDREN,30).

%Record to define connection state. Used by gconnection mainly, but passed to others
-record(conn_state,{
    ip= false,
    sudo= false
}).

-define(FILE_LOCATION,<<"/tmp/cc/">>).

%Hack to turn a record into a tuplelist
-define(record_to_tuplelist(Rec, Ref), lists:zip(record_info(fields, Rec),tl(tuple_to_list(Ref)))).
