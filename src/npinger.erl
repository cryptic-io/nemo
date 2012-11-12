-module(npinger).
-author("Brian Picciano").
-include("nemo.hrl").
-compile(export_all).

start() ->
    periodically:start_link({local,nemo_npinger},10000,{?MODULE,pingall,start}).

pingall(start) -> ?MODULE:pingall(?NODE_LIST);
pingall([]) -> fin;
pingall([Node|Nodes]) -> 
    net_adm:ping(Node),
    ?MODULE:pingall(Nodes).
