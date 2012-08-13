-module(nmain).
-author('Brian Picciano').
-include("nemo.hrl").
-compile(export_all).

-define(PMAP_TABLE(Name), mnesia:create_table(
        Name,
        [
            {record_name,pmap},
            {attributes, record_info(fields,pmap)},
            {type,set},
            {index,[bucket]}
        ])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Nemo init
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() -> spawn(?MODULE,init,[]).
init() ->
    
	%Save the list of remote nodes to memory
	Nodes = ?MODULE:parse_nodes(?NODE_LIST),

	%Ping all the nodes to make sure they're all connected.
	?MODULE:ping_all(Nodes),

    %Starts mnesia and sets up tables
	ok = mnesia:start(),
    {atomic, ok} = mnesia:create_table(
        filekey,
        [
            {attributes, record_info(fields,filekey)},
            {type,set}
        ]
    ),

    %Hopefully force mnesia to actually pick up its garbage
    mnesia_recover:allow_garb(),
    mnesia_recover:start_garb(),

	%Local threads
    nsupervisor:start(),

    %Needs to be done after supervisor since the nopt thread hasn't been
    %started yet
    case nopt:global_getOpt("sasl") of
    "true" -> application:start(sasl);
    _ -> bummer
    end,

    %Hack to keep the supervisor alive
    error_logger:info_msg("Nemo started\n"),
    timer:sleep(infinity).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Utility
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%Parses the node list and returns one not containing this node. This is so that
%each node doesn't need a different node list and code can be constant throughout
parse_nodes(Nodes) -> ?MODULE:parse_nodes(node(),Nodes,[]).
parse_nodes(_,[],Parsed) -> Parsed;
parse_nodes(This,[N|Unparsed],Parsed) ->
	case N of
	This -> ?MODULE:parse_nodes(This,Unparsed,Parsed);
	_ -> ?MODULE:parse_nodes(This,Unparsed,[N|Parsed])
	end.

%Pings every node in a list of nodes
ping_all([]) -> ok;
ping_all([Node|L]) ->
    io:fwrite("Pinging ~w..... ",[Node]),
	io:fwrite("~w\n",[net_adm:ping(Node)]),
	?MODULE:ping_all(L).





