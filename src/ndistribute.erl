-module(ndistribute).
-include("nemo.hrl").
-author('Brian Picciano').
-compile(export_all).


%Using Shift-Add-XOR hash, hashes bitstring into integer
hash(Str) -> ?MODULE:hash(Str,0).
hash(<<B,Str/binary>>,H) ->
    NewH = H bxor ((H bsl 5) + (H bsr 2) + B),
    ?MODULE:hash(Str,NewH);
hash(<<>>,H) -> H rem 100.

%Sets up the nodedist table with a bunch of blank lists
init_nodedist() ->
    lists:foreach(
        fun(I) -> ndb:set_nodedist(I,[]) end,
        lists:seq(0,99)
    ).

%Removes all references to Node from nodedist table
remove_from_nodedists(Node) ->
    ndb:remove_from_nodedists(Node).

%Adds Node with Priority to nodedist table in given range
add_to_nodedists(Node,Priority,{Start,End} = _Range) ->
    lists:foreach(
        fun(I) -> ?MODULE:add_to_nodedist(Node,Priority,I) end,
        lists:seq(Start,End)
    ).

add_to_nodedist(Node,Priority,I) ->
    #nodedist{nodeprios=NodePrios} = ndb:get_nodedist(I),
    NewNodePrio  = #nodeprio{node=Node,priority=Priority},
    NewNodePrios = insert_by_prio(NodePrios,NewNodePrio),
    ndb:set_nodedist(I,NewNodePrios).

insert_by_prio([],NewNodePrio) -> [NewNodePrio];
insert_by_prio([NP|NodePrios],NewNodePrio) ->
    case NP#nodeprio.priority < NewNodePrio#nodeprio.priority of
    true  -> [NewNodePrio,NP|NodePrios];
    false -> [NP|?MODULE:insert_by_prio(NodePrios,NewNodePrio)]
    end.

%Given a filename returns a list of nodes that file should be on in
%order of ascending priority (highest priority number first)
nodes_for_file(FileName) ->
    I = ?MODULE:hash(FileName),
    #nodedist{nodeprios=L} = ndb:get_nodedist(I),
    lists:map(fun(N) -> N#nodeprio.node end,L).
