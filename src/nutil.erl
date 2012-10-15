-module(nutil).
-author('Brian Picciano').
-include("nemo.hrl").
-compile(export_all).

%Takes either a key, or list of keys, and tries to find them at the position
%specified. If no position is specified 1 is used. If a single key is given
%the result is either the value or dne. If a list is given a list is returned,
%where each position in the list corresponds to the return value of keyfind
%for the key given at that position in the input list
%
%keyfind([one,two],1,[ {one,uno},{three,tres} ]) ->
%   [uno,dne]
%
keyfind(Key,List) -> ?MODULE:keyfind(Key,1,List).
keyfind([],_,_) -> [];
keyfind([Key|Keys],Pos,List) ->
    [?MODULE:keyfind(Key,Pos,List)|?MODULE:keyfind(Keys,Pos,List)];
keyfind(Key,Pos,List) ->
    case is_list(List) of
    true ->
        case lists:keyfind(Key,Pos,List) of
        {Key,Value} -> Value;
        _ -> dne
        end;
    false -> dne
    end.

keymember(Key,List) -> ?MODULE:keymember(Key,1,List).
keymember(Key,Pos,List) -> is_list(List) andalso lists:keymember(Key,Pos,List). 
list_check([],_) -> true;
list_check([Item|List],Fun) ->
    case Item == dne orelse Fun(Item) of
    true -> ?MODULE:list_check(List,Fun);
    false -> false
    end;
list_check(_,_) -> false.

list_adds(List1,List2,CompareFun) ->
    lists:filter(fun(ListItem2) ->
        not lists:any(fun(ListItem1) ->
                CompareFun(ListItem1,ListItem2)
            end,List1)
    end,List2).

filter_map(Fun,List) ->
    [ Y || X<-List, Y<-[Fun(X)], Y /= false ].

check_ip_sudo(Ip) ->
    {Ip1,Ip2,Ip3,Ip4} = Ip,
    lists:any(fun({AIp1,AIp2,AIp3,AIp4}) ->
        (AIp1 == '_' orelse Ip1 == AIp1) andalso
        (AIp2 == '_' orelse Ip2 == AIp2) andalso
        (AIp3 == '_' orelse Ip3 == AIp3) andalso
        (AIp4 == '_' orelse Ip4 == AIp4)
    end,?SUDO_IPS).

nnodes() -> [ X || X <- [node()|nodes()], lists:member(X,?NODE_LIST) ].
nnodes_sans_me() ->  [ X || X <- nodes(), lists:member(X,?NODE_LIST) ].


%Returns a list of the pids linked to this thread.
get_linked_pids() ->
	{links,L} = process_info(self(),links),
	[X || X <- L, is_pid(X)].

remote_gen_start(Node,Module,Params,Opts) ->
    rpc:call(Node,gen_server,start,[Module,Params,Opts]).

unlink(Pid) ->
    Pid /= false andalso erlang:unlink(Pid).

struct_to_list(S) when is_binary(S) -> "<<\""++binary_to_list(S)++"\">>";
struct_to_list(S) when is_integer(S) -> integer_to_list(S);
struct_to_list(S) when is_atom(S) -> atom_to_list(S);
struct_to_list(S) when is_pid(S) -> "<pid>";
struct_to_list(S) when is_list(S) -> 
    "["++?MODULE:list_separate([ ?MODULE:struct_to_list(X) || X <- S ],$,)++"]";
struct_to_list(S) when is_tuple(S) ->
    "{"++?MODULE:list_separate([ ?MODULE:struct_to_list(X) || X <- tuple_to_list(S) ],$,)++"}".

ip_to_list({A,B,C,D}) ->    integer_to_list(A)++"."++
                            integer_to_list(B)++"."++
                            integer_to_list(C)++"."++
                            integer_to_list(D).

list_separate([],_) -> [];
list_separate([X],_) -> X;
list_separate([X|Y],I) -> X++[I]++?MODULE:list_separate(Y,I).


timestamp() ->
    {Mega, Sec, _} = now(),
    Mega * 1000 * 1000 + Sec.


-define(RANDOM_CHARS,"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890").
-define(RANDOM_CHARS_LEN,62).
random_string(Length) ->
    L =lists:foldl(fun(_, Acc) ->
                        [lists:nth(random:uniform(?RANDOM_CHARS_LEN), ?RANDOM_CHARS)]
                            ++ Acc
                end, [], lists:seq(1, Length)),
    list_to_binary(L).
