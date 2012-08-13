-module(nrpc).
-author('Brian Picciano').
-compile(export_all).

%extract(Struct,MetaList) -> Result
%    Types:
%        Struct = {struct,StructL} | StructL
%        StructL = [T]
%        List = [Value]
%        T = {Key,Value}
%        Key = binary()
%        Value = binary() | bool() | Struct | List
%
%        MetaList = [MetaObj]
%        MetaObj = {Key,MetaData}
%        MetaData = {MetaType,Default,Return}
%        MetaType = binary | bool | int | list | {list, Pred} | struct
%        Pred = fun((Elem :: Value) -> boolean())
%        Default = term() | required
%        Return = bool()
%
%        Result = StructL | {error, Error, [{field,Key}]}
%        Error = atom()

extract(StructL,MetaList) -> ?MODULE:extract(StructL,MetaList,[]).
extract({struct,StructL},MetaList,Ret) -> ?MODULE:extract(StructL,MetaList,Ret);
extract(_,[],Ret) -> Ret;
extract(StructL,[{Key,MetaData}|MetaList],Ret) ->
    case ?MODULE:process_metadata( nutil:keyfind(Key,StructL), MetaData) of
    {error,E} -> {error,E,[{field,Key}]};
    {return,Value} -> 
        ?MODULE:extract(StructL,MetaList,[{Key,Value}|Ret]);
    pass -> 
        ?MODULE:extract(StructL,MetaList,Ret)
    end.

%key doesn't exist but required is an error
process_metadata( dne, {_,required,_} ) -> {error, dne};
%key doesn't exist but has a default, return that default
process_metadata( dne, {_,Default,true} ) -> {return,Default};
%key doesn't exist but isn't required, don't even bother processing
process_metadata( dne, {_,_,false} ) -> pass;

process_metadata( Value, {MetaType,_,Return} ) ->
    case process_value(Value,MetaType) of
    {error,E} -> {error,E};
    CleanValue ->
        case Return of
        true -> {return, CleanValue};
        false -> pass
        end
    end.

%Check binary
process_value(Value,binary) when is_binary(Value) -> Value;
process_value(_,binary) -> {error,not_binary};

%Check bool
process_value(true,bool) -> true;
process_value(false,bool) -> false;
process_value(<<"true">>,bool) -> true;
process_value(<<"false">>,bool) -> false;
process_value(_,bool) -> {error,not_bool};

%Check int
process_value(Value,int) when is_integer(Value) -> Value;
process_value(_,int) -> {error,not_int};

%Check list
process_value(Value,list) when is_list(Value) -> Value;
process_value(_,list) -> {error,not_list};

%Check list with pred
process_value(Value,{list,Pred}) when is_list(Value) ->
    case lists:all(Pred,Value) of
    true -> Value;
    false -> {error,bad_list}
    end;
process_value(_,{list,_}) -> {error,not_list};

%Check struct
process_value({struct,Value},struct) -> Value;
process_value([{_,_}|_] = Value,struct) -> Value;
process_value(_,struct) -> {error,not_struct}.


%Also can be used as an example, pretend the first parameter
%was returned from mochijson
test() ->
    ?MODULE:extract(
        {struct,[
            {<<"command">>,<<"test">>},
            {<<"list">>,[1,2,<<"test">>]},
            {<<"bool">>,true},
            {<<"integer">>,1234},
            {<<"ignore">>,5}
        ]},
        [
            {<<"command">>,{binary,required,true}},
            {<<"list">>,{{list,fun(E) -> is_integer(E) end},required,true}},
            {<<"integer">>,{int,required,true}},
            {<<"ignore">>,{int,required,false}},
            {<<"optional">>,{int,10,true}},
            {<<"optional_ignore">>,{int,11,false}}
        ]).

