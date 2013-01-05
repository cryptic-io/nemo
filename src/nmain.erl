-module(nmain).
-author('Brian Picciano').
-include("nemo.hrl").
-compile(export_all).

-define(TABLE(Name), mnesia:create_table(
        Name,
        [
            {attributes, record_info(fields,Name)},
            {type,set}
        ])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Nemo init
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() -> spawn(?MODULE,init,[]).
init() ->

    ok = lager:start(),

    %Starts mnesia and sets up tables
	ok = mnesia:start(),
    {atomic,ok} = ?TABLE(filekey),
    {atomic,ok} = ?TABLE(file),
    {atomic,ok} = ?TABLE(nodedist),


    %Hopefully force mnesia to actually pick up its garbage
    mnesia_recover:allow_garb(),
    mnesia_recover:start_garb(),

    case ?SASL_ENABLED of
    true -> application:start(sasl);
    _ -> bummer
    end,

	%Local threads
    nsupervisor:start(),

    %Hack to keep the supervisor alive
    lager:notice("Nemo started"),
    timer:sleep(infinity).
