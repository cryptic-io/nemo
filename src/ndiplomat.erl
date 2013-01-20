%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The diplomat interacts with the other nodes (usually
% over their private interface)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(ndiplomat).
-author('Brian Picciano').
-include("nemo.hrl").
-compile(export_all).

-define(TCP_OPTS, [binary, {packet, raw}, {nodelay, true}, {active, false}]).

private_host()  -> ?PRIVATEHOST.
private_host(N) -> rpc:call(N, ?MODULE, private_host, []).
private_port()  -> ?PRIVATEPORT.
private_port(N) -> rpc:call(N, ?MODULE, private_port, []).

%Returns a socket with the given FileName already coming down it
file_on_sock(N,FileName) ->
    Host = ?MODULE:private_host(N),
    Port = ?MODULE:private_port(N),
    case gen_tcp:connect(Host,Port,?TCP_OPTS) of
    {ok,Sock} ->
        case nsock:gen_json(Sock,[{command,<<"downloadFile">>},{filename,FileName}]) of
        sent   -> Sock;
        closed -> nsock:close(Sock), {error,closed}
        end;
    {error,E} -> {error,E}
    end.
