-module(nconnection).
-author('Brian Picciano').
-include("nemo.hrl").
-compile(export_all).

%Gets the sock, sent from an accept_loop. this pid will already
%be the controlling process by the time this happens, so the race
%condition can't happen
get_sock() ->
    receive
    {ohaithar,Sock} -> ?MODULE:process_sock(Sock)
    after 10000 -> throw(get_sock_timeout)
    end.
    
%Processes sock, determining its originating IP before beginning the loop
process_sock(Sock) ->
    case inet:peername(Sock) of
    {ok,{Ip,_Port}} -> 
        Sudo = nutil:check_ip_sudo(Ip),
        ?MODULE:process_sock_loop(Sock,#conn_state{ip=Ip,sudo=Sudo});
    _ -> nsock:close(Sock)
    end.


%Processes data coming from a socket
process_sock_loop(Sock,S) ->
    try inet:setopts(Sock,[{active,once}]) of
    ok ->
        receive
        {tcp_closed,_} -> 
            ?MODULE:prepare_for_death(Sock,S);
        {tcp_error,_,etimedout} -> 
            ?MODULE:prepare_for_death(Sock,S);
        {tcp,Sock,Data} ->
            case ?MODULE:pull_data(Sock,Data) of
            error -> ?MODULE:prepare_for_death(Sock,S);
            FullDatas ->
                NewS = ?MODULE:handle_datas(Sock,FullDatas,S),
                ?MODULE:process_sock_loop(Sock,NewS)
            end;
        _ ->
            ?MODULE:process_sock_loop(Sock,S)
        after 60000 ->
            ?MODULE:process_sock_loop(Sock,S)
        end;
    {error,_} ->
        ?MODULE:prepare_for_death(Sock,S) 
    catch _:_ -> 
        ?MODULE:prepare_for_death(Sock,S)
    end.

%Prepares for a generic close of the handler, in case
%we ever need to do anything but close the connection
prepare_for_death(Sock,_S) -> 
    nsock:close(Sock).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Data handling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_datas(_,[],S) -> S;
handle_datas(Sock,[Data|Datas],S) ->
    NewS = ?MODULE:handle_data(Sock,Data,S),
    ?MODULE:handle_datas(Sock,Datas,NewS).

%Handles data that's come in from the tcp socket.
%Promises to loop back to process_sock_loop
-define(COMMAND_EXTRACT,[{<<"command">>,{binary,required,true}}]).
handle_data(Socket,Data,S) ->
    case Data of
	<<"{",_/binary>> ->
        %Do stuff
        Ret = try mochijson2:decode(Data) of
            {struct,Struct} ->
                case nrpc:extract(Struct,?COMMAND_EXTRACT) of
                [{<<"command">>,Command}] ->
                    io:fwrite("Command is ~s\n",[Command]),
                    S;
                ErrorObj -> ErrorObj
                end;
            _ -> {error,bad_json}
        catch
            _:_ -> {error,bad_json}
        end,

        %If there were any errors in stuff, send error
        case Ret of
        {error,E} -> nsock:error(Socket,false,E), S;
        {error,E,Key} -> nsock:error(Socket,false,E,Key), S;
        _ -> Ret
        end;

    _ -> S
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Data pulling and processing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Attemps to trim a newline off the raw data and return it.
%If there is no newline, assumes there is still more data
%to get and attempts to get it and trim that. If there's
%no data after a half second, assumes that was all the data
pull_data(Sock,Data) -> binary:split(?MODULE:pull_data2(Sock,Data),<<10>>,[global]).
pull_data2(Sock,Data) ->
    L = size(Data) - 1,
    case Data of
    << Trimmed:L/binary, 10 >> ->
        Trimmed;
    _ ->
        try inet:setopts(Sock,[{active,once}]) of
        ok ->
            receive
                {tcp,Sock,MoreData} -> 
                    ?MODULE:pull_data2(Sock,<<Data/binary,MoreData/binary>>)
            after 1000 ->
                Data
            end;
        {error,_} -> error
        catch _:_ -> error
        end
    end.
