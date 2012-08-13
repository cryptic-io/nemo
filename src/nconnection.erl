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
                case ?MODULE:handle_datas(Sock,FullDatas,S) of
                die -> ?MODULE:prepare_for_death(Sock,S);
                NewS -> ?MODULE:process_sock_loop(Sock,NewS)
                end
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
-define(COMMAND_EXTRACT,[
                            {<<"meta">>, {struct,required,true}},
                            {<<"command">>,{binary,required,true}}
                        ]).
-define(META_EXTRACT,[
                            {<<"http">>, {bool, false, true}}
                     ]).
handle_data(Socket,Data,S) ->
    case Data of
	<<"{",_/binary>> ->
        %Do stuff
        Ret = try mochijson2:decode(Data) of
            {struct,Struct} ->
                case nrpc:extract(Struct,?COMMAND_EXTRACT) of
                [{_,Command},{_,MetaStruct}] -> 
                    case nrpc:extract(MetaStruct, ?META_EXTRACT) of 
                    ErrorObj = {error,_,_} -> ErrorObj;
                    Meta -> 
                        ?MODULE:command_dispatch(Socket,Struct,Meta,S,Command)
                    end;
                ErrorObj -> ErrorObj
                end;
            _ -> {error,bad_json}
        catch
            _:_ -> {error,bad_json}
        end,

        %If there were any errors in stuff, send error
        case Ret of
        {error,E} -> nsock:error(Socket,false,E), S;
        {error,E,Extra} -> nsock:error(Socket,false,E,Extra), S;
        _ -> Ret
        end;

    _ -> S
	end.

command_dispatch(Socket,Struct,Meta,S,Command) ->
    Ret = case {S#conn_state.sudo,Command} of
    {true,<<"addFileKey">>} ->  ?MODULE:command_addFileKey(Socket,Struct,Meta,S);
    {_,<<"downloadFile">>} ->   ?MODULE:command_downloadFile(Socket,Struct,Meta,S);
    _ ->                        {error, unknown_command}
    end,

    NS = case Ret of
    {error,Error} -> nsock:error(Socket,Command,Error), S;
    {error,Error,Extra} -> nsock:error(Socket,Command,Error,Extra), S;
    _ -> Ret
    end,

    case nutil:keyfind(<<"http">>,Meta) of
    true -> die;
    false -> NS
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(ADDFILEKEY_EXTRACT,[
                                {<<"filename">>,{binary,required,true}},
                                {<<"key">>,{binary,required,true}}
                           ]).
command_addFileKey(Socket,Struct,Meta,S) ->
    nutil:keyfind(<<"http">>,Meta) andalso 
        ?MODULE:print_http_headers(Socket,json),
    case nrpc:extract(Struct,?ADDFILEKEY_EXTRACT) of
    {error,Error,Extra} -> {error,Error,Extra};
    [{_,Key},{_,FileName}] ->
        ndb:add_key(FileName,Key),
        nsock:success(Socket,<<"addFileKey">>),
        S
    end.

-define(DOWNLOADFILE_EXTRACT,[
                                {<<"filename">>,{binary,required,true}},
                                {<<"key">>,{binary,required,true}}
                             ]).
command_downloadFile(Socket,Struct,Meta,S) ->
    Http = nutil:keyfind(<<"http">>,Meta),
    case nrpc:extract(Struct,?DOWNLOADFILE_EXTRACT) of
    [{_,Key},{_,FileName}] -> 
        case ndb:get_file_for_key(Key) of
        FileName ->
            case nfile:file_size(FileName) of
            {error,E} -> 
                Http andalso ?MODULE:print_http_headers(Socket,json),
                {error,E};
            Size ->
                Http andalso ?MODULE:print_http_headers(Socket,{binary,Size}),
                case nfile:pipe_file(Socket,FileName) of
                {error,E} -> {error,E};
                {error,E,Ex} -> {error,E,Ex};
                success -> 
                    spawn(ndb,delete_key,[Key]), S
                end
            end;
        _ -> 
            Http andalso ?MODULE:print_http_headers(Socket,json),
            {error,bad_key}
        end;
    {error,Error,Extra} -> 
        Http andalso ?MODULE:print_http_headers(Socket,json),
        {error,Error,Extra}
    end.


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

%Print http headers for different kinds of returns
print_http_headers(Sock,json) ->
    gen_tcp:send(Sock,[
        "HTTP/1.1 OK\r\n",
        "Content-Type: application/json\r\n",
        "Server: nemo\r\n",
        "\r\n"
    ]);
print_http_headers(Sock,{binary,Size}) ->
    gen_tcp:send(Sock,[
        "HTTP/1.1 OK\r\n",
        "Content-Type: application/octet-stream\r\n",
        "Content-Length: ",integer_to_list(Size),"\r\n",
        "Server: nemo\r\n",
        "\r\n"
    ]).
