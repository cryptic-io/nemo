-module(nuploadhandler).
-author('Brian Picciano').
-include("nemo.hrl").
-compile(export_all).

get_sock(_) ->
    receive
    {ohaithar,Sock} -> ?MODULE:process_sock(Sock)
    after 10000 -> throw(get_sock_timeout)
    end.

process_sock(Sock) ->
    case inet:peername(Sock) of
    {ok, {Ip,_Port}} ->
        case nutil:check_ip_sudo(Ip) of
        false -> do_nothing;
        true  -> ?MODULE:process_sock_loop(Sock)
        end;
    _ -> oh_well
    end,

    %Putting this here makes the process_sock_loop call not tail recursive,
    %so hot-reloading won't work. But that shouldn't matter, these upload
    %connections should be short-lived anyway.
    nsock:close(Sock).

process_sock_loop(Sock) ->
    try inet:setopts(Sock,[{active,once}]) of
    ok ->
        receive
        {tcp_closed,_} -> exit;
        {tcp_error,_,etimedout} -> exit;
        {tcp,Sock,Data} ->
            {FileName,Size,Leftover} = ?MODULE:get_name(Data),

            case ndb:insert_partial(FileName) of
            stopped -> file_exists;
            ok ->

                case nfile:pipe_sock_to_file(Sock,FileName,Size,Leftover) of
                ok -> nfs:add_whole_file(FileName);
                {error,E} -> error_logger:error_msg("Error piping file to file: ~p\n",[E])
                end

            end;

        _ -> exit
        after 60000 ->
            ?MODULE:process_sock_loop(Sock)
        end;
    _ -> oh_well
    catch _:_ -> oh_well
    end.

get_name(Data) ->
    [FileName,Rest] = binary:split(Data,<<10>>),
    [SizeStr,Leftover] = binary:split(Rest,<<10>>),
    {FileName,list_to_integer(binary_to_list(SizeStr)),Leftover}.
