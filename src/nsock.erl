-module(nsock).
-author('Brian Picciano').
-include("nemo.hrl").
-compile(export_all).

gen(Sock,Data) ->
	try gen_tcp:send(Sock,[Data,$\n]) of
    ok -> sent;
    {error,_} -> closed
    catch _:_ -> closed
    end.

gen_json(Sock,Data) ->
	?MODULE:gen(Sock,mochijson2:encode(Data)).

success(Sock,Command) -> ?MODULE:success(Sock,Command,ok).
success(Sock,Command,SuccessMessage) ->
    ?MODULE:gen_json(Sock,[{command,Command},{success,SuccessMessage}]).

error(Sock,Command,Error) ->
    ?MODULE:gen_json(Sock,[{command,Command},{error,Error}]).
error(Sock,Command,Error,Extra) ->
    ?MODULE:gen_json(Sock,[{command,Command},{error,Error}|Extra]).

close(Sock) -> gen_tcp:close(Sock).
