%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan
%%% @copyright (c) 2018, Mathieu Kerjouan <mk [at] steepath.eu>
%%% @doc documentation about this module
%%%      ...
%%% @end
%%%-------------------------------------------------------------------

-module(spaceinvader_server).
-compile([export_all]).

init(Port) -> 
    {ok, Socket} = gen_tcp:listen(Port, [binary, {port, Port}]),
    loop(Socket).

start() -> 
    erlang:spawn(fun() -> init(7777) end).
start(Port) ->
    erlang:spawn(fun() -> init(Port) end).

loop(Socket) ->
    {ok, Accept} = gen_tcp:accept(Socket),
    _ = spawn(fun() -> acceptor(Accept) end),
    loop(Socket).

acceptor(Socket) ->
    erlang:port_connect(Socket, self()),
    receive 
	exit -> gen_tcp:close(Socket);
	_Else -> io:format("~p~n", [_Else]),
		 acceptor(Socket)
    end.
