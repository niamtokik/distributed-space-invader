-module(spaceinvader_listener).
-compile([export_all]).

start() ->
    start(7777).

start(Port) ->
    start(Port, [{port, 7777}]).

start(Port, Args) ->
    spawn(fun() -> init(Port, Args) end).

init(Port, Args) ->
    {ok, Socket} = gen_tcp:listen(Port, Args),
    loop(Socket).

loop(Socket) ->
    {ok, AcceptedSocket} = gen_tcp:accept(Socket),
    _Pid = spawn(fun() -> spaceinvader_acceptor:init(AcceptedSocket) end),
    io:format("connection ~p accepted by ~p~n",[AcceptedSocket, _Pid]),
    loop(Socket).

    


