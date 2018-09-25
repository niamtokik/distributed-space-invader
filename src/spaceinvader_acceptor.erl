-module(spaceinvader_acceptor).
-compile([export_all]).

init(Socket) ->
    erlang:port_connect(Socket, self()),
    loop(Socket).

loop(Socket) ->
    receive
        {tcp, _Port, Message} ->
            io:format("received message: ~p~n", [Message]),
            forward(Message),
            loop(Socket);
        {tcp_closed, Port} ->
            gen_tcp:close(Socket),
            io:format("Close connection ~p from ~p~n", [Port, self()]);
        _Else -> io:format("~p~n", [_Else]),
                 forward(_Else),
                 loop(Socket)
    end.

send_message(Node, Message) ->
    gen_server:cast({spaceinvader_relay, Node}, { node(), Message }).

forward(Message) ->
    [ send_message(Node, Message) || Node <- erlang:nodes() ].
