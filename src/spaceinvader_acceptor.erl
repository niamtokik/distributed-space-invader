%%%-------------------------------------------------------------------
%%% spaceinvader_acceptor is our main tcp acceptor module. This one
%%% will got the new accepted socket and do all routing an processing.
%%% This code, like spaceinvader_listener, doesn't follow OTP
%%% principle, in this case, this code can't be supervised as it by
%%% any standard OTP supervisor.
%%%-------------------------------------------------------------------
-module(spaceinvader_acceptor).
-export([init/1]).

%%--------------------------------------------------------------------
%% init/1 get a socket from spaceinvader_listener loop. By default
%% this socket is directly connected to our listener process, its why
%% we need to explicitely connect this new acceptor process to the
%% socket passed in argument.
%%--------------------------------------------------------------------
-spec init(Socket :: port()) -> none().
init(Socket) ->
    erlang:port_connect(Socket, self()),
    loop(Socket).

%%--------------------------------------------------------------------
%% loop/1 is our main loop, containing all information about our
%% routing TCP messages and actions to do.
%% --------------------------------------------------------------------
-spec loop(Socket :: port()) -> none().
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

%%--------------------------------------------------------------------
%% send_message/2 will send a message to a specific node. This node
%% must have a registered process named spaceinvader_relay, if not,
%% message is lost in the wild.
%%--------------------------------------------------------------------
-spec send_message(Node :: node(), Message :: term()) -> ok.
send_message(Node, Message) ->
    gen_server:cast({spaceinvader_relay, Node}, { node(), Message }).

%%--------------------------------------------------------------------
%% forward/1 is a function helper to send a message to all connected
%% nodes.
%%--------------------------------------------------------------------
-spec forward(Message :: term()) -> list().
forward(Message) ->
    [ send_message(Node, Message) || Node <- erlang:nodes() ].
