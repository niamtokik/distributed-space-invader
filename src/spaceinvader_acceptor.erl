%%%-------------------------------------------------------------------
%%% @doc spaceinvader_acceptor is our main tcp acceptor
%%%      module. This one will got the new accepted socket and do all
%%%      routing an processing.  This code, like
%%%      spaceinvader_listener, doesn't follow OTP principle, in this
%%%      case, this code can't be supervised as it by any standard OTP
%%%      supervisor.
%%%
%%%      More information:
%%%      <ul><li>[http://erlang.org/doc/man/gen_tcp.html#accept-1]</li>
%%%          <li>[http://erlang.org/doc/man/gen_tcp.html#accept-2]</li>
%%%      </ul>
%%% @end
%%%-------------------------------------------------------------------
-module(spaceinvader_acceptor).
-export([init/1]).

%%--------------------------------------------------------------------
%% @doc init/1 get a socket from spaceinvader_listener loop. By
%%      default this socket is directly connected to our listener
%%      process, its why we need to explicitely connect this new
%%      acceptor process to the socket passed in argument.
%% @end
%%--------------------------------------------------------------------
-spec init(Socket :: port()) -> none().
init(Socket) ->
    erlang:port_connect(Socket, self()),
    spaceinvader_relay:register(self()),
    loop(Socket).

%%--------------------------------------------------------------------
%% @doc loop/1 is our main loop, containing all information about our
%%      routing TCP messages and actions to do.
%% @end
%%--------------------------------------------------------------------
-spec loop(Socket :: port()) -> none().
loop(Socket) ->
    receive
        {tcp, _Port, Message} ->
            io:format("received message: ~p~n", [Message]),
            forward(Message),
            loop(Socket);
        {tcp_closed, Port} ->
            gen_tcp:close(Socket),
            spaceinvader_relay:unregister(self()),
            io:format("Close connection ~p from ~p~n", [Port, self()]);
        _Else -> io:format("~p~n", [_Else]),
                 forward(_Else),
                 loop(Socket)
    end.

%%--------------------------------------------------------------------
%% @doc message/1 craft a message to send to other nodes
%% @end
%%--------------------------------------------------------------------
-spec message(Message :: term()) -> tuple().
message(Message) ->
    {self(), node(), Message}.

%%--------------------------------------------------------------------
%% @doc send_message/2 will send a message to a specific
%%      node. This node must have a registered process named
%%      spaceinvader_relay, if not, message is lost in the wild.
%% @end
%%--------------------------------------------------------------------
-spec send_message(Node :: node(), Message :: term()) -> ok.
send_message(Node, Message) ->
    Data = message(Message),
    gen_server:cast({spaceinvader_relay, Node}, Data).

%%--------------------------------------------------------------------
%% @doc forward/1 is a function helper to send a message to all
%%      connected nodes.  
%% @end
%%--------------------------------------------------------------------
-spec forward(Message :: term()) -> list().
forward(Message) ->
    [ send_message(Node, Message) || Node <- erlang:nodes() ].
