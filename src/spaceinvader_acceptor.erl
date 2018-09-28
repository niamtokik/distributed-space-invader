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
        {tcp, Port, Message} ->
            tcp(Socket, Port, Message);
        {tcp_closed, Port} ->
            tcp_closed(Socket, Port);
        {message, Message} ->
            from_relay(Socket, Message);
        _Else -> io:format("~p~n", [_Else]),
                 forward(_Else),
                 loop(Socket)
    end.

%%--------------------------------------------------------------------
%% @doc tcp/3 function will catch all standard tcp message.
%% @end
%%--------------------------------------------------------------------
tcp(Socket, Port, Message) 
  when is_port(Port) ->
    io:format("received message: ~p~n", [Message]),
    io:format("received message: ~p~n", [convert(Message)]),
    forward(Message),
    loop(Socket).

%%--------------------------------------------------------------------
%% @doc tcp_closed/2 will close the socket and unregister the process
%%      on the relay.
%% @end
%%--------------------------------------------------------------------
tcp_closed(Socket, Port) 
  when is_port(Port) ->
    gen_tcp:close(Socket),
    spaceinvader_relay:unregister(self()),
    io:format("Close connection ~p from ~p~n", [Port, self()]).

%%--------------------------------------------------------------------
%% @doc from_relay/2 will catch messages from the relay and send them
%%      to the accepted socket.
%% @end
%%--------------------------------------------------------------------
from_relay(Socket, Message) ->
    Convert = erlang:term_to_binary(Message),
    Size = erlang:bit_size(Convert),
    Send = <<Size:32, Convert/bitstring>>,
    io:format("send: ~p~n", [Send]),
    gen_tcp:send(Socket, Send),
    loop(Socket).

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

%%--------------------------------------------------------------------
%% @doc convert/1 function take BERT message as binary string, extract
%%      size and convert term, returning it. If something goes wrong
%%      we catch and return an error with the reason.
%%
%%      See also:
%%      <ul><li>[http://bert-rpc.org/]</li>
%%      </ul>
%% @end
%%--------------------------------------------------------------------
-spec convert(Bitstring :: bitstring()) 
             -> {integer(), term()} | {error, atom()}.
convert(<<Size:32, Message/bitstring>>) ->
    try erlang:binary_to_term(Message) of
        Term -> {Size, Term}
    catch
         _:_ -> {error, not_bert}
    end.
