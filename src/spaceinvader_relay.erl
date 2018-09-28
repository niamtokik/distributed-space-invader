%%%-------------------------------------------------------------------
%%% @doc spaceinvader_relay is our registered relay on each
%%%      nodes. This process will receive message from the other node
%%%      and forward them to the appropriate process. This code follow
%%%      OTP principle using gen_server behaviour.
%%%
%%%      See also:
%%%      <ul><li>[http://erlang.org/doc/man/gen_server.html]</li>
%%%          <li>[http://erlang.org/doc/design_principles/gen_server_concepts.html]</li>
%%%      </ul>
%%% @end
%%%-------------------------------------------------------------------
-module(spaceinvader_relay).
-behaviour(gen_server).
-export([start/0, start_link/0, stop/0]).
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([register/1, unregister/1]).
-export([forwarded/0, received/0, registered/0]).

%%--------------------------------------------------------------------
%% @doc start/0 is a function helper to start automatically 
%%      spaceinvader_relay process based on default gen_server
%%      handler.
%%
%%      See also:
%%      <ul><li>[http://erlang.org/doc/man/gen_server.html#start-3]</li>
%%          <li>[http://erlang.org/doc/man/gen_server.html#start-4]</li>
%%      </ul>
%% @end
%%--------------------------------------------------------------------
-spec start() -> {ok, pid()}.
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc start_link/0, like start/0 function, but automatically link
%%      our process to another one. This one is used mainly with
%%      supervisor.
%%
%%      See also:
%%      <ul><li>[http://erlang.org/doc/man/gen_server.html#start_link-3]</li>
%%          <li>[http://erlang.org/doc/man/gen_server.html#start_link-4]</li>
%%      </ul>
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc stop the spaceinvader_relay process.
%%
%%      See also:
%%      <ul><li>[http://erlang.org/doc/man/gen_server.html#stop-1]</li>
%%          <li>[http://erlang.org/doc/man/gen_server.html#stop-3]</li>
%%      </ul>
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%%--------------------------------------------------------------------
%% @doc This function is actually not used in our code.
%% 
%%      See also:
%%      <ul><li>[http://erlang.org/doc/man/gen_server.html#Module:code_change-3]</li>
%%      </ul>
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, _State, _Extra) ->
    {ok, []}.

%%--------------------------------------------------------------------
%% @doc init/1 is a callback. Like any other OTP behavior, a gen_server
%%      must be initialized. This initialization phase give us the
%%      ability to define our state properly before starting our 
%%      process.
%%
%%      See also:
%%      <ul><li>[http://erlang.org/doc/man/gen_server.html#Module:init-1]</li>
%%      </ul>
%% @end
%%--------------------------------------------------------------------
-spec init(term()) -> {ok, map()}.
init(_Args) ->
    {ok, #{ forwarded => 0 
          , received => 0 
          , registered => [] }
    }.

%%--------------------------------------------------------------------
%% @doc handle_cast/2 is a callback. This function will receive
%%      cast message (without connection) from any other process and
%%      will always return ok. We don't know if the message was
%%      correctly received. The raw received message is a standardize
%%      OTP message.  
%%
%%      See also:
%%      <ul><li>[http://erlang.org/doc/man/gen_server.html#Module:handle_cast-2]</li>
%%      </ul>
%% @end
%%--------------------------------------------------------------------
-spec handle_cast({pid(), atom(), term()}, map()) -> {noreply, map()};
                 (term(), map()) -> {noreply, map()}.
handle_cast({register, Pid}, State) 
  when is_pid(Pid) ->
    io:format("register pid ~p~n", [Pid]),
    #{ registered := Registered } = State,
    Added = Registered ++ [Pid],
    {noreply, State#{ registered => Added }};
handle_cast({unregister, Pid}, State) 
  when is_pid(Pid) ->
    io:format("unregister pid ~p~n", [Pid]),
    #{ registered := Registered } = State,
    Removed = [ X || X <- Registered, X =/= Pid ],
    {noreply, State#{ registered => Removed }};
handle_cast({_Pid, Node, Message}, State) ->
    io:format("received message ~p from ~p~n", [Message, Node]),
    {noreply, State};
handle_cast(_Else, State) ->
    io:format("handle_cast: received ~p~n", [_Else]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc handle_call/3 is a callback. This is a connected
%%      function, this one can return some data to the client. 
%%
%%      See also:
%%      <ul><li>[http://erlang.org/doc/man/gen_server.html#Module:handle_call-3]</li>
%%      </ul>
%% @end
%%--------------------------------------------------------------------
-spec handle_call({atom(), atom()}, pid(), map()) 
                 -> {reply, {ok, integer()}, map()};
                 (term(), pid(), map()) 
                 -> {reply, ok, map()}.
handle_call({get, forwarded}, _From, State) ->
    #{ forwarded := Forwarded} = State,
    {reply, {ok, Forwarded}, State};
handle_call({get, received}, _From, State) ->
    #{ received := Received } = State,
    {reply, {ok, Received}, State};
handle_call({get, registered}, _From, State) ->
    #{ registered := Registered } = State,
    {reply, {ok, Registered}, State};
handle_call(Message, _From, State) ->
    io:format("handle_call: received message ~p from ~p~n", [Message, _From]),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @doc handle_info/2 is a callback. This function will receive
%%      all non OTP messages. For example, if you have a TCP socket
%%      running, you will probably have some tcp message
%%      (non-OTP). Those messages will be available in this function.
%%
%%      See also:
%%      <ul><li>[http://erlang.org/doc/man/gen_server.html#Module:handle_info-2]</li>
%%      </ul>
%% @end
%%--------------------------------------------------------------------
-spec handle_info(term(), map()) -> {noreply, map()}.
handle_info(Message, State) ->
    io:format("handle_info: received ~p~n", [Message]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc terminate/2 is a callback. This function is like a destructor
%%      in OOP, and will clean states.
%%
%%      See also:
%%      <ul><li>[http://erlang.org/doc/man/gen_server.html#Module:terminate-2]</li>
%%      </ul>
%% @end
%%--------------------------------------------------------------------
-spec terminate(term(), map()) -> ok.
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @doc cast is a function helper to call gen_server:cast/2 function
%%      with right value (in our case, our gen_server is registered
%%      with its module name).
%%
%%      More information:
%%      <ul><li>[http://erlang.org/doc/man/gen_server.html#cast-2]</li>
%%      </ul>
%% @end
%%--------------------------------------------------------------------
-spec cast(Message :: term()) -> ok.
cast(Message) ->
    gen_server:cast(?MODULE, Message).

%%--------------------------------------------------------------------
%% @doc call is a function helper and will call gen_server:call/2 or
%%      gen_server:call/3.
%%
%%      More information:
%%      <ul><li>[http://erlang.org/doc/man/gen_server.html#call-2]</li>
%%          <li>[http://erlang.org/doc/man/gen_server.html#call-3]</li>
%%      </ul>
%% @end
%%--------------------------------------------------------------------
-spec call(Message :: term()) -> term().
call(Message) ->
    gen_server:call(?MODULE, Message).

-spec call(Message :: term(), Timeout :: integer()) -> term().
call(Message, Timeout) ->
    gen_server:call(?MODULE, Message, Timeout).

%%--------------------------------------------------------------------
%% @doc register/1 is an API to register easily a PID (in our case an
%%      acceptor) in our relay. Relay must keep a list of acceptor
%%      to forward messages.
%% @end
%%--------------------------------------------------------------------
-spec register(Pid :: pid()) -> ok.
register(Pid) 
  when is_pid(Pid) ->
    cast({register, Pid}).

%%--------------------------------------------------------------------
%% @doc unregister/1 is a function API to remove a pid from register
%%      process list.
%% @end
%%--------------------------------------------------------------------
-spec unregister(Pid :: pid()) -> ok.
unregister(Pid) 
  when is_pid(Pid) ->
    cast({unregister, Pid}).

%%--------------------------------------------------------------------
%% @doc registered/0 get the list of all registered pid.
%% @end
%%--------------------------------------------------------------------
-spec registered() -> {ok, [pid(), ...]}.
registered() ->
    call({get, registered}).

%%--------------------------------------------------------------------
%% @doc forwarded/0 is a function API to get the number of forwarded
%%      connections.
%% @end
%%--------------------------------------------------------------------
-spec forwarded() -> {ok, integer()}.
forwarded() ->
    call({get, forwarded}).

%%--------------------------------------------------------------------
%% @doc received/0 is a function API to get the number of received
%%      connection.
%% @end
%%--------------------------------------------------------------------
-spec received() -> {ok, integer()}.
received() ->
    call({get, received}).
