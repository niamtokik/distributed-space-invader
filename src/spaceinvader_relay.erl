%%%-------------------------------------------------------------------
%%% spaceinvader_relay is our registered relay on each nodes. This
%%% process will receive message from the other node and forward them
%%% to the appropriate process. This code follow OTP principle using
%%% gen_server behaviour.
%%%
%%% More information about gen_statem here:
%%%   http://erlang.org/doc/man/gen_server.html
%%%   http://erlang.org/doc/design_principles/gen_server_concepts.html
%%%-------------------------------------------------------------------
-module(spaceinvader_relay).
-behaviour(gen_server).
-compile(export_all).

%%--------------------------------------------------------------------
%% More information here:
%%   http://erlang.org/doc/man/gen_server.html#start-3
%%   http://erlang.org/doc/man/gen_server.html#start-4
%%--------------------------------------------------------------------
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% More information here:
%%   http://erlang.org/doc/man/gen_server.html#start_link-3
%%   http://erlang.org/doc/man/gen_server.html#start_link-4
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% More information here:
%%   http://erlang.org/doc/man/gen_server.html#stop-1
%%   http://erlang.org/doc/man/gen_server.html#stop-3
%%--------------------------------------------------------------------
stop() ->
    gen_server:stop(?MODULE).

%%--------------------------------------------------------------------
%% This function is actually not used in our code.
%%
%% More information here:
%%   http://erlang.org/doc/man/gen_server.html#Module:code_change-3
%%--------------------------------------------------------------------
code_change(_OldVsn, _State, _Extra) ->
    {ok, []}.

%%--------------------------------------------------------------------
%% init/1 is a callback. Like any other OTP behavior, a gen_server
%% must be initialized. This initialization phase give us the ability
%% to define our state properly before starting our process.
%%
%% More information here:
%%   http://erlang.org/doc/man/gen_server.html#Module:init-1
%%--------------------------------------------------------------------
init(_Args) ->
    {ok, []}.

%%--------------------------------------------------------------------
%% handle_cast/2 is a callback. This function will receive cast
%% message (without connection) from any other process and will always
%% return ok. We don't know if the message was correctly
%% received. The raw received message is a standardize OTP message.
%%
%% More information here:
%%   http://erlang.org/doc/man/gen_server.html#Module:handle_cast-2
%%--------------------------------------------------------------------
handle_cast({Node, Message}, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% handle_call/3 is a callback. This is a connected function, this one
%% can return some data to the client.
%%
%% More information here:
%%   http://erlang.org/doc/man/gen_server.html#Module:handle_call-3
%%--------------------------------------------------------------------
handle_call(Message, From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% handle_info/2 is a callback. This function will receive all non OTP
%% messages. For example, if you have a TCP socket running, you will
%% probably have some tcp message (non-OTP). Those messages will be
%% available in this function.
%%
%% More information here:
%%   http://erlang.org/doc/man/gen_server.html#Module:handle_info-2
%%--------------------------------------------------------------------
handle_info(Message, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% terminate/2 is a callback. This function is like a destructor in
%% OOP, and will clean states.
%%
%% More information here:
%%   http://erlang.org/doc/man/gen_server.html#Module:terminate-2
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

