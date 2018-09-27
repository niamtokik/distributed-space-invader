%%%-------------------------------------------------------------------
%%% spaceinvader_listener is our TCP listener, listening by default on
%%% TCP/7777 port. It can be started manually by calling start/0,
%%% start/1 or start/2 functions. This code doesn't follow OTP by
%%% design (its an example).
%%%-------------------------------------------------------------------
-module(spaceinvader_listener).
-export([start/0, start/1, start/2]).
-export([start_link/0, start_link/1, start_link/2]).

%%--------------------------------------------------------------------
%% start/0, start/1 and start/2 functions create a new listener
%% process by default listening on port TCP/7777.
%%--------------------------------------------------------------------
-spec start() -> {ok, pid()}.
start() ->
    start(7777).

-spec start(Port :: integer()) -> {ok, pid()}.
start(Port) ->
    start(Port, [{port, 7777}]).

-spec start(Port :: integer(), Args :: term()) -> {ok, pid()}.
start(Port, Args) ->
    {ok, spawn(fun() -> init(Port, Args) end)}.

%%--------------------------------------------------------------------
%% start/0, start/1 and start/2 functions create a new listener
%% process by default listening on port TCP/7777.
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()}.
start_link() ->
    start_link(7777).

-spec start_link(Port :: integer()) -> {ok, pid()}.
start_link(Port) ->
    start_link(Port, [{port, 7777}]).

-spec start_link(Port :: integer(), Args :: term()) -> {ok, pid()}.
start_link(Port, Args) ->
    {ok, spawn_link(fun() -> init(Port, Args) end)}.

%%--------------------------------------------------------------------
%% init/2 will create our listening socket with gen_tcp:listen/2
%% function on port TCP/7777 by default.
%%
%% More information here:
%%   http://erlang.org/doc/man/gen_tcp.html#listen-2
%%--------------------------------------------------------------------
-spec init(Port :: integer(), Args :: term()) -> none().
init(Port, Args) ->
    {ok, Socket} = gen_tcp:listen(Port, Args),
    loop(Socket).

%%--------------------------------------------------------------------
%% loop/1 function is our main loop. When a connection arrives from
%% the outside world, our code will automatically accept it and spawn
%% a new acceptor process with the new opened port (accepted socket).
%% This code doesn't catch kind of exception, and should be considered
%% as an example.
%%
%% More information here:
%%   http://erlang.org/doc/man/gen_tcp.html#accept-1
%%--------------------------------------------------------------------
-spec loop(Socket :: port()) -> none().
loop(Socket) ->
    {ok, AcceptedSocket} = gen_tcp:accept(Socket),
    {ok, _Pid} = acceptor(AcceptedSocket),
    io:format("connection ~p accepted by ~p~n",[AcceptedSocket, _Pid]),
    loop(Socket).

%%--------------------------------------------------------------------
%% acceptor/1 is dedicated to spawn an acceptor process.
%%--------------------------------------------------------------------
-spec acceptor(Socket :: port()) -> {ok, pid()}.
acceptor(Socket) ->
    Fun = fun() -> spaceinvader_acceptor:init(Socket) end,
    Pid = spawn(Fun),
    {ok, Pid}.
