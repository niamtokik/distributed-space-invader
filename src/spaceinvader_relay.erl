-module(spaceinvader_relay).
-behaviour(gen_server).
-compile(export_all).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

init(_Args) ->
    {ok, []}.

handle_cast({Node, Message}, State) ->
    {noreply, State}.

handle_call(Message, From, State) ->
    {reply, ok, State}.

handle_info(Message, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    ok.
    
t(Message) ->
    [ gen_server:cast({?MODULE, Node}, { erlang:node(), Message }) || Node <- erlang:nodes() ].
