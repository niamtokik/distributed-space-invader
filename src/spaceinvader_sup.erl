%%%--------------------------------------------------------------------
%%% @doc Usually, all *_sup.erl files are supervisors based on
%%%      OTP supervisor behavior. A supervisor is a specific process
%%%      responsible to monitor, and supervise one or more
%%%      processes. If a process die in an unexpected way, the
%%%      supervisor can restart it and do some other actions.  
%%% 
%%%      See also:
%%%      <ul><li>[http://erlang.org/doc/design_principles/sup_princ.html]</li>
%%%      </ul>
%%% @end
%%%--------------------------------------------------------------------
-module(spaceinvader_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

% Here, we define a macro based on another macro ?SERVER is equivalent
% to ?MODULE.
-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% @doc start_link function is an helper function and can refer
%%      to two other function, supervisor:start_link/2 and
%%      supervisor:start_link/3. This is the usual function to start a
%%      supervisor and will return the supervisor pid.
%%
%%      See also:
%%      <ul><li>[http://erlang.org/doc/man/supervisor.html#start_link-2]</li>
%%          <li>[http://erlang.org/doc/man/supervisor.html#start_link-3]</li>
%%      </ul>
%% @end
%%--------------------------------------------------------------------
-spec start_link() 
                -> {ok, pid()} | 
                   ignore | 
                   {error
                   , {already_started, pid()} | {shutdown, term()} | term()
                   }.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%--------------------------------------------------------------------
%% @doc Init function is a callback used by supervisor
%%      behavior. This function will initialize the supervisor with
%%      required information (e.g. processes names, restart rules and
%%      many other variables).
%%
%%      See also:
%%      <ul><li>[http://erlang.org/doc/man/supervisor.html#Module:init-1]</li>
%%      </ul>
%% @end
%%--------------------------------------------------------------------
init(_Args) ->
    {ok
    ,{ sup_flags()
     , workers() 
     } 
    }.

%%--------------------------------------------------------------------
%% @doc sup_flags will initialize the behaviour of the supervisor
%%      with the restart intensity based on the period of crash but
%%      also the kind of strategy (method) to restart a child.
%%
%%      See also:
%%      <ul><li>[http://erlang.org/doc/man/supervisor.html#type-sup_flags]</li>
%%      </ul>
%% @end
%%--------------------------------------------------------------------
-spec sup_flags() -> map().
sup_flags() ->
    #{ strategy => one_for_one
     , intensity => 1
     , period => 1
     }.

%%--------------------------------------------------------------------
%% @doc A supervisor need a list of worker to supervise, except
%%      for the simple_one_for_one method (this one can spawn
%%      automatically and on demand one type of worker/supervisor). In
%%      our case, actually, we only want to start one worker: our
%%      spaceinvader_listener.
%%
%%      See also:
%%      <ul><li>[http://erlang.org/doc/man/supervisor.html#type-child_spec]</li>
%%      </ul>
%% @end
%%--------------------------------------------------------------------
-spec workers() -> [map(), ...].
workers() ->
    [spaceinvader_listener()
    ,spaceinvader_relay()
    ].

%%--------------------------------------------------------------------
%% @doc spaceinvader_listener refer to our main worker, here, the
%%      spaceinvader_listener module. This one will be monitored by this
%%      supervisor, if something goes wrong, it will automatically restart
%%      it.  
%% 
%%      See also:
%%      <ul><li>http://erlang.org/doc/man/supervisor.html#type-child_spec</li>
%%      </ul>
%% @end
%%--------------------------------------------------------------------
-spec spaceinvader_listener() -> map().
spaceinvader_listener() ->
    #{ id => spaceinvader_listener
     , start => {spaceinvader_listener, start_link, []}
     , restart => permanent
     , shutdown => brutal_kill
     , type => worker
     }.

%%--------------------------------------------------------------------
%% @doc spaceinvader_relay/0 is another worker, based on our
%%      spaceinvader_relay. This one will get messages from outside
%%      world and send them to acceptor.
%% @end
%%--------------------------------------------------------------------
-spec spaceinvader_relay() -> map().
spaceinvader_relay() ->
    #{ id => spaceinvader_relay
     , start => {spaceinvader_relay, start_link, []}
     , restart => permanent
     , type => worker
     }.
