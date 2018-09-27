%%%-------------------------------------------------------------------
%%% @doc Usually, an _*app.erl file will contain all code to
%%%      create and start an application manually. This convention
%%%      comes from OTP framework. Normally, the simple application is
%%%      to start only the main application supervisor and return the
%%%      name of the registered application (if it exist).  
%%% @end
%%%
%%% More information available here:
%%%   http://erlang.org/doc/design_principles/applications.html
%%%-------------------------------------------------------------------
-module(spaceinvader_app).
-behaviour(application).

% exported functions come directly from OTP application specification.
% http://erlang.org/doc/apps/kernel/application.html
-export([start/2]).
-export([stop/1]).

%%--------------------------------------------------------------------
%% @doc start the application. This function is a callback from
%%      application OTP behavior:
%% @end
%%
%% More information here:
%%   http://erlang.org/doc/apps/kernel/application.html#Module:start-2
%%--------------------------------------------------------------------
-spec start(_StartType :: normal | {takeover, node()} | {failover, node()} 
           ,_StartArgs :: term()) 
           -> {ok, Pid :: pid()} |
              {ok, Pid :: pid(), State :: term()} |
              {error, Reason :: term()}.
start(_StartType, _StartArgs) ->
    % spaceinvader_sup is our application supervisor and it
    % will manage all other application process, in the default
    % OTP model.
    spaceinvader_sup:start_link().

%%--------------------------------------------------------------------
%% @doc stop our running application. This function is a callback from
%%      OTP application behavior:
%% @end
%%
%% More information here:
%%   http://erlang.org/doc/apps/kernel/application.html#Module:stop-1
%%--------------------------------------------------------------------
-spec stop(State :: term()) 
          -> ok.
stop(_State) ->
    ok.

