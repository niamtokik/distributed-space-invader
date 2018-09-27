%%%-------------------------------------------------------------------
%%% @doc main module for make this application as script.
%%% @end
%%%-------------------------------------------------------------------
-module(spaceinvader).
-export([main/1]).

%%--------------------------------------------------------------------
%% @doc this function is here to get argument from command line,
%%      parse them and start our application properly.
%% @end
%%--------------------------------------------------------------------
-spec main(list()) -> none().
main(_Args) ->
    io:format("start spaceinvader application...~n"),
    application:start(spaceinvader),
    loop().

%%--------------------------------------------------------------------
%% @doc just a hack function to stay in invite process, used for
%%      long living process with escript. If you don't want to do
%%      this kind of thing, you should start erlang vm in another
%%      way...
%% @end
%%--------------------------------------------------------------------
-spec loop() -> none().
loop() ->
    receive _ -> loop()
    end.
