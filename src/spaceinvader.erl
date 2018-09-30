%%%-------------------------------------------------------------------
%%% @doc main module for make this application as script.
%%%
%%%      More information:
%%%      <ul><li>[http://erlang.org/doc/man/escript.html]</li>
%%%          <li>[https://www.rebar3.org/docs/commands#section-escriptize]</li>
%%%      </ul>
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
% set ip of the current node
main(["-ip", IP|Rest]) ->
    set_node(IP),
    main(Rest);

% try to connect to another node
main(["-node", Node|Rest]) ->
    N = erlang:list_to_atom(Node),
    net_kernel:connect_node(N),
    main(Rest);

% start our spaceinvader application.
main(_Args) ->
    io:format("start spaceinvader application~n"),
    io:format("  nodename: ~p~n", [node()]),
    io:format("  spaceinvader localhost tcp/7777~n"),
    application:start(spaceinvader),
    loop().

%%--------------------------------------------------------------------
%% @doc set_node/1 set our node based on IP address and a random
%%      generated name.
%% @end
%%--------------------------------------------------------------------
-spec set_node(IP :: list()) -> ok.
set_node(IP) ->
    {ok, Name} = generate_name(),
    NodeName = erlang:list_to_atom(Name ++ "@" ++ IP),
    net_kernel:stop(),
    net_kernel:start([NodeName, longnames]).

%%--------------------------------------------------------------------
%% @doc generate_name/0 generate a random name for ou running node.
%% @end
%%--------------------------------------------------------------------
-spec generate_name() -> {ok, iodata()}.
generate_name() ->
    generate_name(<<>>).

-spec generate_name(iodata()) -> {ok, iodata()}.
generate_name(Bitstring) 
  when byte_size(Bitstring) > 5 ->
    {ok, erlang:binary_to_list(Bitstring)};
generate_name(Bitstring) 
  when byte_size(Bitstring) =< 5 ->
    <<Char>> = crypto:strong_rand_bytes(1),
    generate_name(Bitstring, Char).

-spec generate_name(Bitstring :: iodata(), Char :: integer()) -> {ok, iodata()}.
generate_name(Bitstring, Char) 
  when Char >= 65 andalso Char =< 80 ->
    generate_name(<<Char/integer, Bitstring/bitstring>>);
generate_name(Bitstring, _) ->
    generate_name(Bitstring).      

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

