# distributed-space-invader

Distributed Space Invader is a simple game state forwarder based only
with built-in Erlang datastructure. Client sent data with BERT, this
server will forward them to all other notes.

## Build

    $ rebar3 compile

## Play with a shell

If you want to see what's going on in the VM during playing:

    $ rebar3 shell -name ${name}@${ip}
    > application:start(spaceinvader).
    > net_kernel:connect_node('othernode@otherip').

## Start it manually

If you want to create a full executable file, you can create an
escript file with escriptize command

    $ rebar3 escriptize
    $ ./_build/bin/spaceinvader

## Generate documentation

A bunch of documentation was created, you can access them in directory
`doc` after compiling it with this command:

    $ rebar3 edoc

## Haskell Space Invader Client

Our own client is actually present here:

    $ stack build
    $ stack exec game "username"

 * https://github.com/lambda-rennes/space-invaders

# Project history and steps

This project is to show that Erlang language is not only a language:
when you install it, Erlang give you lot of integrated tools to do lot
of things without installing external modules or libraries. This
project will introduce

 * Concise introduction to Erlang syntax
 * Live testing with Erlang REPL
 * Simple rebar3 use case
 * Small introduction to Distributed Erlang
 * Small introduction to OTP

```
         [bert datastructure]                      [accepted connection]
  ______/                __________     __________/    _______
 |      |               |          |   |          |   |       |
 | game |--[tcp/7777]-->| listener |-->| acceptor |-->| relay |--//
 |______|               |__________|   |__________|   |_______|
                        /                             /
    [tcp socket handler]                             /
                              [message relay process]



             [distributed erlang]
     _______/    _______     __________                  ______
    (       )   |       |   |          |                |      |
 --( cluster )->| relay |-->| acceptor |---[tcp/7777]-->| game |
    (_______)   |_______|   |__________|                |______|
        |                              \
        .                               [send ETF flow]
     ___.___
    :       :
    : relay :
    :_______:
    
```

Here the detailed steps:

1. Ensure your have all required Erlang tools on your system

   ```
   # On FreeBSD
   pkg install erlang rebar3
   ```
   
   If, for some reasons, those requirements are not available on your
   distribution, source code and binary version of Erlang can be
   downloaded directly from the official website:
   http://www.erlang.org/downloads.

2. Create a repository on github (or locally with github or fossil or
   whatever you want)
   
   ```
   git init spaceinvaders
   ```
   
3. We can now use rebar3, this tool will initialize our project with
   some default files and templates, make things easier for everyone.
   
   ```
   rebar3 new app name=spaceinvaders
   cd spaceinvaders
   git add LICENSE README.md
   git add rebar.config
   git add src/*.erl
   git add src/*.src
   git commit -m "our first commit"
   ```
   
   * `LICENSE` and `README.md` don't need any explaination.
   
   * `rebar.config` is our configuration for our project.
   
   * `src/spaceinvader_app.erl` will contain our Erlang application
     code. (http://erlang.org/doc/apps/kernel/application.html)
   
   * `src/spaceinvader.app.src` is our resource file, this is a kind
     of checker/starter for our app. (http://erlang.org/doc/man/app.html)
	 
   * `src/spaceinvader_sup.erl` is our main supervisor for our
     application, this one will monitor our application and restart it
     automatically if this one crash. (http://erlang.org/doc/man/supervisor.html)
	 
   * `src/*.erl` all other files, can be libraries, other application...

4. So, actually, we want an application to manage tcp socket and
   forward all messages to other client, based on Distributed
   Erlang. To make this thing possible, we'll, first, create our
   listener and acceptor. (I don't want to reuse code from other
   project, but, if you are interested, you can take a look at ranch,
   this application manage a pool of tcp acceptor, mainly used in
   cowboy http server). So, we'll create a file named
   `src/spaceinvader_listener.erl`. This process
   
   * listen a defined tcp port localy (7777 in our case)
   * spawn a new acceptor when a connection is accepted
   * keep state of our connection
   
   ```
   ```
   
   And `src/spaceinvader_acceptor.erl` for our acceptor side.
   
   * accept messages from the accepted socket
   * route messages
   
   ```
   
   ```
   
   We will also use Distributed Erlang feature, we should enable it in
   `rebar.config`. (https://www.rebar3.org/docs/configuration#section-distribution)
   
   ```
   { dist_node
   , [{setcookie, 'mycookie'}
     ,{sname, 'nodename@localhost'}
     ]
   }.
   ```

   NOTE: each player must choose a different name and hostname.

   Its a good moment to try and see if our code is compiling and
   working as expected, first, we build our project with rebar3 and
   start an Erlang REPL. (https://www.rebar3.org/docs/commands#section-shell)
   
   ```
   rebar3 compile
   rebar3 shell
   ```
   
   In the new opened shell, we can start manually our listener
   
   ```
   spaceinvader_listener:start().
   ```
   
   In another shell console session, we can send some raw tcp data
   with netcat.
   
   ```
   echo "test" | nc localhost 7777
   ```

5. Now is the "complicated" part, for your information, this part is
   not really safe for different reason, and should be used for
   testing purpose only. Distributed Erlang feature offer you a way to
   connect Erlang nodes between them but, without a lot of
   security. We'll see that later. For the moment, we'll create a new
   file named `src/spaceinvader_relay.erl`. This file will give us a
   common interface to talk to other connected nodes.
   
   ```
   ```
   
   We can rebuild our code directly from rebar3 shell and start our
   relay.
   
   ```
   r3:do(compile).
   spaceinvader_relay:start().
   ```

   This process should be registered and will receive all messages
   from all other nodes...
   
   ```
   Msg = "This is a test".
   {'myname', 'myhostname'} ! Msg.
   ```

   ... and relay it to our game.


----

   * http://erlang.org/doc/reference_manual/distributed.html
   * http://erlang.org/doc/man/net_kernel.html
