-module(doctor).
-export([loop/0, loop/2]).

loop() ->
    process_flag(trap_exit, true),
    receive
	new ->
	    io:format("Creating new monitored process\n"),
	    register(translator, spawn_link(fun translate:loop/0)),
	    loop();
	{'EXIT', From, Reason} ->
	    io:format("Process ~p died: ~p", [From, Reason]),
	    self() ! new,
	    loop()
	end.

loop(F, A) ->
    process_flag(trap_exit, true),
    receive
	new ->
	    io:format("Creating new monitored process\n"),
	    register(A, spawn_link(F)),
	    loop(F, A);
	{'EXIT', From, Reason} ->
	    io:format("Process ~p died: ~p", [From, Reason]),
	    self() ! new,
	    loop(F, A)
	end.
