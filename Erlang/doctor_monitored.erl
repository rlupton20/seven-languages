-module(doctor_monitored).
-export([loop/0, monitored/0]).


% loop provides the doctor for a revolver process
loop() ->
    process_flag(trap_exit, true),
    receive
        new ->
	    case whereis(revolver) of
	        undefined ->
		    io:format("Creating and monitoring revolver process.~n"),
	    	    register(revolver, spawn_link(fun roulette:loop/0)),
	    	    loop();
		P ->
		    link(P),
		    loop()
	    end;

	kill ->
	    io:format("doctor killed.~n"),
	    exit({doctor,killed,at,erlang:time()});

	{'EXIT', From, Reason} ->
	    io:format("The shooter ~p died with reason ~p.", [From, Reason]),
	    io:format(" Restarting.~n"),
	    self() ! new,
	    loop()
       end.


% monitored lets us create a monitored doctor
monitored() -> monitored_helper(none,none).


% We want to monitor both the doctor and a monitor for the monitor
% We do this by keeping the doctor and monitors PID in the function
% parameters, and calling (updated if neccesary) recursively. We could
% also just use the registered names.
monitored_helper(Doctor, SuperMonitor) ->
    process_flag(trap_exit, true),
    receive
	new ->
	    self() ! create,
	    monitored_helper(Doctor, SuperMonitor);

	create ->
	    self() ! create_monitor,
	    self() ! create_doctor,
	    monitored_helper(Doctor, SuperMonitor);

	create_doctor ->
	    case whereis(doctor) of
		undefined ->
		    io:format("Creating monitored doctor~n"),
		    register(doctor, spawn_link(fun loop/0)),
		    doctor ! new,
		    monitored_helper(whereis(doctor), SuperMonitor);
		P ->
		    link(P), % super_monitor won't link monitor to doctor
		    monitored_helper(P, SuperMonitor)
	    end;

	    create_monitor ->
		case whereis(super_monitor) of
		    undefined ->
			io:format("Creating super monitor~n"),
			register(super_monitor, spawn_link(fun() -> super_monitor(fun monitored/0) end)),
			super_monitor ! { monitor, self()},
			monitored_helper(Doctor, whereis(super_monitor));
		    P ->
			monitored_helper(Doctor, P)
		end;

	    kill ->
		io:format("monitor killed.~n"),
		exit({monitor,killed,at,erlang:time()});

	    {'EXIT', Doctor, Reason} ->
		io:format("doctor died with reason ~p. Restarting.~n", [Reason]),
		self() ! create_doctor,
		monitored_helper(Doctor, SuperMonitor);

	    {'EXIT', SuperMonitor, Reason} ->
		io:format("super monitor died with reason ~p. Restarting. ~n", [Reason]),
		self() ! create_monitor,
		monitored_helper(Doctor,SuperMonitor)
	    end.


% super_monitor(F) is a monitor for a process spawned and running F
super_monitor(F) ->
    process_flag(trap_exit, true),
    receive
        kill ->
            io:format("super monitor killed. ~n"),
	    exit({super_monitor,killed,at,erlang:time()});

        { monitor, P } ->
	    link(P),
	    super_monitor(F);

	{'EXIT', Pid, Reason} ->
    	    io:format("monitor died with reason ~p. Restarting.~n", [Reason]),
	    register(monitor, spawn_link(F)),
	    monitor ! new,
	    super_monitor(F)
    end.
