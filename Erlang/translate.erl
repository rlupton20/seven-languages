-module(translate).
-export([loop/0, translate/2]).

%%% loop() loops over messages which can be translated
loop() ->
       receive
        kill -> exit({translate, "died at", erlang:time()});

	{From, "casa"} ->
	       From ! "house",
	       loop();

	{From, "blanca"} ->
	       From ! "white",
	       loop();

	{From, _} ->
	       From ! {error, "Could not translate"},
	       loop()
end.

translate(To, Word) ->
	      To ! {self(), Word},
	      receive
		{error, Msg} -> io:format("error: ~p~n", [Msg]), Word;
		Translation -> Translation
	      end.
	  