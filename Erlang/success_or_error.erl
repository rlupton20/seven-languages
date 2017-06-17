-module(success_or_error).
-export([pretty_print/1]).

pretty_print(success) -> io:fwrite("success~n");
pretty_print({error, Message}) -> io:fwrite(string:concat("error: ", Message)).