-module(hash_map).
-export([lookup/2]).
-export([test_lookup/0]).

lookup(_, []) -> none;
lookup(Seeking, [{Key, Value}|Tail]) ->
    if
      Seeking == Key -> Value;
      true -> lookup(Seeking, Tail)
    end.

test_lookup() -> lookup(test,[{thing, "Hello"},{test,"World"},{other,"Story"}]).
