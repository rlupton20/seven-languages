-module(count_words).
-export([count_words/1]).


count_words(S) -> helper(string:tokens(S, " \n")).

helper([]) -> 0;
helper([_|Tail]) -> 1 + helper(Tail).
