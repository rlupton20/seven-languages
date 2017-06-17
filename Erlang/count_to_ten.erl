-module(count_to_ten).
-export([count_to_ten/0]).

count_to_ten() -> help_count_to_ten(0).

help_count_to_ten(10) -> io:fwrite("~B~n", [10]);
help_count_to_ten(K) -> io:fwrite("~B~n", [K]), help_count_to_ten(K + 1).