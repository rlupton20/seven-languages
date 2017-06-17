-module(tic_tac_toe).
-export([winner/1]).

% Represent boards by 9-tuples

% Do any moves remain?
moves_remain(Tup) -> lists:any(fun(P) -> (P /= x) and (P /= o) end, tuple_to_list(Tup)).

% Extract a line according to tuple positions
get_line({A,B,C}, Tup) -> {element(A, Tup), element(B, Tup), element(C, Tup)}.

winner(B) -> Lines = [ {1,4,7}, {2,5,8}, {3,6,9},
     	               {1,2,3}, {4,5,6}, {7,8,9},
		       {1,5,9}, {3,5,7} ],
	Positions = [get_line(P,B) || P <- Lines],
	case [A || {A,A,A} <- Positions, (A==x) or (A==o)] of
	     [A|_] -> A;
	     _ -> (case moves_remain(B) of
	            true -> no_winner;
		    false -> cat
		  end)
	end.

	