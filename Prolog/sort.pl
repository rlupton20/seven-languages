partition([],P,[],[]).
partition([Head|Tail], P, [Head|Y], Z) :- Head =< P, partition(Tail, P, Y, Z).
partition([Head|Tail], P, Y, [Head|Z]) :- Head > P, partition(Tail, P, Y, Z).

quicksort([],[]).
quicksort([Head|Tail], Y) :- partition(Tail,Head,LP,RP),
			     quicksort(LP, L), quicksort(RP,R),
			     append(L, [Head], LL), append(LL,R,Y).
