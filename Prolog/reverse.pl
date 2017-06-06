new_append([],X,[X]).
new_append([Head|Tail], X, [Head|Z]) :- new_append(Tail, X, Z). 

new_reverse([], []).
new_reverse([Head|Tail], Y) :- new_append(Z, Head, Y), new_reverse(Tail, Z).
