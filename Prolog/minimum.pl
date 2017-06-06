minimum([X], X).
minimum([Head|Tail], Head) :- minimum(Tail,Y), Head < Y.
minimum([Head|Tail], X) :- minimum(Tail, X), Head >= X.
