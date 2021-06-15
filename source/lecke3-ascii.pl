tartalmaz(X, [X|_]).
tartalmaz(X, [_|Maradek]) :- tartalmaz(X, Maradek).

hozzafuz([], L2, L2).
hozzafuz([X|M1], L2, [X|M3]) :- hozzafuz(M1, L2, M3).

hozzaad(X, L, [X|L]).

torol(X, [X|M], M).
torol(X, [Y|M], [Y|M1]) :- torol(X, M, M1).

betesz(X, L, L1) :- torol(X, L1, L).

reszlista(R, L) :-
    hozzafuz(_, L1, L), hozzafuz(R, _, L1).

permutacio([], []).
permutacio([X|M], P) :-
    permutacio(M, L), betesz(X, L, P).
