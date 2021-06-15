tartalmaz(X, [X|_]).
tartalmaz(X, [_|Maradék]) :- tartalmaz(X, Maradék).

hozzáfűz([], L2, L2).
hozzáfűz([X|M1], L2, [X|M3]) :- hozzáfűz(M1, L2, M3).

hozzáad(X, L, [X|L]).

töröl(X, [X|M], M).
töröl(X, [Y|M], [Y|M1]) :- töröl(X, M, M1).

betesz(X, L, L1) :- töröl(X, L1, L).

részlista(R, L) :-
    hozzáfűz(_, L1, L), hozzáfűz(R, _, L1).

permutáció([], []).
permutáció([X|M], P) :-
    permutáció(M, L), betesz(X, L, P).
