rendez1([], []).
rendez1([X|M], Y) :-
    rendez1(M, M1), beszúr(X, M1, Y).

beszúr(X, [], [X]).
beszúr(X, [Y|M], [X,Y|M]) :- X =< Y.
beszúr(X, [Y|M], [Y|M1]) :- X > Y, beszúr(X, M, M1).

rendez2([], []).
rendez2([X|M], Y) :-
    szétoszt(X, M, Kicsi, Nagy),
    rendez2(Kicsi, K),
    rendez2(Nagy, N),
    hozzáfűz(K, [X|N], Y).

szétoszt(_, [], [], []).
szétoszt(X, [Y|M], K, [Y|N]) :-
    X =< Y, szétoszt(X, M, K, N).
szétoszt(X, [Y|M], [Y|K], N) :-
    X > Y, szétoszt(X, M, K, N).

rendez3([], []).
rendez3([X], [X]) :- !.
rendez3(X, Y) :-
    X = [_,_|_], Y = [_,_|_],
    kettéoszt(X, X1, X2),
    rendez3(X1, Y1),
    rendez3(X2, Y2),
    összefésül(Y1, Y2, Y).

kettéoszt([], [], []).
kettéoszt([X], [X], []).
kettéoszt([X,Y|M], [X|M1], [Y|M2]) :-
    kettéoszt(M, M1, M2).

összefésül([X|Mx], [Y|My], [X|M]) :-
    X =< Y, !, összefésül(Mx, [Y|My], M).
összefésül([X|Mx], [Y|My], [Y|M]) :-
    X > Y, !, összefésül([X|Mx], My, M).
összefésül(X, [], X) :- !.
összefésül([], Y, Y) :- !.

max1(X, Y, X) :- X >= Y.
max1(X, Y, Y) :- X < Y.

max2(X, Y, X) :- X >= Y, !.
max2(X, Y, Y) :- X < Y.

max3(X, Y, X) :- X >= Y, !.
max3(_, Y, Y).

hozzáad(X, L, L) :- tartalmaz(X, L), !.
hozzáad(X, L, [X|L]).

szereti(csilla, X) :- pók(X), !, fail.
szereti(csilla, X) :- állat(X).

állat(tarantula).
állat(denevér).
pók(tarantula).

nem(P) :- P, !, fail.
nem(_).

tartalmaz(X, [X|_]).
tartalmaz(X, [_|Maradék]) :- tartalmaz(X, Maradék).

hozzáfűz([], L2, L2).
hozzáfűz([X|M1], L2, [X|M3]) :- hozzáfűz(M1, L2, M3).
