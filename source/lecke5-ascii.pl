rendez1([], []).
rendez1([X|M], Y) :-
    rendez1(M, M1), beszur(X, M1, Y).

beszur(X, [], [X]).
beszur(X, [Y|M], [X,Y|M]) :- X =< Y.
beszur(X, [Y|M], [Y|M1]) :- X > Y, beszur(X, M, M1).

rendez2([], []).
rendez2([X|M], Y) :-
    szetoszt(X, M, Kicsi, Nagy),
    rendez2(Kicsi, K),
    rendez2(Nagy, N),
    hozzafuz(K, [X|N], Y).

szetoszt(_, [], [], []).
szetoszt(X, [Y|M], K, [Y|N]) :-
    X =< Y, szetoszt(X, M, K, N).
szetoszt(X, [Y|M], [Y|K], N) :-
    X > Y, szetoszt(X, M, K, N).

rendez3([], []).
rendez3([X], [X]) :- !.
rendez3(X, Y) :-
    X = [_,_|_], Y = [_,_|_],
    ketteoszt(X, X1, X2),
    rendez3(X1, Y1),
    rendez3(X2, Y2),
    osszefesul(Y1, Y2, Y).

ketteoszt([], [], []).
ketteoszt([X], [X], []).
ketteoszt([X,Y|M], [X|M1], [Y|M2]) :-
    ketteoszt(M, M1, M2).

osszefesul([X|Mx], [Y|My], [X|M]) :-
    X =< Y, !, osszefesul(Mx, [Y|My], M).
osszefesul([X|Mx], [Y|My], [Y|M]) :-
    X > Y, !, osszefesul([X|Mx], My, M).
osszefesul(X, [], X) :- !.
osszefesul([], Y, Y) :- !.

max1(X, Y, X) :- X >= Y.
max1(X, Y, Y) :- X < Y.

max2(X, Y, X) :- X >= Y, !.
max2(X, Y, Y) :- X < Y.

max3(X, Y, X) :- X >= Y, !.
max3(_, Y, Y).

hozzaad(X, L, L) :- tartalmaz(X, L), !.
hozzaad(X, L, [X|L]).

szereti(csilla, X) :- pok(X), !, fail.
szereti(csilla, X) :- allat(X).

allat(tarantula).
allat(denever).
pok(tarantula).

nem(P) :- P, !, fail.
nem(_).

tartalmaz(X, [X|_]).
tartalmaz(X, [_|Maradek]) :- tartalmaz(X, Maradek).

hozzafuz([], L2, L2).
hozzafuz([X|M1], L2, [X|M3]) :- hozzafuz(M1, L2, M3).
