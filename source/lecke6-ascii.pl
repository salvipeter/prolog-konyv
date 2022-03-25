hozzafuz_kl(X-Y, Y-Z, X-Z).

rendez2(X, Y) :- rendez2_kl(X, Y-[]).

rendez2_kl([], X-X).
rendez2_kl([X|M], Y-Z) :-
    szetoszt(X, M, Kicsi, Nagy),
    rendez2_kl(Kicsi, Y-[X|Y1]),
    rendez2_kl(Nagy, Y1-Z).

szetoszt(_, [], [], []).
szetoszt(X, [Y|M], K, [Y|N]) :-
    X =< Y, szetoszt(X, M, K, N).
szetoszt(X, [Y|M], [Y|K], N) :-
    X > Y, szetoszt(X, M, K, N).

forditott1([], []).
forditott1([X|M], Y) :-
    forditott1(M, M1), hozzafuz(M1, [X], Y).

forditott2(X, Y) :- forditott2(X, [], Y).

forditott2([], Y, Y).
forditott2([X|M], F, Y) :- forditott2(M, [X|F], Y). 

forditott3(X, Y) :- forditott_kl(X, Y-[]).

forditott_kl([], X-X).
forditott_kl([X|M], Y-Z) :- forditott_kl(M, Y-[X|Z]).

lecserel(X, Y, X, Y) :- !.
lecserel(_, _, Z, Z) :- atomic(Z), !.
lecserel(X, Y, Z, Z1) :-
    Z =.. [F|Arg],
    mindent_lecserel(X, Y, Arg, Arg1),
    Z1 =.. [F|Arg1].

mindent_lecserel(_, _, [], []).
mindent_lecserel(X, Y, [Z|M], [Z1|M1]) :-
    lecserel(X, Y, Z, Z1),
    mindent_lecserel(X, Y, M, M1).

kiertekel(K, L, X) :-
    behelyettesit(K, L, K1), X is K1.

behelyettesit(K, [], K).
behelyettesit(K, [A=N|M], K2) :-
    lecserel(A, N, K, K1),
    behelyettesit(K1, M, K2).

eletkor(lica, 11).
eletkor(mimi, 10).
eletkor(dusa, 5).
eletkor(zsofi, 5).
eletkor(juli, 2).

polip([3,5,2,4,5,5,3]).

nyertes([]) :- !. 
nyertes(L) :- vesz(L, _, L1), vesztes(L1), !.

:- dynamic(vesztes/1). 
vesztes(L) :-
    \+ nyertes(L), !,
    asserta((vesztes(L) :- !)). 
vesztes(L) :-
    asserta((vesztes(L) :- !, fail)),
    fail. 

vesz(L, V, L1) :- vesz(L, 1, V, L1).
vesz([X|M], I, I-X, M).
vesz([X|M], I, I-Y, [Y1|M]) :- X1 is X - 1, kozott(1, X1, Y), Y1 is X1 - Y.
vesz([X|M], I, V, [X|M1]) :- I1 is I + 1, vesz(M, I1, V, M1).

nyero_lepes(L, V) :- vesz(L, V, X), vesztes(X), !.

kozott(N, M, N) :- N =< M.
kozott(N, M, X) :-
    N < M, N1 is N + 1,
    kozott(N1, M, X).

negyzetes :-
    repeat, read(X),
    ( X = stop, !
    ; Y is X * X, write(Y), fail
    ).
