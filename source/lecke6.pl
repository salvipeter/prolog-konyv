hozzáfűz_kl(X-Y, Y-Z, X-Z).

rendez2(X, Y) :- rendez2_kl(X, Y-[]).

rendez2_kl([], X-X).
rendez2_kl([X|M], Y-Z) :-
    szétoszt(X, M, Kicsi, Nagy),
    rendez2_kl(Kicsi, Y-[X|Y1]),
    rendez2_kl(Nagy, Y1-Z).

szétoszt(_, [], [], []).
szétoszt(X, [Y|M], K, [Y|N]) :-
    X =< Y, szétoszt(X, M, K, N).
szétoszt(X, [Y|M], [Y|K], N) :-
    X > Y, szétoszt(X, M, K, N).

fordított1([], []).
fordított1([X|M], Y) :-
    fordított1(M, M1), hozzáfűz(M1, [X], Y).

fordított2(X, Y) :- fordított2(X, [], Y).

fordított2([], Y, Y).
fordított2([X|M], F, Y) :- fordított2(M, [X|F], Y). 

fordított3(X, Y) :- fordított_kl(X, Y-[]).

fordított_kl([], X-X).
fordított_kl([X|M], Y-Z) :- fordított_kl(M, Y-[X|Z]).

lecserél(X, Y, X, Y) :- !.
lecserél(_, _, Z, Z) :- atomic(Z), !.
lecserél(X, Y, Z, Z1) :-
    Z =.. [F|Arg],
    mindent_lecserél(X, Y, Arg, Arg1),
    Z1 =.. [F|Arg1].

mindent_lecserél(_, _, [], []).
mindent_lecserél(X, Y, [Z|M], [Z1|M1]) :-
    lecserél(X, Y, Z, Z1),
    mindent_lecserél(X, Y, M, M1).

kiértékel(K, L, X) :-
    behelyettesít(K, L, K1), X is K1.

behelyettesít(K, [], K).
behelyettesít(K, [A=N|M], K2) :-
    lecserél(A, N, K, K1),
    behelyettesít(K1, M, K2).

életkor(lica, 11).
életkor(mimi, 10).
életkor(dusa, 5).
életkor(zsófi, 5).
életkor(juli, 2).

polip([3,5,2,4,5,5,3]).

nyertes([]) :- !. 
nyertes(L) :- nyom(L, _, L1), vesztes(L1), !.

:- dynamic(vesztes/1). 
vesztes(L) :-
    \+ nyertes(L), !,
    asserta((vesztes(L) :- !)). 
vesztes(L) :-
    asserta((vesztes(L) :- !, fail)),
    fail. 

nyom(L, V, L1) :- nyom(L, 1, V, L1).
nyom([X|M], I, I-X, M).
nyom([X|M], I, I-Y, [Y1|M]) :- X1 is X - 1, között(1, X1, Y), Y1 is X1 - Y.
nyom([X|M], I, V, [X|M1]) :- I1 is I + 1, nyom(M, I1, V, M1).

nyerő_lépés(L, V) :- nyom(L, V, X), vesztes(X), !.

között(N, M, N) :- N =< M.
között(N, M, X) :-
    N < M, N1 is N + 1,
    között(N1, M, X).

négyzetes :-
    repeat, read(X),
    ( X = stop, !
    ; Y is X * X, write(Y), fail
    ).
