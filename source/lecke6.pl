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

kiértékel(K, L, X) :- behelyettesít(K, L, K1), X is K1.

behelyettesít(K, [], K).
behelyettesít(K, [A=N|M], K2) :-
    lecserél(A, N, K, K1),
    behelyettesít(K1, M, K2).

életkor(lica, 11).
életkor(mimi, 10).
életkor(dusa, 5).
életkor(zsófi, 5).
életkor(juli, 2).

négyzetes :-
    repeat, read(X),
    ( X = stop, !
    ; Y is X * X, write(Y), fail
    ).
