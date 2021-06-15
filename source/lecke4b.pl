tartalmaz(X, [X|_]).
tartalmaz(X, [_|M]) :- tartalmaz(X, M).

nemtartalmaz(_, []).
nemtartalmaz(X, [Y|M]) :- X \= Y, nemtartalmaz(X, M).

hossz([], 0).
hossz([_|M], N) :- hossz(M, N1), N is 1 + N1.

között(N, M, N) :- N =< M.
között(N, M, X) :-
    N < M, N1 is N + 1,
    között(N1, M, X).

% Standard
méret(3).
kígyó([3,2,2,3,2,3,2,2,3,3,2,2,2,3,3,3,3]).

% Mean green
% méret(3).
% kígyó([3,3,2,3,2,3,2,2,2,3,3,3,2,3,3,3]).

% King
% méret(4).
% kígyó([3,2,3,2,2,4,2,3,2,3,2,3,2,2,2,2,
%        2,2,2,2,3,3,2,2,2,2,2,3,4,2,2,2,
%        4,2,3,2,2,2,2,2,2,2,2,2,4,2]).

% Generáló
% méret(3).
% kígyó(K) :-
%     méret(N), N3 is N^3, N1 is N3 // (N - 1),
%     között(N1, N3, H), hossz(K, H),
%     kígyó(K, N3).
% kígyó([], 1).
% kígyó([Sz|K], M) :-
%     M > 1, méret(N), között(2, N, Sz),
%     M1 is M - Sz + 1, kígyó(K, M1).

pozíció(p(X, Y, Z)) :-
    méret(N), között(1, N, X),
    között(1, N, Y), között(1, N, Z).

irány(i(T,I)) :-
    tartalmaz(T, [x,y,z]),
    tartalmaz(I, [-1,1]).

köv_irány(i(T,_), i(T1,I)) :-
    irány(i(T1,I)), T \= T1.

lépés(p(X,Y,Z), i(x,I), p(X1,Y,Z)) :- X1 is X + I.
lépés(p(X,Y,Z), i(y,I), p(X,Y1,Z)) :- Y1 is Y + I.
lépés(p(X,Y,Z), i(z,I), p(X,Y,Z1)) :- Z1 is Z + I.

ellenőriz(PL, _, 1, PL).
ellenőriz([P|M], I, N, PL1) :-
    N > 1, lépés(P, I, P1),
    pozíció(P1), nemtartalmaz(P1, M), N1 is N - 1,
    ellenőriz([P1, P|M], I, N1, PL1).

megoldás([N], PL, I, [I]) :- ellenőriz(PL, I, N, _).
megoldás([N|M], PL, I, [I|X]) :-
    ellenőriz(PL, I, N, PL1), köv_irány(I, I1), 
    megoldás(M, PL1, I1, X).

fordít([], []).
fordít([i(T, I)|M], [F|FM]) :-
    (  T = x, (I = 1, F = jobbra; I = -1, F = balra)
     ; T = y, (I = 1, F = fel;    I = -1, F = le)
     ; T = z, (I = 1, F = előre;  I = -1, F = hátra)
    ), 
    fordít(M, FM).

kígyó_kocka(K, P, FI, FX) :-
    kígyó(K), pozíció(P), irány(I),
    megoldás(K, [P], I, X), fordít([I|X], [FI|FX]).
