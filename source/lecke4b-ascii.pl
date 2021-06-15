tartalmaz(X, [X|_]).
tartalmaz(X, [_|M]) :- tartalmaz(X, M).

nemtartalmaz(_, []).
nemtartalmaz(X, [Y|M]) :- X \= Y, nemtartalmaz(X, M).

hossz([], 0).
hossz([_|M], N) :- hossz(M, N1), N is 1 + N1.

kozott(N, M, N) :- N =< M.
kozott(N, M, X) :-
    N < M, N1 is N + 1,
    kozott(N1, M, X).

% Standard
meret(3).
kigyo([3,2,2,3,2,3,2,2,3,3,2,2,2,3,3,3,3]).

% Mean green
% meret(3).
% kigyo([3,3,2,3,2,3,2,2,2,3,3,3,2,3,3,3]).

% King
% meret(4).
% kigyo([3,2,3,2,2,4,2,3,2,3,2,3,2,2,2,2,
%        2,2,2,2,3,3,2,2,2,2,2,3,4,2,2,2,
%        4,2,3,2,2,2,2,2,2,2,2,2,4,2]).

% Generalo
% meret(3).
% kigyo(K) :-
%     meret(N), N3 is N^3, N1 is N3 // (N - 1),
%     kozott(N1, N3, H), hossz(K, H),
%     kigyo(K, N3).
% kigyo([], 1).
% kigyo([Sz|K], M) :-
%     M > 1, meret(N), kozott(2, N, Sz),
%     M1 is M - Sz + 1, kigyo(K, M1).

pozicio(p(X, Y, Z)) :-
    meret(N), kozott(1, N, X),
    kozott(1, N, Y), kozott(1, N, Z).

irany(i(T,I)) :-
    tartalmaz(T, [x,y,z]),
    tartalmaz(I, [-1,1]).

kov_irany(i(T,_), i(T1,I)) :-
    irany(i(T1,I)), T \= T1.

lepes(p(X,Y,Z), i(x,I), p(X1,Y,Z)) :- X1 is X + I.
lepes(p(X,Y,Z), i(y,I), p(X,Y1,Z)) :- Y1 is Y + I.
lepes(p(X,Y,Z), i(z,I), p(X,Y,Z1)) :- Z1 is Z + I.

ellenoriz(PL, _, 1, PL).
ellenoriz([P|M], I, N, PL1) :-
    N > 1, lepes(P, I, P1),
    pozicio(P1), nemtartalmaz(P1, M), N1 is N - 1,
    ellenoriz([P1, P|M], I, N1, PL1).

megoldas([N], PL, I, [I]) :- ellenoriz(PL, I, N, _).
megoldas([N|M], PL, I, [I|X]) :-
    ellenoriz(PL, I, N, PL1), kov_irany(I, I1), 
    megoldas(M, PL1, I1, X).

fordit([], []).
fordit([i(T, I)|M], [F|FM]) :-
    (  T = x, (I = 1, F = jobbra; I = -1, F = balra)
     ; T = y, (I = 1, F = fel;    I = -1, F = le)
     ; T = z, (I = 1, F = elore;  I = -1, F = hatra)
    ), 
    fordit(M, FM).

kigyo_kocka(K, P, FI, FX) :-
    kigyo(K), pozicio(P), irany(I),
    megoldas(K, [P], I, X), fordit([I|X], [FI|FX]).
