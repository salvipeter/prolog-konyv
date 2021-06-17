% Magas szintű keretprogram

kalah :-
    alapbeállítás(Állás, Játékos),
    kirajzol(Állás, Játékos),
    játék(Állás, Játékos).

játék(Állás, Játékos) :-
    játék_vége(Állás, Játékos, Eredmény), !,
    bejelent(Eredmény).
játék(Állás, Játékos) :-
    lépést_választ(Állás, Játékos, Lépés),
    lép(Lépés, Állás, Állás1),
    következő_játékos(Játékos, Játékos1), !,
    kirajzol(Állás1, Játékos1),
    játék(Állás1, Játékos1).

következő_játékos(ember, számítógép).
következő_játékos(számítógép, ember).

bejelent(ember) :- kiír('Nyertél, gratulálok!').
bejelent(számítógép) :- kiír('Nyertem.').
bejelent(döntetlen) :- kiír('Döntetlen lett!').

% Reprezentáció

alapbeállítás(Állás, ember) :-
    kövek_száma(K),
    Állás = tábla([K,K,K,K,K,K],0,
                  [K,K,K,K,K,K],0).

% Szabályok

játék_vége(tábla(_,N,_,N), _, döntetlen) :-
    kövek_száma(K), N =:= 6 * K, !.
játék_vége(tábla(_,N1,_,_), Játékos, Játékos) :-
    kövek_száma(K), N1 > 6 * K, !.
játék_vége(tábla(_,_,_,N2), Játékos, Másik) :-
    kövek_száma(K), N2 > 6 * K,
    következő_játékos(Játékos, Másik).

lép([], Állás, Állás1) :- megfordít(Állás, Állás1).
lép([L|M], Állás, Állás2) :-
    kövek(L, Állás, K),
    vet(K, L, Állás, Állás1),
    lép(M, Állás1, Állás2).

megfordít(tábla(La,Na,Lb,Nb), tábla(Lb,Nb,La,Na)).

kövek(I, tábla(L,_,_,_), K) :- n_edik(I, L, K), K > 0.

vet(Kövek, Luk, Állás, Állás2) :-
    vet_saját(Kövek, Luk, Állás, Állás1, Kövek1),
    vet_ellenfél(Kövek1, Állás1, Állás2).

vet_saját(Kövek, Luk, tábla(La,Na,Lb,Nb),
          tábla(La1,Na1,Lb,Nb), Kövek1) :-
    Kövek > 7 - Luk, !, % átmegy az ellenfélhez
    felvesz_és_szór(Luk, Kövek, La, La1),
    Na1 is Na + 1, Kövek1 is Kövek + Luk - 7.
vet_saját(Kövek, Luk,
          tábla(La,Na,Lb,Nb), Állás, 0) :-
    Kövek =< 7 - Luk,
    felvesz_és_szór(Luk, Kövek, La, La1),
    elfogás(Luk, Kövek, La1, La2, Lb, Lb1, N),
    raktározás(N, Kövek, Luk, Na, Na1),
    vetés_vége(tábla(La2,Na1,Lb1,Nb), Állás).

felvesz_és_szór(0, K, L, L1) :- % szórás folytatása
    !, szór(K, L, L1).
felvesz_és_szór(1, K, [_|M], [0|M1]) :-
    !, szór(K, M, M1).
felvesz_és_szór(Luk, K, [L|M], [L|M1]) :-
    Luk > 1, !, Luk1 is Luk - 1,
    felvesz_és_szór(Luk1, K, M, M1).

szór(0, L, L) :- !.
szór(N, [L|M], [L1|M1]) :-
    N > 0, !,
    N1 is N - 1, L1 is L + 1,
    szór(N1, M, M1).
szór(_, [], []) :- !.

elfogás(Luk, Kövek, La, La1, Lb, Lb1, N) :-
    Vége is Luk + Kövek,
    n_edik(Vége, La, 1),
    Szemben is 7 - Vége,
    n_edik(Szemben, Lb, K),
    K > 0, !, % üresbe érkeztünk és van szemben kő
    n_csere(Vége, La, 0, La1),
    n_csere(Szemben, Lb, 0, Lb1),
    N is K + 1.
elfogás(_, _, La, La, Lb, Lb, 0) :- !.

raktározás(0, Kövek, Luk, Na, Na) :-
    Kövek < 7 - Luk, !.
raktározás(0, Kövek, Luk, Na, Na1) :-
    Kövek =:= 7 - Luk, !, Na1 is Na + 1.
raktározás(N, _, _, Na, Na1) :-
    N > 0, !, Na1 is Na + N.

vetés_vége(tábla(La,Na,Lb,Nb),
           tábla(La,Na,La,Nb1)) :-
    üres(La), !, összeg(Lb, X), Nb1 is Nb + X.
vetés_vége(tábla(La,Na,Lb,Nb),
           tábla(Lb,Na1,Lb,Nb)) :-
    üres(Lb), !, összeg(La, X), Na1 is Na + X.
vetés_vége(Állás, Állás) :- !.

üres([0,0,0,0,0,0]).

vet_ellenfél(0, Állás, Állás) :- !.
vet_ellenfél(Kövek, tábla(La,Na,Lb,Nb),
             tábla(La,Na,Lb1,Nb)) :-
    1 =< Kövek, Kövek =< 6,
    \+ üres(La), !,
    szór(Kövek, Lb, Lb1).
vet_ellenfél(Kövek, tábla(La,Na,Lb,Nb),
             tábla(La,Na,La,Nb1)) :-
    1 =< Kövek, Kövek =< 6,
    üres(La), !,
    összeg(Lb, X), Nb1 is Nb + Kövek + X.
vet_ellenfél(Kövek, tábla(La,Na,Lb,Nb), Állás) :-
    Kövek > 6, !,
    szór(6, Lb, Lb1),
    Kövek1 is Kövek - 6,
    vet(Kövek1, 0, tábla(La,Na,Lb1,Nb), Állás).

% Kirajzolás

kirajzol(Állás, számítógép) :- kirajzol(Állás).
kirajzol(Állás, ember) :-
    megfordít(Állás, Állás1),
    kirajzol(Állás1).

kirajzol(tábla(La,Na,Lb,Nb)) :-
    nl,
    fordított(La, F),
    sort_ír(F),
    kalahot_ír(Na, Nb),
    sort_ír(Lb).

sort_ír(L) :- behúz(5), lyukat_ír(L).

lyukat_ír([]) :- nl.
lyukat_ír([L|M]) :- köveket_ír(L), lyukat_ír(M).

köveket_ír(N) :- N < 10, write(N), behúz(4).
köveket_ír(N) :- N >= 10, write(N), behúz(3).

kalahot_ír(N1, N2) :-
    köveket_ír(N1), behúz(30),
    write(N2), nl.

% Alfa-béta nyírás

alfa_béta(0, Állás, _, _, _-Érték) :-
    értékelés(Állás, Érték).
alfa_béta(E, Állás, A, B, Lépés-Érték) :-
    E > 0, E1 is E - 1,
    A1 is -B, B1 is -A,
    findall(L, lépés(Állás, L), Lépések),
    választ(Lépések, Állás, E1, A1, B1,
            nincs, Lépés-Érték).

választ([], _, _, A, _, Legjobb, Legjobb-A).
választ([Lépés|M], Állás, E, A, B, Legjobb, X) :-
    lép(Lépés, Állás, Állás1),
    alfa_béta(E, Állás1, A, B, _-Érték),
    Érték1 is -Érték,
    nyír(Lépés-Érték1, E, A, B, M, Állás,
         Legjobb, X).

nyír(Lépés-Érték, _, _, B, _, _, _, Lépés-Érték) :-
    Érték >= B.
nyír(Lépés-Érték, E, A, B, Többi, Állás, _, X) :-
    A < Érték, Érték < B,
    választ(Többi, Állás, E, Érték, B, Lépés, X).
nyír(_-Érték, E, A, B, Többi, Állás, Lépés1, X) :-
    Érték =< A,
    választ(Többi, Állás, E, A, B, Lépés1, X).

% Kalah-specifikus rész

lépés(tábla([0,0,0,0,0,0],_,_,_), []).
lépés(Állás, [L|M]) :-
    tartalmaz(L, [1,2,3,4,5,6]),
    kövek(L, Állás, K),
    lépést_folytat(K, L, Állás, M).

lépést_folytat(Kövek, L, _, []) :-
    Kövek =\= (7 - L) mod 13, !.
lépést_folytat(Kövek, L, Állás, Lépések) :-
    Kövek =:= (7 - L) mod 13, !,
    vet(Kövek, L, Állás, Állás1),
    lépés(Állás1, Lépések).

értékelés(tábla(_,Na,_,Nb), X) :- X is Na - Nb.

lépést_választ(_, ember, Lépés) :-
    nl, kiír('Melyiket választod?'),
    read(Lépés), érvényes(Lépés).
lépést_választ(Állás, számítógép, Lépés) :-
    előrelátás(E),
    alfa_béta(E, Állás, -40, 40, Lépés-_),
    nl, write(Lépés), nl.

érvényes([]).
érvényes([L|M]) :- 0 < L, L < 7, érvényes(M).

% Beállítások

kövek_száma(6).

előrelátás(4).

% Segéd-szabályok

kiír(X) :- write(X), nl.

behúz(0) :- !.
behúz(N) :-
    N > 0, N1 is N - 1,
    write(' '), behúz(N1).

tartalmaz(X, [X|_]).
tartalmaz(X, [_|Maradék]) :- tartalmaz(X, Maradék).

fordított(X, Y) :- fordított(X, [], Y).

fordított([], Y, Y).
fordított([X|M], F, Y) :- fordított(M, [X|F], Y). 

n_edik(N, [_|M], X) :-
    N > 1, !, N1 is N - 1,
    n_edik(N1, M, X).
n_edik(1, [X|_], X).

n_csere(1, [_|M], Y, [Y|M]) :- !.
n_csere(N, [X|M], Y, [X|M1]) :-
    N > 1, !, N1 is N - 1,
    n_csere(N1, M, Y, M1).

összeg(L, X) :- összeg(L, 0, X).

összeg([], A, A).
összeg([K|M], A, X) :-
    A1 is A + K,
    összeg(M, A1, X).
