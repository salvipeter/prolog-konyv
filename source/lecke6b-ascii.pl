% Magas szintu keretprogram

kalah :-
    alapbeallitas(Allas, Jatekos),
    kirajzol(Allas, Jatekos),
    jatek(Allas, Jatekos).

jatek(Allas, Jatekos) :-
    jatek_vege(Allas, Jatekos, Eredmeny), !,
    bejelent(Eredmeny).
jatek(Allas, Jatekos) :-
    lepest_valaszt(Allas, Jatekos, Lepes),
    lep(Lepes, Allas, Allas1),
    kovetkezo_jatekos(Jatekos, Jatekos1), !,
    kirajzol(Allas1, Jatekos1),
    jatek(Allas1, Jatekos1).

kovetkezo_jatekos(ember, szamitogep).
kovetkezo_jatekos(szamitogep, ember).

bejelent(ember) :- kiir('Nyertel, gratulalok!').
bejelent(szamitogep) :- kiir('Nyertem.').
bejelent(dontetlen) :- kiir('Dontetlen lett!').

% Reprezentacio

alapbeallitas(Allas, ember) :-
    kovek_szama(K),
    Allas = tabla([K,K,K,K,K,K],0,
                  [K,K,K,K,K,K],0).

% Szabalyok

jatek_vege(tabla(_,N,_,N), _, dontetlen) :-
    kovek_szama(K), N =:= 6 * K, !.
jatek_vege(tabla(_,N1,_,_), Jatekos, Jatekos) :-
    kovek_szama(K), N1 > 6 * K, !.
jatek_vege(tabla(_,_,_,N2), Jatekos, Masik) :-
    kovek_szama(K), N2 > 6 * K,
    kovetkezo_jatekos(Jatekos, Masik).

lep([], Allas, Allas1) :- megfordit(Allas, Allas1).
lep([L|M], Allas, Allas2) :-
    kovek(L, Allas, K),
    vet(K, L, Allas, Allas1),
    lep(M, Allas1, Allas2).

megfordit(tabla(La,Na,Lb,Nb), tabla(Lb,Nb,La,Na)).

kovek(I, tabla(L,_,_,_), K) :- n_edik(I, L, K), K > 0.

vet(Kovek, Luk, Allas, Allas2) :-
    vet_sajat(Kovek, Luk, Allas, Allas1, Kovek1),
    vet_ellenfel(Kovek1, Allas1, Allas2).

vet_sajat(Kovek, Luk, tabla(La,Na,Lb,Nb),
          tabla(La1,Na1,Lb,Nb), Kovek1) :-
    Kovek > 7 - Luk, !, % atmegy az ellenfelhez
    felvesz_es_szor(Luk, Kovek, La, La1),
    Na1 is Na + 1, Kovek1 is Kovek + Luk - 7.
vet_sajat(Kovek, Luk,
          tabla(La,Na,Lb,Nb), Allas, 0) :-
    Kovek =< 7 - Luk,
    felvesz_es_szor(Luk, Kovek, La, La1),
    elfogas(Luk, Kovek, La1, La2, Lb, Lb1, N),
    raktarozas(N, Kovek, Luk, Na, Na1),
    vetes_vege(tabla(La2,Na1,Lb1,Nb), Allas).

felvesz_es_szor(0, K, L, L1) :- % szoras folytatasa
    !, szor(K, L, L1).
felvesz_es_szor(1, K, [_|M], [0|M1]) :-
    !, szor(K, M, M1).
felvesz_es_szor(Luk, K, [L|M], [L|M1]) :-
    Luk > 1, !, Luk1 is Luk - 1,
    felvesz_es_szor(Luk1, K, M, M1).

szor(0, L, L) :- !.
szor(N, [L|M], [L1|M1]) :-
    N > 0, !,
    N1 is N - 1, L1 is L + 1,
    szor(N1, M, M1).
szor(_, [], []) :- !.

elfogas(Luk, Kovek, La, La1, Lb, Lb1, N) :-
    Vege is Luk + Kovek,
    n_edik(Vege, La, 1),
    Szemben is 7 - Vege,
    n_edik(Szemben, Lb, K),
    K > 0, !, % uresbe erkeztunk es van szemben ko
    n_csere(Vege, La, 0, La1),
    n_csere(Szemben, Lb, 0, Lb1),
    N is K + 1.
elfogas(_, _, La, La, Lb, Lb, 0) :- !.

raktarozas(0, Kovek, Luk, Na, Na) :-
    Kovek < 7 - Luk, !.
raktarozas(0, Kovek, Luk, Na, Na1) :-
    Kovek =:= 7 - Luk, !, Na1 is Na + 1.
raktarozas(N, _, _, Na, Na1) :-
    N > 0, !, Na1 is Na + N.

vetes_vege(tabla(La,Na,Lb,Nb),
           tabla(La,Na,La,Nb1)) :-
    ures(La), !, osszeg(Lb, X), Nb1 is Nb + X.
vetes_vege(tabla(La,Na,Lb,Nb),
           tabla(Lb,Na1,Lb,Nb)) :-
    ures(Lb), !, osszeg(La, X), Na1 is Na + X.
vetes_vege(Allas, Allas) :- !.

ures([0,0,0,0,0,0]).

vet_ellenfel(0, Allas, Allas) :- !.
vet_ellenfel(Kovek, tabla(La,Na,Lb,Nb),
             tabla(La,Na,Lb1,Nb)) :-
    1 =< Kovek, Kovek =< 6,
    \+ ures(La), !,
    szor(Kovek, Lb, Lb1).
vet_ellenfel(Kovek, tabla(La,Na,Lb,Nb),
             tabla(La,Na,La,Nb1)) :-
    1 =< Kovek, Kovek =< 6,
    ures(La), !,
    osszeg(Lb, X), Nb1 is Nb + Kovek + X.
vet_ellenfel(Kovek, tabla(La,Na,Lb,Nb), Allas) :-
    Kovek > 6, !,
    szor(6, Lb, Lb1),
    Kovek1 is Kovek - 6,
    vet(Kovek1, 0, tabla(La,Na,Lb1,Nb), Allas).

% Kirajzolas

kirajzol(Allas, szamitogep) :- kirajzol(Allas).
kirajzol(Allas, ember) :-
    megfordit(Allas, Allas1),
    kirajzol(Allas1).

kirajzol(tabla(La,Na,Lb,Nb)) :-
    nl,
    forditott(La, F),
    sort_ir(F),
    kalahot_ir(Na, Nb),
    sort_ir(Lb).

sort_ir(L) :- behuz(5), lyukat_ir(L).

lyukat_ir([]) :- nl.
lyukat_ir([L|M]) :- koveket_ir(L), lyukat_ir(M).

koveket_ir(N) :- N < 10, write(N), behuz(4).
koveket_ir(N) :- N >= 10, write(N), behuz(3).

kalahot_ir(N1, N2) :-
    koveket_ir(N1), behuz(30),
    write(N2), nl.

% Alfa-beta nyiras

alfa_beta(0, Allas, _, _, _-Ertek) :-
    ertekeles(Allas, Ertek).
alfa_beta(E, Allas, A, B, Lepes-Ertek) :-
    E > 0, E1 is E - 1,
    A1 is -B, B1 is -A,
    findall(L, lepes(Allas, L), Lepesek),
    valaszt(Lepesek, Allas, E1, A1, B1,
            nincs, Lepes-Ertek).

valaszt([], _, _, A, _, Legjobb, Legjobb-A).
valaszt([Lepes|M], Allas, E, A, B, Legjobb, X) :-
    lep(Lepes, Allas, Allas1),
    alfa_beta(E, Allas1, A, B, _-Ertek),
    Ertek1 is -Ertek,
    nyir(Lepes-Ertek1, E, A, B, M, Allas,
         Legjobb, X).

nyir(Lepes-Ertek, _, _, B, _, _, _, Lepes-Ertek) :-
    Ertek >= B.
nyir(Lepes-Ertek, E, A, B, Tobbi, Allas, _, X) :-
    A < Ertek, Ertek < B,
    valaszt(Tobbi, Allas, E, Ertek, B, Lepes, X).
nyir(_-Ertek, E, A, B, Tobbi, Allas, Lepes1, X) :-
    Ertek =< A,
    valaszt(Tobbi, Allas, E, A, B, Lepes1, X).

% Kalah-specifikus resz

lepes(tabla([0,0,0,0,0,0],_,_,_), []).
lepes(Allas, [L|M]) :-
    tartalmaz(L, [1,2,3,4,5,6]),
    kovek(L, Allas, K),
    lepest_folytat(K, L, Allas, M).

lepest_folytat(Kovek, L, _, []) :-
    Kovek =\= (7 - L) mod 13, !.
lepest_folytat(Kovek, L, Allas, Lepesek) :-
    Kovek =:= (7 - L) mod 13, !,
    vet(Kovek, L, Allas, Allas1),
    lepes(Allas1, Lepesek).

ertekeles(tabla(_,Na,_,Nb), X) :- X is Na - Nb.

lepest_valaszt(_, ember, Lepes) :-
    nl, kiir('Melyiket valasztod?'),
    read(Lepes), ervenyes(Lepes).
lepest_valaszt(Allas, szamitogep, Lepes) :-
    elorelatas(E),
    alfa_beta(E, Allas, -40, 40, Lepes-_),
    nl, write(Lepes), nl.

ervenyes([]).
ervenyes([L|M]) :- 0 < L, L < 7, ervenyes(M).

% Beallitasok

kovek_szama(6).

elorelatas(4).

% Seged-szabalyok

kiir(X) :- write(X), nl.

behuz(0) :- !.
behuz(N) :-
    N > 0, N1 is N - 1,
    write(' '), behuz(N1).

tartalmaz(X, [X|_]).
tartalmaz(X, [_|Maradek]) :- tartalmaz(X, Maradek).

forditott(X, Y) :- forditott(X, [], Y).

forditott([], Y, Y).
forditott([X|M], F, Y) :- forditott(M, [X|F], Y). 

n_edik(N, [_|M], X) :-
    N > 1, !, N1 is N - 1,
    n_edik(N1, M, X).
n_edik(1, [X|_], X).

n_csere(1, [_|M], Y, [Y|M]) :- !.
n_csere(N, [X|M], Y, [X|M1]) :-
    N > 1, !, N1 is N - 1,
    n_csere(N1, M, Y, M1).

osszeg(L, X) :- osszeg(L, 0, X).

osszeg([], A, A).
osszeg([K|M], A, X) :-
    A1 is A + K,
    osszeg(M, A1, X).
