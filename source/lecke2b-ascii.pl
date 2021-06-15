lap(l(fekete, lila, sarga, kek, zold, piros)).
lap(l(fekete, zold, piros, kek, sarga, lila)).
lap(l(fekete, zold, lila, sarga, kek, piros)).
lap(l(fekete, lila, piros, sarga, zold, kek)).
lap(l(fekete, piros, kek, sarga, zold, lila)).
lap(l(fekete, sarga, zold, kek, piros, lila)).
lap(l(fekete, zold, piros, lila, sarga, kek)).

forgat(l(A,B,C,D,E,F), l(A,B,C,D,E,F)).
forgat(l(A,B,C,D,E,F), l(B,C,D,E,F,A)).
forgat(l(A,B,C,D,E,F), l(C,D,E,F,A,B)).
forgat(l(A,B,C,D,E,F), l(D,E,F,A,B,C)).
forgat(l(A,B,C,D,E,F), l(E,F,A,B,C,D)).
forgat(l(A,B,C,D,E,F), l(F,A,B,C,D,E)).

kapcsolodik(L1, L2, dny, F1, F2) :-
    forgat(L1, F1), forgat(L2, F2),
    F1 = l(X,_,_,_,_,_),
    F2 = l(_,_,_,X,_,_).
kapcsolodik(L1, L2, d, F1, F2) :-
    forgat(L1, F1), forgat(L2, F2),
    F1 = l(_,X,_,_,_,_),
    F2 = l(_,_,_,_,X,_).
kapcsolodik(L1, L2, dk, F1, F2) :-
    forgat(L1, F1), forgat(L2, F2),
    F1 = l(_,_,X,_,_,_),
    F2 = l(_,_,_,_,_,X).
kapcsolodik(L1, L2, ek, F1, F2) :-
    forgat(L1, F1), forgat(L2, F2),
    F1 = l(_,_,_,X,_,_),
    F2 = l(X,_,_,_,_,_).
kapcsolodik(L1, L2, e, F1, F2) :-
    forgat(L1, F1), forgat(L2, F2),
    F1 = l(_,_,_,_,X,_),
    F2 = l(_,X,_,_,_,_).
kapcsolodik(L1, L2, eny, F1, F2) :-
    forgat(L1, F1), forgat(L2, F2),
    F1 = l(_,_,_,_,_,X),
    F2 = l(_,_,X,_,_,_).

megoldas(F1, F2, F3, F4, F5, F6, F7) :-
    lap(L1),
    lap(L2), L2 \= L1,
    lap(L3), L3 \= L1, L3 \= L2,
    lap(L4), L4 \= L1, L4 \= L2, L4 \= L3,
    lap(L5), L5 \= L1, L5 \= L2, L5 \= L3, L5 \= L4,
    lap(L6), L6 \= L1, L6 \= L2, L6 \= L3, L6 \= L4,
             L6 \= L5,
    lap(L7), L7 \= L1, L7 \= L2, L7 \= L3, L7 \= L4,
             L7 \= L5, L7 \= L6,
    kapcsolodik(L1, L2, dk,  F1, F2),
    kapcsolodik(F1, L7, ek,  F1, F7),
    kapcsolodik(F2, L3, ek,  F2, F3),
    kapcsolodik(F2, F7, e,   F2, F7),
    kapcsolodik(F3, L4, e,   F3, F4),
    kapcsolodik(F3, F7, eny, F3, F7),
    kapcsolodik(F4, L5, eny, F4, F5),
    kapcsolodik(F4, F7, dny, F4, F7),
    kapcsolodik(F5, L6, dny, F5, F6),
    kapcsolodik(F5, F7, d,   F5, F7),
    kapcsolodik(F6, F1, d,   F6, F1),
    kapcsolodik(F6, F7, dk,  F6, F7).
