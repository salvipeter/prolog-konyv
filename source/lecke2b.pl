lap(l(fekete,lila,sárga,kék,zöld,piros)).
lap(l(fekete,zöld,piros,kék,sárga,lila)).
lap(l(fekete,zöld,lila,sárga,kék,piros)).
lap(l(fekete,lila,piros,sárga,zöld,kék)).
lap(l(fekete,piros,kék,sárga,zöld,lila)).
lap(l(fekete,sárga,zöld,kék,piros,lila)).
lap(l(fekete,zöld,piros,lila,sárga,kék)).

forgat(l(A,B,C,D,E,F), l(A,B,C,D,E,F)).
forgat(l(A,B,C,D,E,F), l(B,C,D,E,F,A)).
forgat(l(A,B,C,D,E,F), l(C,D,E,F,A,B)).
forgat(l(A,B,C,D,E,F), l(D,E,F,A,B,C)).
forgat(l(A,B,C,D,E,F), l(E,F,A,B,C,D)).
forgat(l(A,B,C,D,E,F), l(F,A,B,C,D,E)).

kapcsolódik(L1, L2, dny, F1, F2) :-
    forgat(L1, F1), forgat(L2, F2),
    F1 = l(X,_,_,_,_,_),
    F2 = l(_,_,_,X,_,_).
kapcsolódik(L1, L2, d, F1, F2) :-
    forgat(L1, F1), forgat(L2, F2),
    F1 = l(_,X,_,_,_,_),
    F2 = l(_,_,_,_,X,_).
kapcsolódik(L1, L2, dk, F1, F2) :-
    forgat(L1, F1), forgat(L2, F2),
    F1 = l(_,_,X,_,_,_),
    F2 = l(_,_,_,_,_,X).
kapcsolódik(L1, L2, ék, F1, F2) :-
    forgat(L1, F1), forgat(L2, F2),
    F1 = l(_,_,_,X,_,_),
    F2 = l(X,_,_,_,_,_).
kapcsolódik(L1, L2, é, F1, F2) :-
    forgat(L1, F1), forgat(L2, F2),
    F1 = l(_,_,_,_,X,_),
    F2 = l(_,X,_,_,_,_).
kapcsolódik(L1, L2, ény, F1, F2) :-
    forgat(L1, F1), forgat(L2, F2),
    F1 = l(_,_,_,_,_,X),
    F2 = l(_,_,X,_,_,_).

megoldás(F1, F2, F3, F4, F5, F6, F7) :-
    lap(L1),
    lap(L2), L2 \= L1,
    lap(L3), L3 \= L1, L3 \= L2,
    lap(L4), L4 \= L1, L4 \= L2, L4 \= L3,
    lap(L5), L5 \= L1, L5 \= L2, L5 \= L3, L5 \= L4,
    lap(L6), L6 \= L1, L6 \= L2, L6 \= L3, L6 \= L4,
             L6 \= L5,
    lap(L7), L7 \= L1, L7 \= L2, L7 \= L3, L7 \= L4,
             L7 \= L5, L7 \= L6,
    kapcsolódik(L1, L2, dk,  F1, F2),
    kapcsolódik(F1, L7, ék,  F1, F7),
    kapcsolódik(F2, L3, ék,  F2, F3),
    kapcsolódik(F2, F7, é,   F2, F7),
    kapcsolódik(F3, L4, é,   F3, F4),
    kapcsolódik(F3, F7, ény, F3, F7),
    kapcsolódik(F4, L5, ény, F4, F5),
    kapcsolódik(F4, F7, dny, F4, F7),
    kapcsolódik(F5, L6, dny, F5, F6),
    kapcsolódik(F5, F7, d,   F5, F7),
    kapcsolódik(F6, F1, d,   F6, F1),
    kapcsolódik(F6, F7, dk,  F6, F7).
