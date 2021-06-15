lapok([[bagoly,balna,hal,kacsa,rak,tehen],
       [bagoly,barany,elefant,polip,teknos,teve],
       [bagoly,beka,delfin,gorilla,kigyo,kutya],
       [bagoly,capa,cica,kakas,nyuszi,pingvin],
       [bagoly,katica,kenguru,krokodil,tigris,zebra],
       [bagoly,lo,medve,oroszlan,papagaj,vizilo],
       [balna,barany,cica,delfin,kenguru,papagaj],
       [balna,beka,capa,lo,teknos,zebra],
       [balna,elefant,kutya,nyuszi,oroszlan,tigris],
       [balna,gorilla,kakas,krokodil,medve,teve],
       [balna,katica,kigyo,pingvin,polip,vizilo],
       [barany,beka,kacsa,kakas,katica,oroszlan],
       [barany,capa,gorilla,hal,tigris,vizilo],
       [barany,kigyo,krokodil,lo,nyuszi,tehen],
       [barany,kutya,medve,pingvin,rak,zebra],
       [beka,cica,elefant,krokodil,rak,vizilo],
       [beka,hal,kenguru,medve,nyuszi,polip],
       [beka,papagaj,pingvin,tehen,teve,tigris],
       [capa,delfin,elefant,katica,medve,tehen],
       [capa,kacsa,krokodil,kutya,papagaj,polip],
       [capa,kenguru,kigyo,oroszlan,rak,teve],
       [cica,gorilla,oroszlan,polip,tehen,zebra],
       [cica,kacsa,kigyo,medve,teknos,tigris],
       [delfin,hal,krokodil,oroszlan,pingvin,teknos],
       [delfin,kacsa,nyuszi,teve,vizilo,zebra],
       [delfin,kakas,lo,polip,rak,tigris],
       [elefant,gorilla,kacsa,kenguru,lo,pingvin],
       [elefant,hal,kakas,kigyo,papagaj,zebra],
       [gorilla,katica,nyuszi,papagaj,rak,teknos],
       [kakas,kenguru,kutya,tehen,teknos,vizilo]]).

allatok([bagoly,balna,barany,beka,capa,cica,delfin,
         elefant,gorilla,hal,kacsa,kakas,katica,
         kenguru,kigyo,krokodil,kutya,lo,medve,
         nyuszi,oroszlan,papagaj,pingvin,polip,rak,
         tehen,teknos,teve,tigris,vizilo,zebra]).

valaszt6(A, L) :-
    L = [_, _, _, _, _, _],
    reszhalmaz(A, L).

reszhalmaz([], []).
reszhalmaz([X|M], [X|R]) :- reszhalmaz(M, R).
reszhalmaz([_|M], R) :- reszhalmaz(M, R).

jo_lap([], _).
jo_lap([L|M], X) :- metszet(X, L, [_]), jo_lap(M, X).

metszet([], _, []).
metszet([X|L1], L2, [X|L3]) :-
    tartalmaz(X, L2), metszet(L1, L2, L3).
metszet([X|L1], L2, L3) :-
    nemtartalmaz(X, L2), metszet(L1, L2, L3).

tartalmaz(X, [X|_]).
tartalmaz(X, [_|Maradek]) :- tartalmaz(X, Maradek).

nemtartalmaz(_, []).
nemtartalmaz(X, [Y|M]) :- X \= Y, nemtartalmaz(X, M).

megoldas(X) :-
    allatok(A), lapok(L),
    valaszt6(A, X), jo_lap(L, X).
