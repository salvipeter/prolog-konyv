lapok([[bagoly,bálna,hal,kacsa,rák,tehén],
       [bagoly,bárány,elefánt,polip,teknős,teve],
       [bagoly,béka,delfin,gorilla,kígyó,kutya],
       [bagoly,cápa,cica,kakas,nyuszi,pingvin],
       [bagoly,katica,kenguru,krokodil,tigris,zebra],
       [bagoly,ló,medve,oroszlán,papagáj,víziló],
       [bálna,bárány,cica,delfin,kenguru,papagáj],
       [bálna,béka,cápa,ló,teknős,zebra],
       [bálna,elefánt,kutya,nyuszi,oroszlán,tigris],
       [bálna,gorilla,kakas,krokodil,medve,teve],
       [bálna,katica,kígyó,pingvin,polip,víziló],
       [bárány,béka,kacsa,kakas,katica,oroszlán],
       [bárány,cápa,gorilla,hal,tigris,víziló],
       [bárány,kígyó,krokodil,ló,nyuszi,tehén],
       [bárány,kutya,medve,pingvin,rák,zebra],
       [béka,cica,elefánt,krokodil,rák,víziló],
       [béka,hal,kenguru,medve,nyuszi,polip],
       [béka,papagáj,pingvin,tehén,teve,tigris],
       [cápa,delfin,elefánt,katica,medve,tehén],
       [cápa,kacsa,krokodil,kutya,papagáj,polip],
       [cápa,kenguru,kígyó,oroszlán,rák,teve],
       [cica,gorilla,oroszlán,polip,tehén,zebra],
       [cica,kacsa,kígyó,medve,teknős,tigris],
       [delfin,hal,krokodil,oroszlán,pingvin,teknős],
       [delfin,kacsa,nyuszi,teve,víziló,zebra],
       [delfin,kakas,ló,polip,rák,tigris],
       [elefánt,gorilla,kacsa,kenguru,ló,pingvin],
       [elefánt,hal,kakas,kígyó,papagáj,zebra],
       [gorilla,katica,nyuszi,papagáj,rák,teknős],
       [kakas,kenguru,kutya,tehén,teknős,víziló]]).

állatok([bagoly,bálna,bárány,béka,cápa,cica,delfin,
         elefánt,gorilla,hal,kacsa,kakas,katica,
         kenguru,kígyó,krokodil,kutya,ló,medve,
         nyuszi,oroszlán,papagáj,pingvin,polip,rák,
         tehén,teknős,teve,tigris,víziló,zebra]).

választ6(A, L) :-
    L = [_, _, _, _, _, _],
    részhalmaz(A, L).

részhalmaz([], []).
részhalmaz([X|M], [X|R]) :- részhalmaz(M, R).
részhalmaz([_|M], R) :- részhalmaz(M, R).

jó_lap([], _).
jó_lap([L|M], X) :- metszet(X, L, [_]), jó_lap(M, X).

metszet([], _, []).
metszet([X|L1], L2, [X|L3]) :-
    tartalmaz(X, L2), metszet(L1, L2, L3).
metszet([X|L1], L2, L3) :-
    nemtartalmaz(X, L2), metszet(L1, L2, L3).

tartalmaz(X, [X|_]).
tartalmaz(X, [_|Maradék]) :- tartalmaz(X, Maradék).

nemtartalmaz(_, []).
nemtartalmaz(X, [Y|M]) :- X \= Y, nemtartalmaz(X, M).

megoldás(X) :-
    állatok(A), lapok(L),
    választ6(A, X), jó_lap(L, X).
