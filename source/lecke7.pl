/*  Part of SWI-Prolog

    Author:        R.A.O'Keefe, L.Damas, V.S.Costa, Glenn Burgess,
                   Jiri Spitz and Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2004-2018, various people and institutions
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

% empty_assoc(?Assoc) [semidet]
empty_assoc(t).

% assoc_to_list(+Assoc, -Pairs) [det]
assoc_to_list(Assoc, List) :-
    assoc_to_list(Assoc, List, []).

assoc_to_list(t(Key,Val,_,L,R), List, Rest) :-
    assoc_to_list(L, List, [Key-Val|More]),
    assoc_to_list(R, More, Rest).
assoc_to_list(t, List, List).

% assoc_to_keys(+Assoc, -Keys) [det]
assoc_to_keys(Assoc, List) :-
    assoc_to_keys(Assoc, List, []).

assoc_to_keys(t(Key,_,_,L,R), List, Rest) :-
    assoc_to_keys(L, List, [Key|More]),
    assoc_to_keys(R, More, Rest).
assoc_to_keys(t, List, List).

% assoc_to_values(+Assoc, -Values) [det]
assoc_to_values(Assoc, List) :-
    assoc_to_values(Assoc, List, []).

assoc_to_values(t(_,Value,_,L,R), List, Rest) :-
    assoc_to_values(L, List, [Value|More]),
    assoc_to_values(R, More, Rest).
assoc_to_values(t, List, List).

% is_assoc(+Assoc) [semidet]
is_assoc(Assoc) :-
    is_assoc(Assoc, _Min, _Max, _Depth).

is_assoc(t,X,X,0) :- !.
is_assoc(t(K,_,-,t,t),K,K,1) :- !, ground(K).
is_assoc(t(K,_,>,t,t(RK,_,-,t,t)),K,RK,2) :-
    !, ground((K,RK)), K @< RK.

is_assoc(t(K,_,<,t(LK,_,-,t,t),t),LK,K,2) :-
    !, ground((LK,K)), LK @< K.

is_assoc(t(K,_,B,L,R),Min,Max,Depth) :-
    is_assoc(L,Min,LMax,LDepth),
    is_assoc(R,RMin,Max,RDepth),
    compare(Rel,RDepth,LDepth),
    balance(Rel,B),
    ground((LMax,K,RMin)),
    LMax @< K,
    K @< RMin,
    Depth is max(LDepth, RDepth)+1.

balance(=,-).
balance(<,<).
balance(>,>).

% gen_assoc(?Key, +Assoc, ?Value) [nondet]
gen_assoc(Key, Assoc, Value) :-
    (   ground(Key)
    ->  get_assoc(Key, Assoc, Value)
    ;   gen_assoc_(Key, Assoc, Value)
    ).

gen_assoc_(Key, t(_,_,_,L,_), Val) :-
    gen_assoc_(Key, L, Val).
gen_assoc_(Key, t(Key,Val,_,_,_), Val).
gen_assoc_(Key, t(_,_,_,_,R), Val) :-
    gen_assoc_(Key, R, Val).

% get_assoc(+Key, +Assoc, -Value) [semidet]
get_assoc(Key, t(K,V,_,L,R), Val) :-
    compare(Rel, Key, K),
    get_assoc(Rel, Key, V, L, R, Val).

get_assoc(=, _, Val, _, _, Val).
get_assoc(<, Key, _, Tree, _, Val) :-
    get_assoc(Key, Tree, Val).
get_assoc(>, Key, _, _, Tree, Val) :-
    get_assoc(Key, Tree, Val).

% get_assoc(+Key, +Assoc0, ?Val0, ?Assoc, ?Val)
%                                         [semidet]
get_assoc(Key, t(K,V,B,L,R), Val,
          t(K,NV,B,NL,NR), NVal) :-
    compare(Rel, Key, K),
    get_assoc(Rel, Key, V, L, R, Val,
              NV, NL, NR, NVal).

get_assoc(=, _, Val, L, R, Val, NVal, L, R, NVal).
get_assoc(<, Key, V, L, R, Val, V, NL, R, NVal) :-
    get_assoc(Key, L, Val, NL, NVal).
get_assoc(>, Key, V, L, R, Val, V, L, NR, NVal) :-
    get_assoc(Key, R, Val, NR, NVal).

% list_to_assoc(+Pairs, -Assoc) [det]
list_to_assoc(List, Assoc) :-
    (  List = [] -> Assoc = t
    ;  keysort(List, Sorted),
       length(Sorted, N),
       list_to_assoc(N, Sorted, [], _, Assoc)
    ).

list_to_assoc(1, [K-V|More], More,
              1, t(K,V,-,t,t)) :- !.
list_to_assoc(2, [K1-V1,K2-V2|More], More,
              2, t(K2,V2,<,t(K1,V1,-,t,t),t)) :- !.
list_to_assoc(N, List, More,
              Depth, t(K,V,Balance,L,R)) :-
    N0 is N - 1,
    RN is N0 div 2,
    Rem is N0 mod 2,
    LN is RN + Rem,
    list_to_assoc(LN, List, [K-V|Upper], LDepth, L),
    list_to_assoc(RN, Upper, More, RDepth, R),
    Depth is LDepth + 1,
    compare(B, RDepth, LDepth), balance(B, Balance).

% ord_list_to_assoc(+Pairs, -Assoc) [det]
ord_list_to_assoc(Sorted, Assoc) :-
    (  Sorted = [] -> Assoc = t
    ;  length(Sorted, N),
       list_to_assoc(N, Sorted, [], _, Assoc)
    ).

% map_assoc(:Pred, +Assoc) [semidet]
map_assoc(Pred, T) :-
    map_assoc_(T, Pred).

map_assoc_(t, _).
map_assoc_(t(_,Val,_,L,R), Pred) :-
    map_assoc_(L, Pred),
    call(Pred, Val),
    map_assoc_(R, Pred).

% map_assoc(:Pred, +Assoc0, ?Assoc) [semidet]
map_assoc(Pred, T0, T) :-
    map_assoc_(T0, Pred, T).

map_assoc_(t, _, t).
map_assoc_(t(Key,Val,B,L0,R0), Pred,
           t(Key,Ans,B,L1,R1)) :-
    map_assoc_(L0, Pred, L1),
    call(Pred, Val, Ans),
    map_assoc_(R0, Pred, R1).

% max_assoc(+Assoc, -Key, -Value) [semidet]
max_assoc(t(K,V,_,_,R), Key, Val) :-
    max_assoc(R, K, V, Key, Val).

max_assoc(t, K, V, K, V).
max_assoc(t(K,V,_,_,R), _, _, Key, Val) :-
    max_assoc(R, K, V, Key, Val).

% min_assoc(+Assoc, -Key, -Value) [semidet]
min_assoc(t(K,V,_,L,_), Key, Val) :-
    min_assoc(L, K, V, Key, Val).

min_assoc(t, K, V, K, V).
min_assoc(t(K,V,_,L,_), _, _, Key, Val) :-
    min_assoc(L, K, V, Key, Val).

% put_assoc(+Key, +Assoc0, +Value, -Assoc) [det]
put_assoc(Key, A0, Value, A) :-
    insert(A0, Key, Value, A, _).

insert(t, Key, Val, t(Key,Val,-,t,t), yes).
insert(t(Key,Val,B,L,R), K, V,
       NewTree, WhatHasChanged) :-
    compare(Rel, K, Key),
    insert(Rel, t(Key,Val,B,L,R), K, V,
           NewTree, WhatHasChanged).

insert(=, t(Key,_,B,L,R), _, V, t(Key,V,B,L,R), no).
insert(<, t(Key,Val,B,L,R), K, V,
       NewTree, WhatHasChanged) :-
    insert(L, K, V, NewL, LeftHasChanged),
    adjust(LeftHasChanged, t(Key,Val,B,NewL,R),
           left, NewTree, WhatHasChanged).
insert(>, t(Key,Val,B,L,R), K, V,
       NewTree, WhatHasChanged) :-
    insert(R, K, V, NewR, RightHasChanged),
    adjust(RightHasChanged, t(Key,Val,B,L,NewR),
           right, NewTree, WhatHasChanged).

adjust(no, Oldree, _, Oldree, no).
adjust(yes, t(Key,Val,B0,L,R), LoR,
       NewTree, WhatHasChanged) :-
    table(B0, LoR, B1,
          WhatHasChanged, ToBeRebalanced),
    rebalance(ToBeRebalanced, t(Key,Val,B0,L,R), B1,
              NewTree, _, _).

table(-, left , <, yes, no ) :- !.
table(-, right, >, yes, no ) :- !.
table(<, left , -, no , yes) :- !.
table(<, right, -, no , no ) :- !.
table(>, left , -, no , no ) :- !.
table(>, right, -, no , yes) :- !.

% del_min_assoc(+Assoc0, ?Key, ?Val, -Assoc)
%                                         [semidet]
del_min_assoc(Tree, Key, Val, NewTree) :-
    del_min_assoc(Tree, Key, Val,
                  NewTree, _DepthChanged).

del_min_assoc(t(Key,Val,_B,t,R), Key, Val,
              R, yes) :- !.
del_min_assoc(t(K,V,B,L,R), Key, Val,
              NewTree, Changed) :-
    del_min_assoc(L, Key, Val, NewL, LeftChanged),
    deladjust(LeftChanged, t(K,V,B,NewL,R),
              left, NewTree, Changed).

% del_max_assoc(+Assoc0, ?Key, ?Val, -Assoc)
%                                         [semidet]
del_max_assoc(Tree, Key, Val, NewTree) :-
    del_max_assoc(Tree, Key, Val,
                  NewTree, _DepthChanged).

del_max_assoc(t(Key,Val,_B,L,t), Key, Val,
              L, yes) :- !.
del_max_assoc(t(K,V,B,L,R), Key, Val,
              NewTree, Changed) :-
    del_max_assoc(R, Key, Val, NewR, RightChanged),
    deladjust(RightChanged, t(K,V,B,L,NewR),
              right, NewTree, Changed).

% del_assoc(+Key, +Assoc0, ?Value, -Assoc) [semidet]
del_assoc(Key, A0, Value, A) :-
    delete(A0, Key, Value, A, _).

delete(t(Key,Val,B,L,R), K, V,
       NewTree, WhatHasChanged) :-
    compare(Rel, K, Key),
    delete(Rel, t(Key,Val,B,L,R), K, V,
           NewTree, WhatHasChanged).

delete(=, t(Key,Val,_B,t,R), Key, Val, R, yes) :- !.
delete(=, t(Key,Val,_B,L,t), Key, Val, L, yes) :- !.
delete(=, t(Key,Val,>,L,R), Key, Val,
       NewTree, WhatHasChanged) :-
    del_min_assoc(R, K, V, NewR, RightHasChanged),
    deladjust(RightHasChanged, t(K,V,>,L,NewR),
              right, NewTree, WhatHasChanged),
    !.
delete(=, t(Key,Val,B,L,R), Key, Val,
       NewTree, WhatHasChanged) :-
    del_max_assoc(L, K, V, NewL, LeftHasChanged),
    deladjust(LeftHasChanged, t(K,V,B,NewL,R),
              left, NewTree, WhatHasChanged),
    !.

delete(<, t(Key,Val,B,L,R), K, V,
       NewTree, WhatHasChanged) :-
    delete(L, K, V, NewL, LeftHasChanged),
    deladjust(LeftHasChanged, t(Key,Val,B,NewL,R),
              left, NewTree, WhatHasChanged).
delete(>, t(Key,Val,B,L,R), K, V,
       NewTree, WhatHasChanged) :-
    delete(R, K, V, NewR, RightHasChanged),
    deladjust(RightHasChanged, t(Key,Val,B,L,NewR),
              right, NewTree, WhatHasChanged).

deladjust(no, OldTree, _, OldTree, no).
deladjust(yes, t(Key,Val,B0,L,R), LoR,
          NewTree, RealChange) :-
    deltable(B0, LoR, B1,
             WhatHasChanged, ToBeRebalanced),
    rebalance(ToBeRebalanced, t(Key,Val,B0,L,R), B1,
              NewTree, WhatHasChanged, RealChange).

deltable(-, right, <, no , no ) :- !.
deltable(-, left , >, no , no ) :- !.
deltable(<, right, -, yes, yes) :- !.
deltable(<, left , -, yes, no ) :- !.
deltable(>, right, -, yes, no ) :- !.
deltable(>, left , -, yes, yes) :- !.

rebalance(no, t(K,V,_,L,R), B, t(K,V,B,L,R),
          Changed, Changed).
rebalance(yes, OldTree, _, NewTree,
          _, RealChange) :-
    avl_geq(OldTree, NewTree, RealChange).

avl_geq(t(A,VA,>,Alpha,t(B,VB,>,Beta,Gamma)),
        t(B,VB,-,t(A,VA,-,Alpha,Beta),Gamma),
        yes) :- !.
avl_geq(t(A,VA,>,Alpha,t(B,VB,-,Beta,Gamma)),
        t(B,VB,<,t(A,VA,>,Alpha,Beta),Gamma),
        no) :- !.
avl_geq(t(B,VB,<,t(A,VA,<,Alpha,Beta),Gamma),
        t(A,VA,-,Alpha,t(B,VB,-,Beta,Gamma)),
        yes) :- !.
avl_geq(t(B,VB,<,t(A,VA,-,Alpha,Beta),Gamma),
        t(A,VA,>,Alpha,t(B,VB,<,Beta,Gamma)),
        no) :- !.
avl_geq(t(A,VA,>,Alpha,
            t(B,VB,<,t(X,VX,B1,Beta,Gamma),Delta)),
        t(X,VX,-,t(A,VA,B2,Alpha,Beta),
            t(B,VB,B3,Gamma,Delta)),
        yes) :-
    !,
    table2(B1, B2, B3).
avl_geq(t(B,VB,<,
            t(A,VA,>,Alpha,t(X,VX,B1,Beta,Gamma)),
            Delta),
        t(X,VX,-,t(A,VA,B2,Alpha,Beta),
            t(B,VB,B3,Gamma,Delta)),
        yes) :-
    !,
    table2(B1, B2, B3).

table2(< ,- ,> ).
table2(> ,< ,- ).
table2(- ,- ,- ).
