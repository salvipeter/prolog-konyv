% szulo(X, Y) => X az Y szuloje
szulo(abu_talib, ali).
szulo(abdulla, mohamed).
szulo(amna, mohamed).
szulo(mohamed, fatima).
szulo(hadidzsa, fatima).
szulo(mohamed, zajnab).
szulo(hadidzsa, zajnab).
szulo(ali, huszajn).
szulo(fatima, huszajn).
szulo(ali, muhszin).
szulo(fatima, muhszin).
szulo(ali, haszan).
szulo(fatima, haszan).
szulo(zajnab, umama).

ferfi(abu_talib).
ferfi(abdulla).
ferfi(mohamed).
ferfi(ali).
ferfi(huszajn).
ferfi(muhszin).
ferfi(haszan).

no(amna).
no(hadidzsa).
no(fatima).
no(zajnab).
no(umama).

anya(X, Y) :- szulo(X, Y), no(X).   % X az Y anyja
apa(X, Y) :- szulo(X, Y), ferfi(X). % X az Y apja

nagyszulo(X, Z) :- szulo(X, Y), szulo(Y, Z).

fiver(X, Y) :-
    ferfi(X),
    szulo(Z, X), szulo(Z, Y),
    X \= Y.

vangyereke(X) :- szulo(X, _).

os(X, Z) :- szulo(X, Z).
os(X, Z) :- szulo(X, Y), os(Y, Z).
