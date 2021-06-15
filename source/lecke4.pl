lnko(N, N, N).
lnko(N, M, O) :-
    N < M,
    M1 is M - N,
    lnko(N, M1, O).
lnko(N, M, O) :-
    N > M,
    lnko(M, N, O).

hossz([], 0).
hossz([_|M], N) :- hossz(M, N1), N is 1 + N1.

hossz2([], N, N).
hossz2([_|M], C, N) :- C1 is 1 + C, hossz2(M, C1, N).
hossz2(L, N) :- hossz2(L, 0, N).

fib(1, 1).
fib(2, 1).
fib(N, M) :-
    N1 is N - 1, N2 is N - 2,
    fib(N1, K1), fib(N2, K2),
    M is K1 + K2.

fib2(1, M, _, M).
fib2(N, K1, K2, M) :-
    N1 is N - 1, K3 is K1 + K2,
    fib2(N1, K2, K3, M).
fib2(N, M) :- fib2(N, 1, 1, M).
