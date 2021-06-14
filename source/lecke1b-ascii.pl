t(piros, zold). t(zold, piros).
t(piros, kek). t(zold, kek).
t(kek, piros). t(kek, zold).
t(piros, sarga). t(zold, sarga). t(kek, sarga).
t(sarga, piros). t(sarga, zold). t(sarga, kek).

terkep(DE, CH, IT, PL, CZ, AT, SI, HR, SK, HU) :-
    Tenger = kek,
    t(DE, PL), t(DE, CZ), t(DE, AT), t(DE, CH),
    t(CH, AT), t(CH, IT),
    t(IT, AT), t(IT, SI),
    t(PL, CZ), t(PL, SK),
    t(CZ, AT), t(CZ, SK),
    t(AT, SK), t(AT, HU), t(AT, SI),
    t(SI, HU), t(SI, HR),
    t(HR, HU),
    t(SK, HU),
    t(DE, Tenger), t(IT, Tenger), t(PL, Tenger),
    t(SI, Tenger), t(HR, Tenger).
