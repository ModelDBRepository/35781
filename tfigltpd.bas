
' K catalizza PP e PP2; V diminuito da PP2
' interstimulus interval according to poisson distribution. Bursts
' end after maxcount pulses. 100 loops. no seed.
DECLARE FUNCTION derivPP2! (KK!, PP2!, yt)
DECLARE FUNCTION derivPP1! (KK!, PP1!, yt)
DECLARE FUNCTION derivCC (yt, CC, PP1, PP2)
DECLARE FUNCTION derivKK (CC, KK)
DECLARE FUNCTION derivxt (zt, xt)
DECLARE FUNCTION derivyt (yt, xt)
DECLARE FUNCTION derivzt (yt, zt)
DIM SHARED eta, frac, taum, gamma, alp, i, bet, delta, delta2, lambda, lambda2, omega, omega2, A, M, A2, M2, trec, USE, tin
SCREEN 12
xmin = 0: xmax = 50: ymin = 0: ymax = 1
WINDOW (xmin, ymin)-(xmax, ymax)

A = 2: M = 3: delta = 400: lambda = 1: omega = 100
A2 = .5: M2 = 3: delta2 = 500: lambda2 = 2: omega2 = 80
alp = 2.5E-10
bet = 1E+08
gamma = 200
eta = 2
taum = .04
frac = .05
trec = .8
USE = .5
tin = .003

i = 0!
ltp2 = (M - SQR(M * M - 4 * lambda * lambda * A)) / (2 * lambda)
ltp3 = (M + SQR(M * M - 4 * lambda * lambda * A)) / (2 * lambda)
ltd2 = (M2 - SQR(M2 * M2 - 4 * lambda2 * lambda2 * A2)) / (2 * lambda2)
ltd3 = (M2 + SQR(M2 * M2 - 4 * lambda2 * lambda2 * A2)) / (2 * lambda2)
'LOCATE 3, 30: COLOR 3: PRINT "XXX "; : COLOR 4: PRINT "YYY "; : COLOR 2: PRINT "ZZZ": COLOR 15
'LOCATE 1, 30: PRINT "LTP "; ltp2, ltp3
'LOCATE 2, 30: PRINT "LTD "; ltd2, ltd3

h = .0001
count = 0
flag = 1
told = 0
tcount = 0
ion = 300
ioff = 0!
freq = 5
ont = .005 / h
offt = (1 / freq - .005) / h
spikes = 50000

csum = 0
cavr = .52
ccheck = 0
maxcount = 10000
tmax = 10 / h
tstart = 5 / h
tfinish = 10000 / h
OPEN "fno_pst.dat" FOR OUTPUT AS 1
'FOR loops = 1 TO 100
tpulse = 20000 / h
tend = 20000 / h
pcount = 0
sumt = 0
sumsq = 0
endflag = 0
maxpre = 0
maxpost = 0
count = 0
flag = 1
told = 0
tcount = 0
t = 0
i = 0!
xt = 1
yt = 0
zt = 0
CC = 0!
KK = 0
PP1 = 0!
PP2 = 0

cont:
IF tcount < tstart OR tcount > tpulse THEN icol = 2 ELSE icol = 4
'IF pcount > maxcount THEN LOCATE 3, 50: PRINT tcount
IF tcount = 0 OR tcount = tpulse THEN A$ = "1"
IF tcount = tstart OR pcount = maxcount OR tcount = tmax THEN
startend = 0
flag = -1 * flag
count = 0
i = ioff
IF pcount = maxcount OR tcount = tmax THEN
pcount = maxcount + 1
count = 0
tfinish = tcount
tend = tcount + 33 / h
tpulse = tcount + 30 / h
'LOCATE 3, 20: PRINT t; tcount; "tpulse"; tpulse
END IF
END IF
IF INKEY$ = "f" GOTO st
IF A$ = "1" THEN
A$ = "0"
count = 0
i = ion
END IF

IF flag = 1 AND i = ioff THEN GOTO skip

IF count >= ont AND i = ion THEN
i = ioff
count = 0
IF tcount >= tstart AND pcount <= maxcount THEN
'IF RND < q THEN time = ts ELSE time = tl
'offt = INT(-time * LOG(1 - RND) / h)
pcount = pcount + 1
sumt = sumt + offt * h
sumsq = sumsq + offt * offt * h * h
'PRINT pcount;
END IF
END IF
one:
IF count >= offt AND i = ioff THEN
i = ion
count = 0
END IF

skip:
k1 = h * derivxt(zt, xt)
k2 = h * derivxt(zt + .5 * k1, xt + .5 * k1)
xtn = xt + k2
k1 = h * derivyt(yt, xt)
k2 = h * derivyt(yt + .5 * k1, xt + .5 * k1)
ytn = yt + k2
'k1 = h * derivzt(yt, zt)
'k2 = h * derivzt(yt + .5 * k1, zt + .5 * k1)
'ztn = zt + k2
k1 = h * derivKK(CC, KK)
k2 = h * derivKK(CC + .5 * k1, KK + .5 * k1)
KKn = KK + k2
k1 = h * derivCC(yt, CC, PP1, PP2)
k2 = h * derivCC(yt + .5 * k1, CC + .5 * k1, PP1 + .5 * k1, PP2 + .5 * k1)
CCn = CC + k2
k1 = h * derivPP1(KK, PP1, yt)
k2 = h * derivPP1(KK + .5 * k1, PP1 + .5 * k1, yt + .5 * k1)
PP1n = PP1 + k2
k1 = h * derivPP2(KK, PP2, yt)
k2 = h * derivPP2(KK + .5 * k1, PP2 + .5 * k1, yt + .5 * k1)
PP2n = PP2 + k2
xt = xtn
yt = ytn
zt = 1 - xt - yt
KK = KKn
CC = CCn
PP1 = PP1n
PP2 = PP2n
PSET (t, CCn * 500), icol
PSET (t, KK * 50), 1
PSET (t, PP2n / 2), 6
PSET (t, PP1n / 2), 7

IF (tcount >= tstart OR tcount <= (tfinish + 15 / h)) THEN
ccheck = ccheck + CC
csum = (CC - cavr) ^ 2
END IF
t = t + h
tcount = tcount + 1

'IF t > tmax THEN endflag = 1
'GOTO sk
tim = tcount - told
IF tim >= .002 / h THEN
IF t < 19 OR t > 38 THEN PRINT #1, t; CC * 1000; KK * 60; PP1 / 2; PP2 / 2; i
LOCATE 1, 1: PRINT USING "###.###"; t
'PRINT #1, USING "##.####^^^^"; t; CC; KK; PP1; PP2; i
'IF tcount < tstart * .75 AND CC > maxpre THEN maxpre = CC
'IF tcount > tpulse AND CC > maxpost THEN maxpost = CC
told = tcount
END IF
sk:
count = count + 1
IF tcount < tend GOTO cont
st:
pcount = pcount - 1
i = 0

CLOSE 1
'NEXT loops
'WEND
'CLOSE 2
LOCATE 10, 30: PRINT "avr cc= "; ccheck / (tfinish + 15 / h - tstart); SQR(csum / (tfinish + 15 / h - tstart - 1))

END

FUNCTION derivCC (yt, CC, PP1, PP2)
derivCC = -CC / taum + bet * yt * alp * (1 / taum + frac * (delta * PP1 - delta2 * PP2))
END FUNCTION

FUNCTION derivKK (CC, KK)
derivKK = gamma * CC - eta * KK
END FUNCTION

FUNCTION derivPP1 (KK, PP1, yt)
derivPP1 = omega * KK - (lambda + yt * delta) * PP1 + M * PP1 ^ 2 / (A + PP1 ^ 2)
END FUNCTION

FUNCTION derivPP2 (KK, PP2, yt)
derivPP2 = omega2 * KK - (lambda2 + yt * delta2) * PP2 + M2 * PP2 ^ 2 / (A2 + PP2 ^ 2)
END FUNCTION

FUNCTION derivxt (zt, xt)
derivxt = zt / trec - USE * xt * i
END FUNCTION

FUNCTION derivyt (yt, xt)
derivyt = -yt / tin + USE * xt * i
END FUNCTION

FUNCTION derivzt (yt, zt)
derivzt = yt / tin - zt / trec
END FUNCTION



