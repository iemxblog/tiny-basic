LET G = 15
PRINT "Guess"
10 INPUT A
IF A = G THEN GOTO 20
IF A > G THEN PRINT "Lower"
IF A < G THEN PRINT "Higher"
GOTO 10
20 PRINT "Won"
