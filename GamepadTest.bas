dev = _DEVICES
LOCATE 1, 1: PRINT _DEVICE$(3)
LOCATE 2, 1: PRINT _DEVICE$(4)
DO
    COLOR 7, 0
    LOCATE 3, 2: PRINT "  л  "
    LOCATE 4, 2: PRINT "ллл  "
    LOCATE 5, 2: PRINT "  л  "
    LOCATE 6, 2: PRINT "  л  "
    LOCATE 7, 2: PRINT "ллллл"
    LOCATE 5, 17: PRINT "STICK"
    LOCATE 3, 40: PRINT " ллл "
    LOCATE 4, 40: PRINT "л   л"
    LOCATE 5, 40: PRINT "  лл "
    LOCATE 6, 40: PRINT " л   "
    LOCATE 7, 40: PRINT "ллллл"
    LOCATE 5, 54: PRINT "STICK"
    COLOR 6, 0
    LOCATE 3, 19: PRINT "UP"
    LOCATE 5, 11: PRINT "LEFT"
    LOCATE 5, 24: PRINT "RIGHT"
    LOCATE 7, 18: PRINT "DOWN"
    LOCATE 3, 56: PRINT "UP"
    LOCATE 5, 48: PRINT "LEFT"
    LOCATE 5, 61: PRINT "RIGHT"
    LOCATE 7, 55: PRINT "DOWN"
    FOR b = 1 TO 10
        LOCATE (8 + b), 9: PRINT "BUTTON" + STR$(b)
        LOCATE (8 + b), 47: PRINT "BUTTON" + STR$(b)
        LOCATE (8 + b), 22: PRINT "BUTTON" + STR$(b + 10)
        LOCATE (8 + b), 60: PRINT "BUTTON" + STR$(b + 10)
    NEXT b
    xaxisA% = STICK(0, 1)
    yaxisA% = STICK(1, 1)
    xaxisB% = STICK(2, 1)
    yaxisB% = STICK(3, 1)
    LOCATE 5, 11: IF xaxisA% < 51 THEN COLOR 14, 0: PRINT "LEFT"
    LOCATE 5, 24: IF xaxisA% > 200 THEN COLOR 14, 0: PRINT "RIGHT"
    LOCATE 3, 19: IF yaxisA% < 51 THEN COLOR 14, 0: PRINT "UP"
    LOCATE 7, 18: IF yaxisA% > 200 THEN COLOR 14, 0: PRINT "DOWN"
    LOCATE 5, 48: IF xaxisB% < 51 THEN COLOR 14, 0: PRINT "LEFT"
    LOCATE 5, 61: IF xaxisB% > 200 THEN COLOR 14, 0: PRINT "RIGHT"
    LOCATE 3, 56: IF yaxisB% < 51 THEN COLOR 14, 0: PRINT "UP"
    LOCATE 7, 55: IF yaxisB% > 200 THEN COLOR 14, 0: PRINT "DOWN"
    FOR na = 0 TO 9
        btn% = STRIG((na * 4) + 1, 1)
        LOCATE na + 9, 9: IF btn% THEN COLOR 14, 0: PRINT "BUTTON" + STR$(na + 1)
    NEXT na
    FOR nb = 10 TO 19
        btn% = STRIG((nb * 4) + 1, 1)
        LOCATE nb - 1, 22: IF btn% THEN COLOR 14, 0: PRINT "BUTTON" + STR$(nb + 1)
    NEXT nb
    FOR nc = 0 TO 9
        btn% = STRIG((nc * 4) + 3, 2)
        LOCATE nc + 9, 47: IF btn% THEN COLOR 14, 0: PRINT "BUTTON" + STR$(nc + 1)
    NEXT nc
    FOR nd = 10 TO 19
        btn% = STRIG((nd * 4) + 3, 2)
        LOCATE nd - 1, 60: IF btn% THEN COLOR 14, 0: PRINT "BUTTON" + STR$(nd + 1)
    NEXT nd

    _DISPLAY
LOOP UNTIL _KEYHIT = 27
