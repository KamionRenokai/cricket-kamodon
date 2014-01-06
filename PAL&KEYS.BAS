'Extra game subcommands for Cricket Kamodon

'From "Multikey Function Update" by Joe Huber, Jr. (with parts by Eric Carr)
DECLARE SUB KEYTEST (LOWERLIMIT, UPPERLIMIT)
DECLARE FUNCTION MULTIKEY (KEYNUM)

SUB KEYTEST (LOWERLIMIT, UPPERLIMIT)
DO
x = 1
y = 1

 FOR i = LOWERLIMIT TO UPPERLIMIT
  TEST = MULTIKEY(i)
  LOCATE y, x
  PRINT TEST; i

  IF y < 23 THEN
   y = y + 1
  ELSE
   y = 1
   x = x + 7
  END IF
 NEXT i

LOOP WHILE MULTIKEY(1) = 0
END

END SUB

FUNCTION MULTIKEY (KEYNUM)
 STATIC FIRSTIME, KEYS(), SC(), DU()

 IF FIRSTIME = 0 THEN
  DIM KEYS(255), SC(255), DU(255)
  FOR E = 0 TO 127              '\
  SC(E) = E: DU(E) = 1          '|
  NEXT                          '|-ERIC CARR'S CODE--------------------\
  FOR E = 128 TO 255            '|                                     |
  SC(E) = E - 128: DU(E) = 0    '|                                     |
  NEXT                          '/                                     |
  FIRSTIME = -1                 '                                      |
 END IF                         '                                      |
                                '                                      |
 i$ = INKEY$       ' So the keyb buffer don't get full     \routine/ \ |
 i = INP(&H60)     ' Get keyboard scan code from port 60h   \lines/  |-/
 OUT &H61, INP(&H61) OR &H82: OUT &H20, &H20       '         \!!!/   |
 KEYS(SC(i)) = DU(i) ' This says what keys are pressed        \!/    /

MULTIKEY = KEYS(KEYNUM)

END FUNCTION

