SCREEN _NEWIMAGE(320, 240, 32)
LINE (0, 0)-(319, 239), _RGBA32(0, 255, 0, 255), BF
_DISPLAY

DO
LOOP WHILE INKEY$ = ""

FOR a = 0 TO 255
    _LIMIT 60
    LINE (0, 0)-(319, 239), _RGBA32(0, 255, 0, 255), BF
    LINE (0, 0)-(319, 239), _RGBA32(0, 0, 0, a), BF
    _DISPLAY
NEXT a

LOCATE 1, 1: PRINT "*"
_DISPLAY
DO
LOOP WHILE INKEY$ = ""

FOR b = 255 TO 0 STEP -1
    _LIMIT 60
    LINE (0, 0)-(319, 239), _RGBA32(0, 255, 0, 255), BF
    LINE (0, 0)-(319, 239), _RGBA32(0, 0, 0, b), BF
    _DISPLAY
NEXT b
