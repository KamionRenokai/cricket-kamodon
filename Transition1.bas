SCREEN _NEWIMAGE(320, 240, 32)

LINE (0, 0)-(319, 239), _RGBA32(0, 255, 0, 255), BF

oldpic& = _COPYIMAGE(0)
_DISPLAY

FOR r = 0 TO 319
    FOR l = 0 TO 119
        LINE (0, l * 2)-(r, l * 2), _RGBA32(0, 0, 0, 255)
    NEXT l
    _DISPLAY
NEXT r

FOR r = 0 TO 319
    FOR l = 0 TO 119
        LINE (319 - r, l * 2 + 1)-(319, l * 2 + 1), _RGBA32(0, 0, 0, 255)
    NEXT l
    _DISPLAY
NEXT r
