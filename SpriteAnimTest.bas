SCREEN _NEWIMAGE(320, 240, 32)

DIM spr(0 TO 1280, 0 TO 1) AS LONG
_DISPLAY
gt3& = _LOADIMAGE("/media/PHANTOM/QBX/CRICKET/SPRITES/84anim.png")
gt2& = _LOADIMAGE("/media/PHANTOM/QBX/CRICKET/SPRITES/241anim.png")
gt& = _LOADIMAGE("/media/PHANTOM/QBX/CRICKET/SPRITES/5anim.png")

_PUTIMAGE (0, 0), gt&
IF _WIDTH(gt&) = 8 THEN
    FOR i = 0 TO 15
        GET (0, 0)-(7, 7), spr(i * 80, 0)
    NEXT i
ELSEIF _WIDTH(gt&) = 158 THEN
    FOR i = 0 TO 15
        GET (i * 10, 0)-((i * 10) + 7, 7), spr(i * 80, 0)
    NEXT i
END IF

CLS
_PUTIMAGE (0, 0), gt2&

FOR i = 0 TO 15
    GET (i * 10, 0)-((i * 10) + 7, 7), spr(i * 80, 1)
NEXT i

CLS
DO
    _LIMIT 20
    PUT (0, 0), spr(x * 80, 0), PSET
    x = x + 1
    IF x = 16 THEN x = 0
    _DISPLAY
LOOP WHILE INKEY$ = ""

IF y = 0 AND (vert% - 1) = -1 THEN ELSE PUT (sprpos(x), 24 + verrow(y)), spritedata(Background1((vert% - 1) + y, sprnum(x)), 0), _CLIP PSET
