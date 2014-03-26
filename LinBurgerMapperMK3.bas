'Make about 40 different integers storing a number from 1 to 8. These will
'keep track of how much of each column of sprites (in eighths) are still on
'the screen. The middle ones will probably always be 8, but the ones on the
'left and right edges will be anywhere from 8 (fully on the screen) to 1
'(about to disappear). Then we'll see about smoothing it out, and maybe even
'try making it support vertical scrolling, as well?
SCREEN _NEWIMAGE(640, 480, 32)
_TITLE "BurgerMapper MapperBurger"
' TODO: Make a world and level selector, at least. Maybe even a world renamer.
mainfont& = _LOADFONT("/media/Lexar/PROJECTS/CRICKET/DejaVuSans.ttf", 12)
_FONT mainfont&

LOCATE 1, 1: PRINT "BurgerMapper -- Cricket Kamodon Map Editor"
LOCATE 1, 430: PRINT "Copyright 2014 Kamion Shanoriko"
LOCATE 3, 1: PRINT "BurgerMapper is getting ready..."
LOCATE 4, 1: PRINT "Found 0 sprites, which I will load, later."

tilecnt% = 0
respath$ = "/media/Lexar/PROJECTS/CRICKET/"
sprfldr$ = "SPRITES/"
chrfldr$ = "SPRITES/PLAYER/"
hudfldr$ = "SPRITES/INTERNAL/"
ext$ = ".png"
DO
    destpath$ = respath$ + sprfldr$ + LTRIM$(STR$(tilecnt%)) + ext$
    IF _FILEEXISTS(destpath$) THEN
        tilecnt% = tilecnt% + 1
        LOCATE 4, 1: PRINT "Found" + STR$(tilecnt%) + " sprites, which I will load, later."
    ELSE
        tilecnt% = tilecnt% - 1
        EXIT DO
    END IF
LOOP
LOCATE 4, 1: PRINT "Found " + LTRIM$(STR$(tilecnt% + 1)) + " sprites, which I will load, later."
DIM tile(0 TO 1280, 0 TO tilecnt%) AS LONG

'Base HUD background graphics (solid color and two-color transition tiles)
hudB& = _LOADIMAGE(respath$ + hudfldr$ + "Hudblue" + ext$)
hudG& = _LOADIMAGE(respath$ + hudfldr$ + "Hudgreen" + ext$)
hudLG& = _LOADIMAGE(respath$ + hudfldr$ + "Hudlightgreen" + ext$)
hudR& = _LOADIMAGE(respath$ + hudfldr$ + "Hudred" + ext$)
hudBG& = _LOADIMAGE(respath$ + hudfldr$ + "Hudbtog" + ext$)
hudBLG& = _LOADIMAGE(respath$ + hudfldr$ + "Hudbtol" + ext$)
hudLGB& = _LOADIMAGE(respath$ + hudfldr$ + "Hudltob" + ext$)
hudLGG& = _LOADIMAGE(respath$ + hudfldr$ + "Hudltog" + ext$)
hudRG& = _LOADIMAGE(respath$ + hudfldr$ + "Hudrtog" + ext$)

DIM SHARED FontTile(0 TO 37) AS LONG 'Numbers 0-9, letters A-Z, a star & dash
FOR n = 0 TO 9 'Numbers first
    destpath$ = respath$ + hudfldr$ + LTRIM$(STR$(n)) + ext$
    FontTile(n) = _LOADIMAGE(destpath$)
    IF FontTile(n) = 0 THEN PRINT "Can't load " + destpath$ + "!": END
NEXT n
FOR l = 65 TO 90 'Then letters
    FontTile((l - 55)) = _LOADIMAGE(respath$ + hudfldr$ + CHR$(l) + ext$)
    IF FontTile((l - 55)) = 0 THEN PRINT "Can't load " + respath$ + hudfldr$ + CHR$(l) + ext$ + "!": END
NEXT l
FontTile(36) = _LOADIMAGE(respath$ + hudfldr$ + "Star" + ext$)
'Don't forget to make a "hyphen" tile, and then load it here.

destpath$ = respath$ + "WLDXL1B1.NRT"
OPEN destpath$ FOR INPUT AS #1
PRINT "Reading BG1 layer... ";
DO UNTIL EOF(1)
    INPUT #1, num$
    backA& = backA& + 1
LOOP
bg1& = backA& / (22 * 8)
PRINT LTRIM$(STR$(bg1&)) + " sprites/row."
DIM Background1(0 TO ((22 * 8) - 1), 0 TO ((backA& / 22) - 1)) AS INTEGER
CLOSE #1

destpath$ = respath$ + "WLDXL1B2.NRT"
OPEN destpath$ FOR INPUT AS #2
PRINT "Reading BG2 layer... ";
DO UNTIL EOF(2)
    INPUT #2, num$
    backB& = backB& + 1
LOOP
bg2& = backB& / (22 * 8)
PRINT LTRIM$(STR$(bg2&)) + " sprites/row."
DIM Background2(0 TO ((22 * 8) - 1), 0 TO ((backB& / 22) - 1)) AS INTEGER
CLOSE #2

destpath$ = respath$ + "WLDXL1F1.NRT"
OPEN destpath$ FOR INPUT AS #3
PRINT "Reading FG1 layer... ";
DO UNTIL EOF(3)
    INPUT #3, num$
    frontA& = frontA& + 1
LOOP
fg1& = frontA& / (22 * 8)
PRINT LTRIM$(STR$(fg1&)) + " sprites/row."
DIM Foreground1(0 TO ((22 * 8) - 1), 0 TO ((frontA& / 22) - 1)) AS INTEGER
CLOSE #3

destpath$ = respath$ + "WLDXL1F2.NRT"
OPEN destpath$ FOR INPUT AS #4
PRINT "Reading FG2 layer... ";
DO UNTIL EOF(4)
    INPUT #4, num$
    frontB& = frontB& + 1
LOOP
fg2& = frontB& / (22 * 8)
PRINT LTRIM$(STR$(fg2&)) + " sprites/row."
DIM Foreground2(0 TO ((22 * 8) - 1), 0 TO ((frontB& / 22) - 1)) AS INTEGER
CLOSE #4

destpath$ = respath$ + "WLDXL1LD.KMD"
OPEN destpath$ FOR INPUT AS #5
INPUT #5, lvname$
INPUT #5, clock$
INPUT #5, scrnum$
INPUT #5, stpos$
tick = VAL(clock$)
scrcnt% = VAL(scrnum$)
vert% = VAL(stpos$)
'This one we'll leave open, since we have it at the spot where we want it at.
DIM LevelData(0 TO ((22 * 8) - 1), 0 TO ((frontB& / 22) - 1)) AS INTEGER

destpath$ = respath$ + "WLDXL1B1.NRT"
OPEN destpath$ FOR INPUT AS #1
destpath$ = respath$ + "WLDXL1B2.NRT"
OPEN destpath$ FOR INPUT AS #2
destpath$ = respath$ + "WLDXL1F1.NRT"
OPEN destpath$ FOR INPUT AS #3
destpath$ = respath$ + "WLDXL1F2.NRT"
OPEN destpath$ FOR INPUT AS #4

lc% = ((backA& / 22) / 8) / 256 'This stuff is... kinda complicated. xD
rc% = ((backA& / 22) / 8) MOD 256 'Loop once more if there's a remainder.
IF rc% >= 128 THEN lc% = lc% - 1 'Bugfix. Don't round up when it's 128 and up!
LOCATE 9, 1: PRINT "Divisor is" + STR$(lc%) + ", remainder is" + STR$(rc%) + "."

'Loop this entire routine as many times as there are screen levels on the map
FOR sl% = 0 TO 7

    IF lc% < 1 THEN 'If the total is less than 256... loop once.
        FOR r% = 0 TO 21
            FOR s% = 0 TO (((backA& / 22) / 8) - 1)
                LOCATE 10, 1: PRINT "Reading row" + STR$((r% + (22 * sl%)) + 1) + ", tile" + STR$(s% + 1) + " A"
                INPUT #1, ns$ 'Grab one of background layer one's sprites
                Background1((r% + (22 * sl%)), s%) = VAL(ns$) 'Convert & save
                ns$ = "" 'Erase what it's set to, just in case
                INPUT #2, ns$ 'Go for background layer two, now
                Background2((r% + (22 * sl%)), s%) = VAL(ns$) 'Convert & save
                ns$ = "" 'Let's erase it again, as quickly as we can
                INPUT #3, ns$ 'Go for foreground layer one, next
                Foreground1((r% + (22 * sl%)), s%) = VAL(ns$) 'Convert & save
                ns$ = "" 'One more quick erasing of this variable
                INPUT #4, ns$ 'Grab from foreground layer two, this time
                Foreground2((r% + (22 * sl%)), s%) = VAL(ns$) 'Convert & save
                ns$ = "" 'Okay, I lied... I gotta erase it, again!
                INPUT #5, ns$ 'And finally, grab some level data!
                LevelData((r% + (22 * sl%)), s%) = VAL(ns$) 'Convert and save
            NEXT s%
        NEXT r%

    ELSEIF lc% >= 1 THEN '          If the total is at least 256 or more,
        FOR l% = 0 TO (lc% - 1) '   loop as many times as the total number
            FOR r% = 0 TO 21 '      of tiles per row goes into 256.
                FOR s% = 0 TO 255
                    LOCATE 10, 1: PRINT "Reading row" + STR$((r% + (22 * sl%)) + 1) + ", tile" + STR$(256 * l% + (s% + 1)) + " B"
                    INPUT #1, ns$ 'Grab one from the first background layer
                    Background1((r% + (22 * sl%)), s% + (256 * l%)) = VAL(ns$)
                    ns$ = "" 'Erase what it's set to, just in case
                    INPUT #2, ns$ 'Go for the second background layer, now
                    Background2((r% + (22 * sl%)), s% + (256 * l%)) = VAL(ns$)
                    ns$ = "" 'Erase it again, Tony
                    INPUT #3, ns$ 'Make a mad dash for foreground layer one!
                    Foreground1((r% + (22 * sl%)), s% + (256 * l%)) = VAL(ns$)
                    ns$ = "" 'Blankety-blank-blank it out
                    INPUT #4, ns$ 'Take something from foreground layer two!
                    Foreground2((r% + (22 * sl%)), s% + (256 * l%)) = VAL(ns$)
                    ns$ = "" 'I sense a serious need for some blankage.
                    INPUT #5, ns$ 'And a-reach-in and-a-grab-a-level-data!
                    LevelData((r% + (22 * sl%)), s% + (256 * l%)) = VAL(ns$)
                NEXT s%
            NEXT r%
        NEXT l%

        'Now, if there was at least 256 or more, but still some left afterward,
        'let's run through one more loop, to get the remaining sprites.

        IF rc% > 0 THEN 'Any sprites left after reading each multiple of 256?
            FOR r% = 0 TO 21
                FOR s% = 0 TO ((((backA& / 22) / 8) - (256 * lc%)) - 1)
                    LOCATE 10, 1: PRINT "Reading row" + STR$((r% + (22 * sl%)) + 1) + ", tile" + STR$((256 * lc%) + s% + 1) + " BC"
                    INPUT #1, ns$ 'Grab a sprite
                    Background1((r% + (22 * sl%)), ((256 * lc%) + s%)) = VAL(ns$)
                    ns$ = "" 'Clear the variable, because I'm paranoid
                    INPUT #2, ns$ 'Grab another sprite
                    Background2((r% + (22 * sl%)), ((256 * lc%) + s%)) = VAL(ns$)
                    ns$ = "" 'Clear that thing! Get it far away from me! lol
                    INPUT #3, ns$ 'Grab yet another sprite
                    Foreground1((r% + (22 * sl%)), ((256 * lc%) + s%)) = VAL(ns$)
                    ns$ = "" 'Erase it! Erase it, I say!
                    INPUT #4, ns$ 'Grab one more sprite!
                    Foreground2((r% + (22 * sl%)), ((256 * lc%) + s%)) = VAL(ns$)
                    ns$ = "" 'It casts magic! Therefore, it must be erased!
                    INPUT #5, ns$ 'Enough with the sprites! Level data, now!
                    LevelData((r% + (22 * sl%)), ((256 * lc%) + s%)) = VAL(ns$)
                NEXT s%
            NEXT r%
        END IF

    ELSE PRINT "Double-check what LC% and RC% are set to!": END
    END IF

NEXT sl%

CLOSE 1, 2

FOR tc = 0 TO tilecnt%
    LOCATE 11, 1: PRINT "Loading sprite " + LTRIM$(STR$(tc)) + "... ";
    destpath$ = respath$ + sprfldr$ + LTRIM$(STR$(tc)) + ext$
    tx& = _LOADIMAGE(destpath$)
    _PUTIMAGE (0, 200), tx&
    IF _WIDTH(tx&) = 8 THEN
        FOR i = 0 TO 15
            GET (0, 200)-(7, 207), tile(i * 80, tc)
        NEXT i
    ELSEIF _WIDTH(tx&) = 158 THEN
        FOR i = 0 TO 15
            GET (i * 10, 200)-((i * 10) + 7, 207), tile(i * 80, tc)
        NEXT i
    END IF
    _FREEIMAGE tx&
    LINE (0, 200)-(170, 207), _RGBA32(0, 0, 0, 255), BF
NEXT tc

'Concept player sprite: Knuckles the Echidna, since everyone else uses Sonic.
destpath$ = respath$ + chrfldr$ + "KTE-Stand0.png"
stand& = _LOADIMAGE(destpath$)
destpath$ = respath$ + chrfldr$ + "KTE-Walk0.png"
walkA& = _LOADIMAGE(destpath$)
destpath$ = respath$ + chrfldr$ + "KTE-Jump0.png"
jump& = _LOADIMAGE(destpath$)
destpath$ = respath$ + chrfldr$ + "KTE-Climb0.png"
climb& = _LOADIMAGE(destpath$)
plyr& = stand&

PRINT "ALL SET!"

PRINT "Press ENTER to start the test."
DO
LOOP WHILE INKEY$ = ""

'Proof of concept: the HUD routine from alpha 8.
'To display all of the colors in each sprite, I found I have to use 32-bit
'color mode. The only downside to this, so far, is that I can't use the DRAW
'and LINE commands (among others) to draw the HUD. So, I'm gonna hafta make
'everything tile-based, including the HUD, if I decide to commit this.
CLS

'Pre drawing routine loop routines -- setting up variables and arrays
lives = 5
tick = 400
'tsc = (256 * lc%) + rc% 'Should be 511
'DIM sprnum(0 TO (tsc - 1)) AS INTEGER
'DIM sprpos(0 TO (tsc - 1)) AS INTEGER
DIM sprseg(0 TO 80) AS INTEGER 'How much of each column is on-screen (in 8ths)
DIM sprnum(0 TO 80) AS INTEGER 'Which sprites are on the screen
DIM sprpos(0 TO 80) AS INTEGER 'The X coordinate of each column of sprites
DIM verrow(0 TO 56) AS INTEGER 'The Y coordinate of each row of sprites
FOR n = 0 TO 80 '(tsc - 1)
    sprseg(n) = 8
    sprnum(n) = (n - 1)
    sprpos(n) = ((n - 1) * 8)
NEXT n
FOR n = 0 TO 56: verrow(n) = ((n - 1) * 8): NEXT n
'sprseg(0) = 8
'sprnum(0) = 0
'sprpos(0) = -8

CKL% = 0 '148 'Left-most X coordinate (player)
CKR% = CKL% + _WIDTH(plyr&) - 1 '171 'Right-most X coordinate (player)
CKT% = 0 '172 'Top-most Y coordinate (player)
CKB% = CKT% + _HEIGHT(plyr&) - 1 'Bottom-most Y coordinate (player)
CKX% = 0 'Global X coordinate, for absolute positioning on the map

startz = 0
startw = 131 'Formerly 154
w = startw 'Starting point is the lowest screen level, for right now.

DIM UnderFoot(0 TO 3) AS INTEGER
DIM AboveHead(0 TO 3) AS INTEGER
DIM InFrontOf(0 TO 4) AS INTEGER
DIM BehindMoi(0 TO 4) AS INTEGER

_TITLE "BurgerMapper: PHRINGDOTT 1"

DO
    'CLS
    'HUD drawing (tile placing) routine

    LINE (0, 0)-(639, 479), _RGB32(0, 0, 180), BF
    LINE (0, 34)-(639, 479), _RGB32(160, 160, 160), B
    COLOR _RGB32(255, 255, 255), _RGB32(0, 0, 180)
    ' TODO: Make this scroll horizontally, so I can display more help information.
    LOCATE 1, 1: PRINT "This is how the map will look, in-game. Use the arrow keys to free-scroll."
    DRAW "B M64, 14"
    DrawBox 4
    DRAW "B M129, 14"
    DrawBox 4
    DRAW "B M194, 14"
    DrawBox 4
    DRAW "B M415, 14"
    DrawBox 3
    DRAW "B M575, 14"
    DrawBox 4
    DRAW "B M8, 25 C" + STR$(&HFFFFFFFF)
    Font "OPTIONS"
    DRAW "B M75, 21 C" + STR$(&HFF777777)
    Font "CHANGE"
    DRAW "B M69, 28"
    Font "MAP SIZE"
    DRAW "B M139, 21"
    Font "CHANGE"
    DRAW "B M136, 28"
    Font "ELEMENT"
    DRAW "B M205, 21"
    Font "DELETE"
    DRAW "B M201, 28"
    Font "ELEMENT"
    DRAW "B M288, 25 C" + STR$(&HFFFFFFFF)
    Font "PHRINGDOTT 1"
    DRAW "B M426,21 C" + STR$(&HFFFFFF77)
    Font "CHANGE"
    DRAW "B M431,28"
    Font "LEVEL"
    DRAW "B M495,25 C" + STR$(&HFFFFFFFF)
    Font "VIEW MODE"
    DRAW "B M589,21 C" + STR$(&HFF777777)
    Font "WORLD"
    DRAW "B M597,28"
    Font "MAP"

    'Top row of the HUD

    'Tile rendering routine -- draw each layer onto the screen, in sequence
    FOR y = 0 TO 56 'Originally 28
        FOR x = 0 TO 80 'Originally 41 (80's good, and 82 works, too)
            'LOCATE 1, 1: PRINT "COLUMN: " + LTRIM$(STR$(w)) + "  ROW: " + LTRIM$(STR$(y)) + "  LEFTMOST: " + STR$(sprpos(0))
            IF sprnum(x) >= 0 AND verrow(y) < 360 THEN '352 is pretty good. It was originally "verrow(y) < 176"
                IF y > 0 AND (w - 1) > -1 THEN PUT (sprpos(x), 71 + verrow(y)), tile(0, Background1((w - 1) + y, sprnum(x))), _CLIP PSET
                IF y > 0 AND (w - 1) > -1 THEN PUT (sprpos(x), 71 + verrow(y)), tile(0, Background2((w - 1) + y, sprnum(x))), _CLIP PSET, _RGB32(1, 1, 1)
                IF y > 0 AND (w - 1) > -1 THEN PUT (sprpos(x), 71 + verrow(y)), tile(0, Foreground1((w - 1) + y, sprnum(x))), _CLIP PSET, _RGB32(1, 1, 1)
                IF y > 0 AND (w - 1) > -1 THEN PUT (sprpos(x), 71 + verrow(y)), tile(0, Foreground2((w - 1) + y, sprnum(x))), _CLIP PSET, _RGB32(1, 1, 1)
            END IF
            'z = z + 1
        NEXT x
        'z = startz
    NEXT y

    FOR clr = 0 TO 3
        UnderFoot(clr) = 0
        AboveHead(clr) = 0
        InFrontOf(clr) = 0
        BehindMoi(clr) = 0
    NEXT clr
    InFrontOf(4) = 0: BehindMoi(4) = 0
    PCol = 0: PRow = 0: topright = 0: topleft = 0

    'This is how we find out which sprite Cricket is next to/on top of/under.
    SELECT CASE (CKB% + 1) 'Start by finding the row that Cricket's above.
        'TODO: MAKE SOMETHING MORE EFFICIENT THAN THIS!!!
        CASE IS < 71
            PRow = 0
        CASE IS < 79
            PRow = 1
        CASE IS < 87
            PRow = 2
        CASE IS < 95
            PRow = 3
        CASE IS < 103
            PRow = 4
        CASE IS < 111
            PRow = 5
        CASE IS < 119
            PRow = 6
        CASE IS < 127
            PRow = 7
        CASE IS < 135
            PRow = 8
        CASE IS < 143
            PRow = 9
        CASE IS < 151
            PRow = 10
        CASE IS < 159
            PRow = 11
        CASE IS < 167
            PRow = 12
        CASE IS < 175
            PRow = 13
        CASE IS < 183
            PRow = 14
        CASE IS < 191
            PRow = 15
        CASE IS < 199
            PRow = 16
        CASE IS < 207
            PRow = 17
        CASE IS < 215
            PRow = 18
        CASE IS < 223
            PRow = 19
        CASE IS < 231
            PRow = 20
        CASE IS < 239
            PRow = 21
    END SELECT
    'And now, for a more efficient routine: what column are we on?
    FOR gx = 0 TO CKX%
        IF gx MOD 8 = 0 THEN PCol = PCol + 1
    NEXT gx
    sn = 0 'For safety purposes, and, well... just because. xD
    UnderFoot(0) = (PCol - 1)
    AboveHead(0) = (PCol - 1)
    'Now that we know exactly what sprite the bottom-leftmost pixel that makes
    'up Cricket is on, let's see if he's standing on three, or four tiles.
    FOR ldir = 0 TO 22 '23 other pixels, counting the one it starts on.
        IF (CKX% + (ldir + 1)) MOD 8 = 0 THEN 'If it divides evenly into 8...
            UnderFoot(sn + 1) = UnderFoot(sn) + 1 'Make the next tile in both
            AboveHead(sn + 1) = AboveHead(sn) + 1 'sets one more than before
            sn = sn + 1
        END IF
    NEXT ldir

    'And with a routine that's somewhat efficient, what's on each side?
    so = 0
    InFrontOf(0) = PRow - 4
    BehindMoi(0) = PRow - 4
    FOR side = 0 TO 30 '31 other pixels, counting the one it starts on.
        IF (CKT% + (side + 1)) MOD 8 = 0 THEN 'If it divides evenly into 8...
            InFrontOf(so + 1) = InFrontOf(so) + 1 'Make the next tile in both
            BehindMoi(so + 1) = BehindMoi(so) + 1 'sets one more than before
            so = so + 1
        END IF
    NEXT side
    topright = AboveHead(2) + 1
    topleft = AboveHead(0)

    'Now, we see what kinds of tiles the sprites under Cricket are. If any of
    'them match the types mentioned below, Cricket will stop falling.
    'IF oco THEN oco = 0
    'IF jump% = 0 THEN jdrop = 1
    'FOR Foot = 0 TO (sn - 1) 'The number of sprites below Cricket's feet
    '    IF LevelData(w + PRow, UnderFoot(Foot)) = 1 THEN 'Platform
    '        'IF jump% = -2 THEN oco = oco + 1 ELSE
    '        jdrop = 0: jump% = 0
    '    ELSEIF LevelData(w + PRow, UnderFoot(Foot)) = 2 THEN 'Wall
    '        'IF jump% = -2 THEN oco = oco + 1 ELSE
    '        jdrop = 0: jump% = 0
    '    END IF
    '    IF jump% = -2 AND LevelData(w + PRow, UnderFoot(Foot)) <> 3 THEN
    '        oco = oco + 1 'Also check to see if Cricket's climbing something
    '    END IF
    '    IF oco > 2 THEN jump% = 0: jdrop = 1 'If he isn't, make him fall
    'NEXT Foot

    'Quick check: did Cricket jump or fall off the screen?
    'IF CKT% >= 241 AND slidecount% = 0 AND (w + 22) / 22 = scrcnt% THEN 'Check his coordinates
    '    'IF (w + 22) / 22 = scrcnt% THEN 'Bottom screen? That's a death.
    '    jdrop = 0 'Stop him from falling
    '    jump% = -1 'Reset the height counter
    '    'IF _SNDPLAYING(bgm&) THEN _SNDSTOP bgm& 'Stop the background music
    '    '_SNDPLAY SEF(3) 'Play the death sound
    '    CKT% = 240: CKB% = (CKT% + _HEIGHT(plyr&) - 1) 'Move Cricket, so the sound only plays once
    '    IF lives > 0 THEN lives = lives - 1 'Take away a life
    '    'At some point, the game will jump to the "try again?" screen.
    '    'For now, all you have to do is press "R" to revive Cricket.
    'ELSEIF CKT% >= 236 AND slidecount% = 0 AND (w + 22) / 22 <> scrcnt% THEN
    '    '    IF NOT slidecount% THEN 'Start scrolling if it isn't, already.
    '    'w = w + 1
    '    'vert% = vert% + 1
    '    slidecount% = 176
    '    jdrop = 0
    '    CKT% = CKT% - 1: CKB% = (CKT% + _HEIGHT(plyr&) - 1)
    '    'END IF
    '    'END IF
    '    'TODO: Make the screen scroll down if Cricket hits the top!
    'END IF

    'Sprite positioning routine -- draw the player, and each enemy in sight
    'IF pf THEN
    '    _PUTIMAGE (CKR%, CKT%)-(CKL%, CKB%), plyr& 'Ol' fat hands facing left
    'ELSEIF NOT pf THEN
    '    _PUTIMAGE (CKL%, CKT%), plyr& 'Ol' fat hands facing right
    'END IF

    'Leftover code, for testing purposes (just in case)
    '    FOR y = 0 TO ((_HEIGHT - 72) / 8)
    '        FOR x = 0 TO (_WIDTH / 8)
    '            _PUTIMAGE (x * 8, 44 + (y * 8)), tile(Background1(w, z))
    '            _PUTIMAGE (x * 8, 44 + (y * 8)), tile(Background2(w, z))
    '            _PUTIMAGE (x * 8, 44 + (y * 8)), tile(Foreground1(w, z))
    '            _PUTIMAGE (x * 8, 44 + (y * 8)), tile(Foreground2(w, z))
    '            z = z + 1
    '        NEXT x
    '        z = startz
    '        w = w + 1
    '    NEXT y

    TileFont "U " + STR$(CKT%) + " L " + STR$(CKL%) + " JUMP " + LTRIM$(STR$(jdrop)) + " * " + STR$(jump%) + " TOP ROW " + STR$(verrow(0)), 0, 40
    TileFont "D " + STR$(CKB%) + " R " + STR$(CKR%) + " GLOBAL X POS " + STR$(CKX%), 0, 48
    TileFont "ABOVE" + STR$(sn) + " TILES  " + STR$(UnderFoot(0)) + " " + STR$(UnderFoot(1)) + " " + STR$(UnderFoot(2)) + " " + STR$(UnderFoot(3)), 0, 56
    TileFont "ON BOTH SIDES  " + STR$(InFrontOf(0)) + " " + STR$(InFrontOf(1)) + " " + STR$(InFrontOf(2)) + " " + STR$(InFrontOf(3)) + " " + STR$(InFrontOf(4)), 0, 64
    TileFont STR$(sprnum(0)) + " * " + STR$(sprpos(0)) + " AND " + STR$(sprnum(1)) + " * " + STR$(sprpos(1)), 0, 471
    _DISPLAY 'This kills out the flicker, which really helps with this.

    'Before we check for user key presses, do we need to scroll the screen?
    IF slidecount% < 0 THEN 'Are we scrolling the screen downward?
        'IF jdrop THEN jdrop = 0
        FOR v = 0 TO 56: verrow(v) = verrow(v) + 1: NEXT v
        slidecount% = slidecount% + 1
        IF verrow(0) >= 0 THEN
            FOR v = 0 TO 56: verrow(v) = verrow(v) - 8: NEXT v
            w = w - 1 'Should become vert% = vert% - 1
            'CKT% = CKT% + 6 'Move Cricket down six pixels each time, too.
            'CKB% = CKB% + 6
        END IF
    ELSEIF slidecount% > 0 THEN 'Or are we scrolling the screen upward?
        'IF jdrop THEN jdrop = 0
        FOR v = 0 TO 56: verrow(v) = verrow(v) - 1: NEXT v
        slidecount% = slidecount% - 1
        IF verrow(0) <= -16 THEN
            FOR v = 0 TO 56: verrow(v) = verrow(v) + 8: NEXT v
            w = w + 1 'Should become vert% = vert% + 1
            'CKT% = CKT% - 6 'Move Cricket up six pixels each time, too.
            'CKB% = CKB% - 6
        END IF
    END IF

    'Key trapping routine -- figure out what key was pressed, and act on it
    kp& = _KEYHIT 'Check for keys that don't need to be held down to work
    '********************
    ' 19200 - LEFT arrow key
    '********************
    IF _KEYDOWN(19200) THEN
        'IF _KEYDOWN(122) THEN 'Hold down Z
        '    FOR q = 0 TO (tsc - 1)
        '        sprpos(q) = sprpos(q) + 2
        '    NEXT q
        'ELSE
        'IF _KEYDOWN(120) AND jdrop = 0 THEN 'Holding down JUMP
        '    IF jump% = -2 THEN jump% = 0 'Jump off from a ladder/beanstalk.
        '    IF jump% = 0 THEN plyr& = jump&: CKB% = CKT% + _HEIGHT(plyr&) - 1: CKR% = CKL% + _WIDTH(plyr&) - 1
        '    IF _KEYDOWN(122) THEN 'The player jumps higher if it's running!
        '        IF jump% < 60 THEN
        '            notop = 0
        '            IF CKT% <= 32 THEN 'Bugfix. If nothing's above him, he just jumps.
        '                CKT% = CKT% - 1: CKB% = CKB% - 1: jump% = jump% + 1
        '            ELSEIF CKT% > 32 THEN
        '                FOR Head = 0 TO sn
        '                    IF LevelData(w + (PRow - 5), AboveHead(Head)) = 2 THEN
        '                        jdrop = 1 'Stop jumping if something's above his head
        '                        'Maybe add the sound of him hitting his head?
        '                    ELSE
        '                        notop = notop + 1 'One of the tiles isn't a wall
        '                    END IF
        '                NEXT Head 'Below: jump if the 3 or 4 tiles above aren't walls
        '                IF notop > 2 THEN CKT% = CKT% - 1: CKB% = CKB% - 1: jump% = jump% + 1
        '            END IF
        '        ELSE
        '            jump% = 0
        '            jdrop = 1
        '        END IF
        '    ELSE
        '        IF jump% < 42 THEN
        '            notop = 0
        '            IF CKT% <= 32 THEN 'Bugfix. If nothing's above him, he just jumps.
        '                CKT% = CKT% - 1: CKB% = CKB% - 1: jump% = jump% + 1
        '            ELSEIF CKT% > 32 THEN
        '                FOR Head = 0 TO sn
        '                    IF LevelData(w + (PRow - 5), AboveHead(Head)) = 2 THEN
        '                        jdrop = 1 'Stop jumping if something's above his head
        '                        'Maybe add the sound of him hitting his head?
        '                    ELSE
        '                        notop = notop + 1 'One of the tiles isn't a wall
        '                    END IF
        '                NEXT Head 'Below: jump if the 3 or 4 tiles above aren't walls
        '                IF notop > 2 THEN CKT% = CKT% - 1: CKB% = CKB% - 1: jump% = jump% + 1
        '            END IF
        '        ELSE
        '            jump% = 0
        '            jdrop = 1
        '        END IF
        '    END IF
        'END IF
        'IF kp& = -120 THEN jdrop = 1 'If you let go of the JUMP key early
        'IF jdrop THEN 'When you let go of JUMP
        '    IF plyr& <> stand& THEN plyr& = stand&: CKB% = CKT% + _HEIGHT(plyr&) - 1: CKR% = CKL% + _WIDTH(plyr&) - 1
        '    CKT% = CKT% + 1
        '    CKB% = CKB% + 1
        'END IF
        'Check for collisions, before we decide if we can move, or not
        'noblock = 0
        'FOR cr = 0 TO so 'Apparently this throws an error, on the top screen.
        '    IF LevelData(w + BehindMoi(cr), topleft) <> 2 THEN 'Wall
        '        noblock = noblock + 1
        '    END IF
        'NEXT cr
        'IF noblock > 3 AND CKX% > 0 THEN 'If nothing's in our way, let's move!
        pf = 1 'pf is TRUE, meaning the player is facing LEFT.
        IF sprnum(0) > -1 THEN 'Scroll the background (CKL% > 148)
            FOR q = 0 TO 80 '(tsc - 1)
                sprpos(q) = sprpos(q) + 1
            NEXT q
            CKX% = CKX% - 1
            'ELSEIF CKL% < 148 THEN
            'ELSEIF sprnum(0) = -1 AND sprpos(0) = -8 THEN 'Move the player
            '    CKL% = CKL% - 1: CKR% = CKR% - 1: CKX% = CKX% - 1
            'END IF
            'END IF
        END IF
        IF sprpos(0) >= 0 THEN 'Do we need to reset the column positions?
            FOR q = 0 TO 80
                sprpos(q) = sprpos(q) - 8 'Reset the column positions
                sprnum(q) = sprnum(q) - 1 'Change the displayed sprite
            NEXT q
        END IF
        '*********************
        ' 19712 - RIGHT arrow key
        '*********************
    ELSEIF _KEYDOWN(19712) THEN
        'IF _KEYDOWN(122) THEN 'Hold down Z
        '    FOR q = 0 TO (tsc - 1)
        '        sprpos(q) = sprpos(q) - 2
        '    NEXT q
        'ELSE
        'IF _KEYDOWN(120) AND jdrop = 0 THEN 'Holding down JUMP
        '    IF jump% = -2 THEN jump% = 0 'Jump off from a ladder/beanstalk.
        '    IF jump% = 0 THEN plyr& = jump&: CKB% = CKT% + _HEIGHT(plyr&) - 1: CKR% = CKL% + _WIDTH(plyr&) - 1
        '    IF _KEYDOWN(122) THEN 'The player jumps higher if it's running!
        '        'Maybe copy this routine to the UP and DOWN arrow routines?
        '        IF jump% < 60 THEN
        '            notop = 0
        '            IF CKT% <= 32 THEN 'Bugfix. If nothing's above him, he just jumps.
        '                CKT% = CKT% - 1: CKB% = CKB% - 1: jump% = jump% + 1
        '            ELSEIF CKT% > 32 THEN
        '                FOR Head = 0 TO sn
        '                    IF LevelData(w + (PRow - 5), AboveHead(Head)) = 2 THEN
        '                        jdrop = 1 'Stop jumping if something's above his head
        '                        'Maybe add the sound of him hitting his head?
        '                    ELSE
        '                        notop = notop + 1 'One of the tiles isn't a wall
        '                    END IF
        '                NEXT Head 'Below: jump if the 3 or 4 tiles above aren't walls
        '                IF notop > 2 THEN CKT% = CKT% - 1: CKB% = CKB% - 1: jump% = jump% + 1
        '            END IF
        '        ELSE
        '            jump% = 0
        '            jdrop = 1
        '        END IF
        '    ELSE
        '        IF jump% < 42 THEN
        '            notop = 0
        '            IF CKT% <= 32 THEN 'Bugfix. If nothing's above him, he just jumps.
        '                CKT% = CKT% - 1: CKB% = CKB% - 1: jump% = jump% + 1
        '            ELSEIF CKT% > 32 THEN
        '                FOR Head = 0 TO sn
        '                    IF LevelData(w + (PRow - 5), AboveHead(Head)) = 2 THEN
        '                        jdrop = 1 'Stop jumping if something's above his head
        '                        'Maybe add the sound of him hitting his head?
        '                    ELSE
        '                        notop = notop + 1 'One of the tiles isn't a wall
        '                    END IF
        '                NEXT Head 'Below: jump if the 3 or 4 tiles above aren't walls
        '                IF notop > 2 THEN CKT% = CKT% - 1: CKB% = CKB% - 1: jump% = jump% + 1
        '            END IF
        '        ELSE
        '            jump% = 0
        '            jdrop = 1
        '        END IF
        '    END IF
        'END IF
        'IF kp& = -120 THEN jdrop = 1 'If you let go of the JUMP key early
        'IF jdrop THEN 'When you let go of JUMP
        '    IF plyr& <> stand& THEN plyr& = stand&: CKB% = CKT% + _HEIGHT(plyr&) - 1: CKR% = CKL% + _WIDTH(plyr&) - 1
        '    CKT% = CKT% + 1
        '    CKB% = CKB% + 1
        'END IF
        'Check for collisions, before we decide if we can move, or not
        'noblock = 0
        'FOR cr = 0 TO so
        '    IF LevelData(w + InFrontOf(cr), topright) <> 2 THEN 'Wall
        '        noblock = noblock + 1
        '    END IF
        'NEXT cr
        'IF noblock > 3 THEN 'If nothing's in our way, where do we move?
        pf = 0 'pf is FALSE, meaning the player is facing RIGHT.
        IF sprnum(0) >= -1 THEN 'CKL = 148
            FOR q = 0 TO 80 '(tsc - 1)
                sprpos(q) = sprpos(q) - 1
            NEXT q
            CKX% = CKX% + 1
            'ELSEIF CKL% < 148 THEN
            '    CKL% = CKL% + 1: CKR% = CKR% + 1: CKX% = CKX% + 1
            'END IF
        END IF
        'END IF
        IF sprpos(0) <= -8 THEN 'Do we need to reset the column positions?
            FOR q = 0 TO 80
                sprpos(q) = sprpos(q) + 8
                sprnum(q) = sprnum(q) + 1
            NEXT q
        END IF
        '*********************
        ' 18432 - UP arrow key
        '*********************
    ELSEIF _KEYDOWN(18432) AND slidecount% = 0 THEN
        'climb = 0 'BUG: You freeze in midair if you jump, then hit UP or DOWN.
        'A temporary fix is to move the UP/DOWN arrow key routines so they're
        'run before the X (jump) key routine, but then you can't climb any-
        'thing unless you're on the ground.
        'FOR cc = 0 TO sn
        '    IF LevelData(w + (PRow - 1), AboveHead(cc)) = 3 THEN
        '        climb = climb + 1 'If Cricket can climb what's above him
        '    END IF
        'NEXT cc 'If he's in front of two climbable tiles, let's climb up!
        'IF climb = 2 THEN
        '    IF plyr& <> climb& THEN plyr& = climb&: CKR% = CKL% + (_WIDTH(plyr&) - 1): CKB% = CKT% + (_HEIGHT(plyr&) - 1)
        '    CKT% = CKT% - 1: CKB% = CKB% - 1: jump% = -2: jdrop = 0
        'END IF
        IF w > 0 THEN slidecount% = -1
        '******************
        ' 20480 - DOWN arrow key
        '******************
    ELSEIF _KEYDOWN(20480) AND slidecount% = 0 THEN
        'The check to see if, while the player is on a climbable object, a
        'wall or platform is right below them, should be fixed, and re-added.
        'climb = 0
        'FOR cc = 0 TO sn
        '    IF LevelData(w + (PRow - 1), AboveHead(cc)) = 3 THEN
        '        climb = climb + 1 'If Cricket can climb what's below him
        '        'Find a way to fix, and re-implement the code below.
        '        'ELSEIF LevelData(w + PRow, AboveHead(cc)) = 1 OR 2 THEN
        '        'climb = 0 'Stop him from sliding if he lands on something
        '    END IF
        'NEXT cc 'If he's in front of two climbable tiles, let's slide!
        'IF climb = 2 THEN
        '    IF plyr& <> climb& THEN plyr& = climb&: CKR% = CKL% + (_WIDTH(plyr&) - 1): CKB% = CKT% + (_HEIGHT(plyr&) - 1)
        '    CKT% = CKT% + 1: CKB% = CKB% + 1: jump% = -2: jdrop = 0
        'END IF
        slidecount% = 1

        'ELSEIF kp& = 114 THEN 'Pressing R resets Cricket's position (DEBUG key)
        '    CKT% = 24
        '    CKB% = CKT% + _HEIGHT(plyr&) - 1
        '    jdrop = 1
        '    jump% = 0
        'ELSEIF kp& = -120 THEN jdrop = 1 'If you let off the JUMP key early
        'ELSEIF jdrop THEN 'If you let off the JUMP key
        '    IF plyr& <> stand& THEN plyr& = stand&: CKB% = CKT% + _HEIGHT(plyr&) - 1: CKR% = CKL% + _WIDTH(plyr&) - 1
        '    CKT% = CKT% + 1 '   This brings the player sprite back down to the
        '    CKB% = CKB% + 1 '   platform it was on, three pixels at a time.
        'ELSEIF _KEYDOWN(120) AND jdrop = 0 AND slidecount% = 0 THEN 'JUMP key (X for right now)
        '    IF jump% = -2 THEN jump% = 0 'Jump off from a ladder/beanstalk.
        '    IF jump% = 0 THEN plyr& = jump&: CKB& = CKT% + _HEIGHT(plyr&) - 1: CKR% = CKL% + _WIDTH(plyr&) - 1
        '    IF jump% < 42 THEN 'Height test, instead of setting the coordinate.
        '        notop = 0
        '        IF CKT% <= 32 THEN 'Bugfix. If nothing's above him, he just jumps.
        '            CKT% = CKT% - 1: CKB% = CKB% - 1: jump% = jump% + 1
        '        ELSEIF CKT% > 32 THEN
        '            FOR Head = 0 TO sn
        '                IF LevelData(w + (PRow - 5), AboveHead(Head)) = 2 THEN
        '                    jdrop = 1 'Stop jumping if something's above his head
        '                    'Maybe add the sound of him hitting his head?
        '                ELSE
        '                    notop = notop + 1 'One of the tiles isn't a wall
        '                END IF
        '            NEXT Head 'Below: jump if the 3 or 4 tiles above aren't walls
        '            IF notop > 2 THEN CKT% = CKT% - 1: CKB% = CKB% - 1: jump% = jump% + 1
        '        END IF
        '    ELSE
        '        jump% = 0
        '        jdrop = 1
        '    END IF
        '
    ELSEIF kp& = 43 THEN 'The PLUS (+) key scrolls the screen down (DEBUG)
        IF w < startw THEN
            'w = w + 1
            slidecount% = 176
        END IF
    ELSEIF kp& = 45 THEN 'The MINUS (-) key scrolls the screen up (DEBUG)
        IF w > 0 THEN
            'w = w - 1
            slidecount% = -176
        END IF

    END IF
LOOP UNTIL _KEYDOWN(27) 'Press ESC to end the test.

'The FONT subcommand will have to be reworked, to use a tile-based font.
SUB TileFont (Word$, StartX%, StartY%)
LET LeftPos% = 1
LET FullPhrase$ = Word$
DO UNTIL FullPhrase$ = BuildPhrase$
    LET Dummy$ = LEFT$(Word$, LeftPos%)
    LET Letter$ = RIGHT$(Dummy$, 1)
    charnum = ASC(Letter$)
    IF charnum = 42 THEN cc = 36 'The asterisk becomes the star tile
    IF charnum > 47 AND charnum < 58 THEN cc = charnum - 48 '0-9
    IF charnum > 64 AND charnum < 91 THEN cc = charnum - 55 'A-Z
    IF charnum = 32 THEN ELSE _PUTIMAGE ((StartX% + ((LeftPos% - 1) * 8)), StartY%), FontTile(cc)
    LET BuildPhrase$ = (BuildPhrase$ + Letter$)
    LET LeftPos% = LeftPos% + 1
LOOP
END SUB

SUB Font (TheSentence$)
LET LeftPos% = 1
LET FullPhrase$ = TheSentence$
DO UNTIL FullPhrase$ = BuildPhrase$
    LET Dummy$ = LEFT$(TheSentence$, LeftPos%)
    LET Letter$ = RIGHT$(Dummy$, 1)
    GOSUB DrawLetter
    LET BuildPhrase$ = (BuildPhrase$ + Letter$)
    LET LeftPos% = LeftPos% + 1
LOOP
GOTO GetOuttaHere

DrawLetter:
SELECT CASE Letter$

    CASE "A"
        DRAW "U4"
        DRAW "B U1"
        DRAW "B R1"
        DRAW "R3"
        DRAW "B R1"
        DRAW "B D1"
        DRAW "D4"
        DRAW "B U2"
        DRAW "L5"
        DRAW "B R7"
        DRAW "B D2"

    CASE "B"
        DRAW "U5"
        DRAW "R4"
        DRAW "F1"
        DRAW "G1"
        DRAW "L4"
        DRAW "B R4"
        DRAW "F1"
        DRAW "D1"
        DRAW "G1"
        DRAW "L4"
        DRAW "B R7"

    CASE "C"
        DRAW "B R5"
        DRAW "L4"
        DRAW "H1"
        DRAW "U3"
        DRAW "E1"
        DRAW "R4"
        DRAW "B D5"
        DRAW "B R2"

    CASE "D"
        DRAW "U5"
        DRAW "R4"
        DRAW "F1"
        DRAW "D3"
        DRAW "G1"
        DRAW "L4"
        DRAW "B R7"

    CASE "E"
        DRAW "R5"
        DRAW "B L5"
        DRAW "U3"
        DRAW "R3"
        DRAW "B L3"
        DRAW "U2"
        DRAW "R5"
        DRAW "B R2"
        DRAW "B D5"

    CASE "F"
        DRAW "U5"
        DRAW "R5"
        DRAW "B L5"
        DRAW "B D2"
        DRAW "R3"
        DRAW "B R4"
        DRAW "B D3"

    CASE "G"
        DRAW "B R1"
        DRAW "R4"
        DRAW "U2"
        DRAW "L2"
        DRAW "B R2"
        DRAW "B D2"
        DRAW "B L4"
        DRAW "H1"
        DRAW "U3"
        DRAW "E1"
        DRAW "R4"
        DRAW "B R2"
        DRAW "B D5"

    CASE "H"
        DRAW "U5"
        DRAW "B D2"
        DRAW "R5"
        DRAW "B U2"
        DRAW "D5"
        DRAW "B R2"

    CASE "I"
        DRAW "B U5"
        DRAW "R4"
        DRAW "B L2"
        DRAW "D5"
        DRAW "B L2"
        DRAW "R4"
        DRAW "B R2"

    CASE "J"
        DRAW "B R1"
        DRAW "B U5"
        DRAW "R4"
        DRAW "B L2"
        DRAW "D4"
        DRAW "G1"
        DRAW "L1"
        DRAW "H1"
        DRAW "B D1"
        DRAW "B R7"

    CASE "K"
        DRAW "U5"
        DRAW "B D3"
        DRAW "E3"
        DRAW "B G3"
        DRAW "B U1"
        DRAW "F3"
        DRAW "B R2"

    CASE "L"
        DRAW "U5"
        DRAW "B D5"
        DRAW "R5"
        DRAW "B R2"

    CASE "M"
        DRAW "U5"
        DRAW "F3"
        DRAW "E3"
        DRAW "D5"
        DRAW "B R2"

    CASE "N"
        DRAW "U5"
        DRAW "F5"
        DRAW "U5"
        DRAW "B D5"
        DRAW "B R2"

    CASE "O"
        DRAW "B R1"
        DRAW "R3"
        DRAW "E1"
        DRAW "U3"
        DRAW "H1"
        DRAW "L3"
        DRAW "G1"
        DRAW "D3"
        DRAW "B D1"
        DRAW "B R7"

    CASE "P"
        DRAW "U5"
        DRAW "R4"
        DRAW "F1"
        DRAW "D1"
        DRAW "G1"
        DRAW "L4"
        DRAW "B R7"
        DRAW "B D2"

    CASE "Q"
        DRAW "B R1"
        DRAW "R3"
        DRAW "E1"
        DRAW "U3"
        DRAW "H1"
        DRAW "L3"
        DRAW "G1"
        DRAW "D3"
        DRAW "B R3"
        DRAW "B U1"
        DRAW "F1"
        DRAW "B D1"
        DRAW "B R3"

    CASE "R"
        DRAW "U5"
        DRAW "R4"
        DRAW "B R1"
        DRAW "B D1"
        DRAW "D1"
        DRAW "B D1"
        DRAW "B L1"
        DRAW "L4"
        DRAW "B R4"
        DRAW "F1"
        DRAW "D1"
        DRAW "B R2"

    CASE "S"
        DRAW "R4"
        DRAW "E1"
        DRAW "U1"
        DRAW "H1"
        DRAW "L3"
        DRAW "H1"
        DRAW "E1"
        DRAW "R4"
        DRAW "B R2"
        DRAW "B D5"

    CASE "T"
        DRAW "B U5"
        DRAW "R4"
        DRAW "B L2"
        DRAW "D5"
        DRAW "B R4"

    CASE "U"
        DRAW "B U1"
        DRAW "U4"
        DRAW "B D4"
        DRAW "F1"
        DRAW "R3"
        DRAW "E1"
        DRAW "U4"
        DRAW "B D5"
        DRAW "B R2"

    CASE "V"
        DRAW "B U5"
        DRAW "D3"
        DRAW "F2"
        DRAW "E2"
        DRAW "U3"
        DRAW "B D5"
        DRAW "B R2"

    CASE "W"
        DRAW "B U5"
        DRAW "D5"
        DRAW "E3"
        DRAW "F3"
        DRAW "U5"
        DRAW "B D5"
        DRAW "B R2"

    CASE "X"
        DRAW "E5"
        DRAW "B L5"
        DRAW "F5"
        DRAW "B R2"

    CASE "Y"
        DRAW "B R2"
        DRAW "U3"
        DRAW "H2"
        DRAW "B F2"
        DRAW "E2"
        DRAW "B R2"
        DRAW "B D5"

    CASE "Z"
        DRAW "B U5"
        DRAW "R5"
        DRAW "G5"
        DRAW "R5"
        DRAW "B R2"

    CASE "0"
        DRAW "B R1"
        DRAW "R3"
        DRAW "E1"
        DRAW "U3"
        DRAW "H1"
        DRAW "L3"
        DRAW "G1"
        DRAW "D3"
        DRAW "B R1"
        DRAW "E3"
        DRAW "B D4"
        DRAW "B R3"

    CASE "1"
        DRAW "R4"
        DRAW "B L2"
        DRAW "U5"
        DRAW "B D1"
        DRAW "L2"
        DRAW "B R2"
        DRAW "B D4"
        DRAW "B R4"

    CASE "2"
        DRAW "R5"
        DRAW "B L5"
        DRAW "E2"
        DRAW "R2"
        DRAW "E1"
        DRAW "U1"
        DRAW "H1"
        DRAW "L3"
        DRAW "G1"
        DRAW "B D4"
        DRAW "B R7"

    CASE "3"
        DRAW "B U1"
        DRAW "F1"
        DRAW "R3"
        DRAW "E1"
        DRAW "U1"
        DRAW "H1"
        DRAW "L2"
        DRAW "B R2"
        DRAW "E1"
        DRAW "H1"
        DRAW "L3"
        DRAW "G1"
        DRAW "B D4"
        DRAW "B R7"

    CASE "4"
        DRAW "B R3"
        DRAW "U5"
        DRAW "G3"
        DRAW "R5"
        DRAW "B R2"
        DRAW "B D2"

    CASE "5"
        DRAW "B U1"
        DRAW "F1"
        DRAW "R3"
        DRAW "E1"
        DRAW "U1"
        DRAW "H1"
        DRAW "L4"
        DRAW "U2"
        DRAW "R5"
        DRAW "B R2"
        DRAW "B D5"

    CASE "6"
        DRAW "B R1"
        DRAW "R3"
        DRAW "E1"
        DRAW "U1"
        DRAW "H1"
        DRAW "L3"
        DRAW "B L1"
        DRAW "B D1"
        DRAW "D1"
        DRAW "U3"
        DRAW "E1"
        DRAW "R4"
        DRAW "B R2"
        DRAW "B D5"

    CASE "7"
        DRAW "B R3"
        DRAW "U2"
        DRAW "E2"
        DRAW "U1"
        DRAW "L5"
        DRAW "B R7"
        DRAW "B D5"

    CASE "8"
        DRAW "B U1"
        DRAW "F1"
        DRAW "R3"
        DRAW "E1"
        DRAW "U1"
        DRAW "H1"
        DRAW "L3"
        DRAW "G1"
        DRAW "D1"
        DRAW "B U1"
        DRAW "B E1"
        DRAW "B H1"
        DRAW "E1"
        DRAW "R3"
        DRAW "F1"
        DRAW "B R2"
        DRAW "B D4"

    CASE "9"
        DRAW "B U1"
        DRAW "F1"
        DRAW "R3"
        DRAW "E1"
        DRAW "U3"
        DRAW "H1"
        DRAW "L3"
        DRAW "G1"
        DRAW "F1"
        DRAW "R3"
        DRAW "B R3"
        DRAW "B D3"

    CASE "'"
        DRAW "B U5"
        DRAW "D1"
        DRAW "B R2"
        DRAW "B D4"

    CASE ","
        DRAW "B U1"
        DRAW "D1"
        DRAW "B R2"

    CASE "."
        DRAW "B U1"
        DRAW "D1"
        DRAW "B U1"
        DRAW "B R1"
        DRAW "D1"
        DRAW "B R2"

    CASE "-"
        DRAW "B U2"
        DRAW "R4"
        DRAW "B U1"
        DRAW "L4"
        DRAW "B R6"
        DRAW "B D3"

    CASE "!"
        DRAW "B U5"
        DRAW "D3"
        DRAW "B D2"
        DRAW "R1"
        DRAW "B U2"
        DRAW "U3"
        DRAW "B D5"
        DRAW "B R2"

    CASE "?"
        DRAW "B U4"
        DRAW "R1"
        DRAW "B U1"
        DRAW "R3"
        DRAW "D1"
        DRAW "R1"
        DRAW "B D1"
        DRAW "L3"
        DRAW "B D1"
        DRAW "R1"
        DRAW "B D2"
        DRAW "L1"
        DRAW "B R5"

    CASE " "
        DRAW "B R5"

END SELECT
RETURN

GetOuttaHere:
END SUB

' TODO: Add a fifth type, which will be BGCOLOR 8, for grayed-out buttons.
SUB DrawBox (BoxType%)

SELECT CASE BoxType%

    CASE 1 'The box on the bottom. The most common one this game will use.
        LINE (0, 135)-(319, 199), , B
        LINE (1, 136)-(318, 198), , B
        LINE (2, 137)-(317, 197), , B
        LINE (3, 138)-(316, 196), _RGB32(0, 0, 0), B
        LINE (4, 139)-(315, 195), , B
        DRAW "B M5,140"
        DRAW "C" + STR$(&H00000000)
        FOR i% = 1 TO 27
            DRAW "R309"
            DRAW "B D1"
            DRAW "L309"
            DRAW "B D1"
        NEXT i%
        DRAW "R309"

    CASE 2 'Just like box 1, except this one's on top. It's the help box.
        LINE (0, 0)-(319, 59), , B
        LINE (1, 1)-(318, 58), , B
        LINE (2, 2)-(317, 57), , B
        LINE (3, 3)-(316, 56), _RGB32(0, 0, 0), B
        LINE (4, 4)-(315, 55), , B
        DRAW "B M5,5"
        DRAW "C" + STR$(&H00000000)
        FOR i% = 1 TO 25
            DRAW "R309"
            DRAW "B D1"
            DRAW "L309"
            DRAW "B D1"
        NEXT i%

    CASE 3 'One of the mini option boxes used by this game. This one's enabled.
        LINE STEP(0, 0)-STEP(61, 16), , B
        DRAW "B L61"
        DRAW "B U16"
        LINE STEP(1, 1)-STEP(59, 14), , B
        DRAW "B L58"
        DRAW "B U13"
        DRAW "C" + STR$(&HFFFF7777)
        FOR i% = 1 TO 6
            DRAW "R57"
            DRAW "B D1"
            DRAW "L57"
            DRAW "B D1"
        NEXT i%
        DRAW "R57"

    CASE 4 'Same as #3, except this one you can't select. It's disabled.
        LINE STEP(0, 0)-STEP(61, 16), , B
        DRAW "B L61"
        DRAW "B U16"
        LINE STEP(1, 1)-STEP(59, 14), , B
        DRAW "B L58"
        DRAW "B U13"
        DRAW "C" + STR$(&HFF222222)
        FOR i% = 1 TO 6
            DRAW "R57"
            DRAW "B D1"
            DRAW "L57"
            DRAW "B D1"
        NEXT i%
        DRAW "R57"

END SELECT
END SUB

SUB GameFont (Letter$)

SELECT CASE Letter$
    CASE "0" ' Number 0
        DRAW "B R1"
        DRAW "R2"
        DRAW "B R1"
        DRAW "B U1"
        DRAW "U6"
        DRAW "B L1"
        DRAW "B U1"
        DRAW "L2"
        DRAW "B L1"
        DRAW "B D1"
        DRAW "D6"
        DRAW "B R6"
        DRAW "B D1"
        EXIT SUB

    CASE "1" ' Number 1
        DRAW "B R3"
        DRAW "U8"
        DRAW "B D2"
        DRAW "L2"
        DRAW "B R2"
        DRAW "B D6"
        DRAW "B R5"
        EXIT SUB

    CASE "2" ' Number 2
        DRAW "R5"
        DRAW "B L5"
        DRAW "E5"
        DRAW "U2"
        DRAW "B U1"
        DRAW "B L1"
        DRAW "L3"
        DRAW "B L1"
        DRAW "B D1"
        DRAW "D1"
        DRAW "B D6"
        DRAW "B R7"
        EXIT SUB

    CASE "3" ' Number 3
        DRAW "B U1"
        DRAW "U1"
        DRAW "B D2"
        DRAW "B R1"
        DRAW "R3"
        DRAW "B R1"
        DRAW "B U1"
        DRAW "U2"
        DRAW "B U1"
        DRAW "B L1"
        DRAW "L2"
        DRAW "B R3"
        DRAW "B U1"
        DRAW "U2"
        DRAW "B U1"
        DRAW "B L1"
        DRAW "L3"
        DRAW "B L1"
        DRAW "B D1"
        DRAW "D1"
        DRAW "B R8"
        DRAW "B D6"
        EXIT SUB

    CASE "4" ' Number 4
        DRAW "B R3"
        DRAW "U8"
        DRAW "G4"
        DRAW "R5"
        DRAW "B D4"
        DRAW "B R3"
        EXIT SUB

    CASE "5" ' Number 5
        DRAW "B U1"
        DRAW "U1"
        DRAW "B D2"
        DRAW "B R1"
        DRAW "R3"
        DRAW "B U1"
        DRAW "B R1"
        DRAW "U4"
        DRAW "L5"
        DRAW "U3"
        DRAW "R5"
        DRAW "B R3"
        DRAW "B D8"
        EXIT SUB

    CASE "6" ' Number 6
        DRAW "B R1"
        DRAW "R3"
        DRAW "B R1"
        DRAW "B U1"
        DRAW "U2"
        DRAW "B U1"
        DRAW "B L1"
        DRAW "L3"
        DRAW "B L1"
        DRAW "B D1"
        DRAW "D2"
        DRAW "B U3"
        DRAW "E4"
        DRAW "B R3"
        DRAW "B D8"
        EXIT SUB

    CASE "7" ' Number 7
        DRAW "B U8"
        DRAW "R5"
        DRAW "D2"
        DRAW "G2"
        DRAW "D4"
        DRAW "B R4"
        EXIT SUB

    CASE "8" ' Number 8
        DRAW "B R1"
        DRAW "R3"
        DRAW "B R1"
        DRAW "B U1"
        DRAW "U2"
        DRAW "B U1"
        DRAW "B L1"
        DRAW "L3"
        DRAW "B U1"
        DRAW "B L1"
        DRAW "U2"
        DRAW "B U1"
        DRAW "B R1"
        DRAW "R3"
        DRAW "B R1"
        DRAW "B D1"
        DRAW "D2"
        DRAW "B D1"
        DRAW "B L1"
        DRAW "B L3"
        DRAW "B L1"
        DRAW "B D1"
        DRAW "D2"
        DRAW "B D1"
        DRAW "B R7"
        EXIT SUB

    CASE "9" ' Number 9
        DRAW "R2"
        DRAW "E3"
        DRAW "U4"
        DRAW "B U1"
        DRAW "B L1"
        DRAW "L3"
        DRAW "B L1"
        DRAW "B D1"
        DRAW "D3"
        DRAW "B D1"
        DRAW "B R1"
        DRAW "R3"
        DRAW "B R3"
        DRAW "B D3"
        EXIT SUB

    CASE "A" ' Uppercase A
        DRAW "U5"
        DRAW "E2"
        DRAW "F2"
        DRAW "D5"
        DRAW "B U2"
        DRAW "L4"
        DRAW "B R4"
        DRAW "B D2"
        DRAW "B R2"
        EXIT SUB

    CASE "B" ' Uppercase B
        DRAW "B U1"
        DRAW "U6"
        DRAW "R3"
        DRAW "B R1"
        DRAW "B D1"
        DRAW "D1"
        DRAW "B D1"
        DRAW "B L1"
        DRAW "L3"
        DRAW "B R3"
        DRAW "B R1"
        DRAW "D3"
        DRAW "B L1"
        DRAW "B D1"
        DRAW "L3"
        DRAW "U1"
        DRAW "B D2"
        DRAW "B R6"
        DRAW "B U1"
        EXIT SUB

    CASE "C" ' Uppercase C
        DRAW "B U1"
        DRAW "U5"
        DRAW "B U1"
        DRAW "B R1"
        DRAW "R3"
        DRAW "B D7"
        DRAW "L3"
        DRAW "B R5"
        EXIT SUB

    CASE "D" ' Uppercase D
        DRAW "U7"
        DRAW "R3"
        DRAW "B R1"
        DRAW "B D1"
        DRAW "D5"
        DRAW "B D1"
        DRAW "B L1"
        DRAW "L3"
        DRAW "B R6"
        EXIT SUB

    CASE "E" ' Uppercase E
        DRAW "U7"
        DRAW "R3"
        DRAW "B L3"
        DRAW "B D3"
        DRAW "R2"
        DRAW "B L2"
        DRAW "B D4"
        DRAW "R3"
        DRAW "B R2"
        EXIT SUB

    CASE "F" ' Uppercase F
        DRAW "U7"
        DRAW "R3"
        DRAW "B L3"
        DRAW "B D3"
        DRAW "R2"
        DRAW "B L2"
        DRAW "B D4"
        DRAW "B R6"
        EXIT SUB

    CASE "G" ' Uppercase G
        DRAW "B R1"
        DRAW "R4"
        DRAW "B L5"
        DRAW "B U1"
        DRAW "U5"
        DRAW "B U1"
        DRAW "B R1"
        DRAW "R4"
        DRAW "B D7"
        DRAW "B R1"
        DRAW "B U1"
        DRAW "U2"
        DRAW "B U1"
        DRAW "B L1"
        DRAW "L2"
        DRAW "B R5"
        DRAW "B D4"
        EXIT SUB

    CASE "H" ' Uppercase H
        DRAW "U7"
        DRAW "B D4"
        DRAW "R4"
        DRAW "B U4"
        DRAW "D7"
        DRAW "B R3"
        EXIT SUB

    CASE "I" ' Uppercase I
        DRAW "R4"
        DRAW "B L2"
        DRAW "U7"
        DRAW "B L2"
        DRAW "R4"
        DRAW "B R2"
        DRAW "B D7"
        EXIT SUB

    CASE "J" ' Uppercase J
        DRAW "B U1"
        DRAW "U2"
        DRAW "B D3"
        DRAW "B R1"
        DRAW "R3"
        DRAW "B R1"
        DRAW "B U1"
        DRAW "U6"
        DRAW "B D7"
        DRAW "B R3"
        EXIT SUB

    CASE "K" ' Uppercase K
        DRAW "U7"
        DRAW "B D4"
        DRAW "E4"
        DRAW "B G4"
        DRAW "B U1"
        DRAW "F4"
        DRAW "B R3"
        EXIT SUB

    CASE "L" ' Uppercase L
        DRAW "U7"
        DRAW "B D7"
        DRAW "R4"
        DRAW "B R2"
        EXIT SUB

    CASE "M" ' Uppercase M
        DRAW "U7"
        DRAW "F3"
        DRAW "E3"
        DRAW "D7"
        DRAW "B R2"
        EXIT SUB

    CASE "N" ' Uppercase N
        DRAW "U7"
        DRAW "F7"
        DRAW "U7"
        DRAW "B D7"
        DRAW "B R3"
        EXIT SUB

    CASE "O" ' Uppercase O
        DRAW "B R1"
        DRAW "R3"
        DRAW "B R1"
        DRAW "B U1"
        DRAW "U5"
        DRAW "B U1"
        DRAW "B L1"
        DRAW "L3"
        DRAW "B L1"
        DRAW "B D1"
        DRAW "D5"
        DRAW "B D1"
        DRAW "B R7"
        EXIT SUB

    CASE "P" ' Uppercase P
        DRAW "U7"
        DRAW "R4"
        DRAW "B R1"
        DRAW "B D1"
        DRAW "D2"
        DRAW "B D1"
        DRAW "B L1"
        DRAW "L4"
        DRAW "B D3"
        DRAW "B R7"
        EXIT SUB

    CASE "Q" ' Uppercase Q
        DRAW "B R1"
        DRAW "R3"
        DRAW "B R1"
        DRAW "B U1"
        DRAW "U5"
        DRAW "B U1"
        DRAW "B L1"
        DRAW "L3"
        DRAW "B L1"
        DRAW "B D1"
        DRAW "D5"
        DRAW "B D2"
        DRAW "B R6"
        DRAW "H3"
        DRAW "B R3"
        DRAW "B D2"
        DRAW "B R3"
        EXIT SUB

    CASE "R" ' Uppercase R
        DRAW "U7"
        DRAW "R3"
        DRAW "B R1"
        DRAW "B D1"
        DRAW "D2"
        DRAW "B D1"
        DRAW "B L1"
        DRAW "L3"
        DRAW "B R1"
        DRAW "F3"
        DRAW "B R2"
        EXIT SUB

    CASE "S" ' Uppercase S
        DRAW "B R1"
        DRAW "R3"
        DRAW "B R1"
        DRAW "B U1"
        DRAW "U2"
        DRAW "B U1"
        DRAW "B L1"
        DRAW "L3"
        DRAW "B U1"
        DRAW "B L1"
        DRAW "U1"
        DRAW "B U1"
        DRAW "B R1"
        DRAW "R3"
        DRAW "B D7"
        DRAW "B R3"
        EXIT SUB

    CASE "T" ' Uppercase T
        DRAW "B R2"
        DRAW "U7"
        DRAW "L3"
        DRAW "R6"
        DRAW "B R2"
        DRAW "B D7"
        EXIT SUB

    CASE "U" ' Uppercase U
        DRAW "B U1"
        DRAW "U6"
        DRAW "B D7"
        DRAW "B R1"
        DRAW "R4"
        DRAW "B R1"
        DRAW "B U1"
        DRAW "U6"
        DRAW "B D7"
        DRAW "B R2"
        EXIT SUB

    CASE "V" ' Uppercase V
        DRAW "B U3"
        DRAW "U4"
        DRAW "B D4"
        DRAW "F3"
        DRAW "E3"
        DRAW "U4"
        DRAW "B D7"
        DRAW "B R3"
        EXIT SUB

    CASE "W" ' Uppercase W
        DRAW "U7"
        DRAW "B D7"
        DRAW "E3"
        DRAW "F3"
        DRAW "U7"
        DRAW "B D7"
        DRAW "B R2"
        EXIT SUB

    CASE "X" ' Uppercase X
        DRAW "U2"
        DRAW "E3"
        DRAW "U2"
        DRAW "B D7"
        DRAW "U2"
        DRAW "H3"
        DRAW "U2"
        DRAW "B R5"
        DRAW "B D7"
        EXIT SUB

    CASE "Y" ' Uppercase Y
        DRAW "B R2"
        DRAW "U4"
        DRAW "H2"
        DRAW "U1"
        DRAW "B D1"
        DRAW "B F2"
        DRAW "E2"
        DRAW "U1"
        DRAW "B D7"
        DRAW "B R2"
        EXIT SUB

    CASE "Z" ' Uppercase Z
        DRAW "R5"
        DRAW "B L5"
        DRAW "U1"
        DRAW "E5"
        DRAW "U1"
        DRAW "L5"
        DRAW "B D7"
        DRAW "B R7"
        EXIT SUB

    CASE "a" ' Lowercase a
        DRAW "B R1"
        DRAW "R3"
        DRAW "U4"
        DRAW "B U1"
        DRAW "B L1"
        DRAW "L2"
        DRAW "B R3"
        DRAW "B D2"
        DRAW "L3"
        DRAW "B L1"
        DRAW "B D1"
        DRAW "D1"
        DRAW "B D1"
        DRAW "B R6"
        EXIT SUB

    CASE "b" ' Lowercase b
        DRAW "U7"
        DRAW "B D3"
        DRAW "R3"
        DRAW "B R1"
        DRAW "B D1"
        DRAW "D2"
        DRAW "B D1"
        DRAW "B L1"
        DRAW "L3"
        DRAW "B R6"
        EXIT SUB

    CASE "c" ' Lowercase c
        DRAW "B U1"
        DRAW "U3"
        DRAW "B U1"
        DRAW "B R1"
        DRAW "R2"
        DRAW "B L2"
        DRAW "B D1"
        DRAW "B D3"
        DRAW "B D1"
        DRAW "R2"
        DRAW "B R2"
        EXIT SUB

    CASE "d" ' Lowercase d
        DRAW "B R1"
        DRAW "R3"
        DRAW "U7"
        DRAW "B D3"
        DRAW "L3"
        DRAW "B L1"
        DRAW "B D1"
        DRAW "D2"
        DRAW "B D1"
        DRAW "B R6"
        EXIT SUB

    CASE "e" ' Lowercase e
        DRAW "B R1"
        DRAW "R3"
        DRAW "B L3"
        DRAW "B U1"
        DRAW "B L1"
        DRAW "U3"
        DRAW "B U1"
        DRAW "B R1"
        DRAW "R3"
        DRAW "B R1"
        DRAW "B D1"
        DRAW "D1"
        DRAW "B D1"
        DRAW "B L1"
        DRAW "L3"
        DRAW "B R6"
        DRAW "B D2"
        EXIT SUB

    CASE "f" ' Lowercase f
        DRAW "B R2"
        DRAW "U6"
        DRAW "B U1"
        DRAW "B R1"
        DRAW "R3"
        DRAW "B L4"
        DRAW "B D4"
        DRAW "L2"
        DRAW "R4"
        DRAW "B R4"
        DRAW "B D3"
        EXIT SUB

    CASE "g" ' Lowercase g
        DRAW "B U2"
        DRAW "B R1"
        DRAW "R3"
        DRAW "B R1"
        DRAW "B U1"
        DRAW "U1"
        DRAW "B U1"
        DRAW "B L1"
        DRAW "L3"
        DRAW "B L1"
        DRAW "B D1"
        DRAW "D1"
        DRAW "B D1"
        DRAW "B R5"
        DRAW "D2"
        DRAW "B D1"
        DRAW "B L1"
        DRAW "L3"
        DRAW "B D2"
        DRAW "B U3"
        DRAW "B R6"
        EXIT SUB

    CASE "h" ' Lowercase h
        DRAW "U7"
        DRAW "B D3"
        DRAW "R3"
        DRAW "B R1"
        DRAW "B D1"
        DRAW "D3"
        DRAW "B R2"
        EXIT SUB

    CASE "i" ' Lowercase i
        DRAW "U4"
        DRAW "B U2"
        DRAW "U1"
        DRAW "B D7"
        DRAW "B R3"
        EXIT SUB

    CASE "j" ' Lowercase j
        DRAW "B U1"
        DRAW "U1"
        DRAW "B D2"
        DRAW "B R1"
        DRAW "R2"
        DRAW "B R1"
        DRAW "B U1"
        DRAW "U3"
        DRAW "B U2"
        DRAW "U1"
        DRAW "B D7"
        DRAW "B R2"
        EXIT SUB

    CASE "k" ' Lowercase k
        DRAW "U7"
        DRAW "B D5"
        DRAW "E3"
        DRAW "B G3"
        DRAW "B U1"
        DRAW "F3"
        DRAW "B R2"
        EXIT SUB

    CASE "l" ' Lowercase l
        DRAW "U7"
        DRAW "B D7"
        DRAW "B R3"
        EXIT SUB

    CASE "m" ' Lowercase m
        DRAW "U4"
        DRAW "B U1"
        DRAW "B R1"
        DRAW "R1"
        DRAW "B D1"
        DRAW "B R1"
        DRAW "D4"
        DRAW "B R1"
        DRAW "B U5"
        DRAW "R1"
        DRAW "B R1"
        DRAW "B D1"
        DRAW "D4"
        DRAW "B R2"
        EXIT SUB

    CASE "n" ' Lowercase n
        DRAW "U5"
        DRAW "R3"
        DRAW "B R1"
        DRAW "B D1"
        DRAW "D4"
        DRAW "B R2"
        EXIT SUB

    CASE "o" ' Lowercase o
        DRAW "B R1"
        DRAW "R3"
        DRAW "B R1"
        DRAW "B U1"
        DRAW "U3"
        DRAW "B U1"
        DRAW "B L1"
        DRAW "L3"
        DRAW "B L1"
        DRAW "B D1"
        DRAW "D3"
        DRAW "B D1"
        DRAW "B R7"
        EXIT SUB

    CASE "p" ' Lowercase p
        DRAW "U5"
        DRAW "R3"
        DRAW "B R1"
        DRAW "B D1"
        DRAW "D1"
        DRAW "B D1"
        DRAW "B L1"
        DRAW "L3"
        DRAW "B D2"
        DRAW "B R6"
        EXIT SUB

    CASE "q" ' Lowercase q
        DRAW "B R4"
        DRAW "U5"
        DRAW "L3"
        DRAW "B L1"
        DRAW "B D1"
        DRAW "D1"
        DRAW "B D1"
        DRAW "B R1"
        DRAW "R3"
        DRAW "B D2"
        DRAW "B R3"
        EXIT SUB

    CASE "r" ' Lowercase r
        DRAW "U5"
        DRAW "R2"
        DRAW "B D5"
        DRAW "B R2"
        EXIT SUB

    CASE "s" ' Lowercase s
        DRAW "R3"
        DRAW "B U1"
        DRAW "B R1"
        DRAW "U1"
        DRAW "B U1"
        DRAW "B L1"
        DRAW "L3"
        DRAW "U1"
        DRAW "B U1"
        DRAW "B R1"
        DRAW "R3"
        DRAW "B R3"
        DRAW "B D5"
        EXIT SUB

    CASE "t" ' Lowercase t
        DRAW "B R3"
        DRAW "U7"
        DRAW "B D2"
        DRAW "L3"
        DRAW "R6"
        DRAW "B D5"
        DRAW "B R2"
        EXIT SUB

    CASE "u" ' Lowercase u
        DRAW "B U1"
        DRAW "U4"
        DRAW "B D5"
        DRAW "B R1"
        DRAW "R2"
        DRAW "B R1"
        DRAW "B U1"
        DRAW "U4"
        DRAW "B D5"
        DRAW "B R2"
        EXIT SUB

    CASE "v" ' Lowercase v
        DRAW "B R3"
        DRAW "H3"
        DRAW "U2"
        DRAW "B D2"
        DRAW "B F3"
        DRAW "E3"
        DRAW "U2"
        DRAW "B D5"
        DRAW "B R2"
        EXIT SUB

    CASE "w" ' Lowercase w
        DRAW "U5"
        DRAW "B D5"
        DRAW "E3"
        DRAW "F3"
        DRAW "U5"
        DRAW "B D5"
        DRAW "B R2"
        EXIT SUB

    CASE "x" ' Lowercase x
        DRAW "E5"
        DRAW "B D5"
        DRAW "H5"
        DRAW "B D5"
        DRAW "B R7"
        EXIT SUB

    CASE "y" ' Lowercase y
        DRAW "E5"
        DRAW "B G3"
        DRAW "H3"
        DRAW "B F5"
        DRAW "B R3"
        EXIT SUB

    CASE "z" ' Lowercase z
        DRAW "R5"
        DRAW "B L5"
        DRAW "E5"
        DRAW "L5"
        DRAW "B D5"
        DRAW "B R8"
        EXIT SUB

    CASE " " ' Space
        DRAW "B R4"
        EXIT SUB

    CASE ELSE
        GameFontAscii Letter$
        EXIT SUB

END SELECT
END SUB

SUB GameFontAscii (Symbol$)
SELECT CASE Symbol$

    CASE "(" ' Left Parenthese "("
        DRAW "B R1"
        DRAW "H2"
        DRAW "U4"
        DRAW "E2"
        DRAW "B D8"
        DRAW "B R3"
        EXIT SUB

    CASE ")" ' Right Parenthese ")"
        DRAW "B R1"
        DRAW "E2"
        DRAW "U4"
        DRAW "H2"
        DRAW "B D8"
        DRAW "B R3"
        EXIT SUB

    CASE "?" ' Question Mark "?"
        DRAW "B R2"
        DRAW "U1"
        DRAW "B U2"
        DRAW "U1"
        DRAW "E1"
        DRAW "U1"
        DRAW "B U1"
        DRAW "B L1"
        DRAW "L1"
        DRAW "B L1"
        DRAW "B D1"
        DRAW "D1"
        DRAW "B D5"
        DRAW "B R5"
        EXIT SUB

    CASE "." ' Period "."
        DRAW "R1"
        DRAW "B R2"
        EXIT SUB

    CASE "/" ' Forward Slash "/"
        FOR i% = 1 TO 2
            DRAW "U1"
            DRAW "B U1"
            DRAW "B R1"
            DRAW "U1"
        NEXT i%
        DRAW "U1"
        DRAW "B R2"
        DRAW "B D7"
        EXIT SUB

    CASE "\" ' Backward Slash "\"
        DRAW "B R2"
        FOR i% = 1 TO 2
            DRAW "U1"
            DRAW "B U1"
            DRAW "B L1"
            DRAW "U1"
        NEXT i%
        DRAW "U1"
        DRAW "B R5"
        DRAW "B D7"
        EXIT SUB

    CASE ":" ' Colon ":"
        DRAW "B R2"
        DRAW "B U2"
        DRAW "U1"
        DRAW "B U2"
        DRAW "U1"
        DRAW "B R3"
        DRAW "B D6"
        EXIT SUB

    CASE "*" ' Star or asterisk "*"
        DRAW "B U2"
        DRAW "E4"
        DRAW "B L2"
        DRAW "D4"
        DRAW "B R2"
        DRAW "H4"
        DRAW "B D6"
        DRAW "B R8"
        EXIT SUB

    CASE "," ' Comma ","
        DRAW "R1"
        DRAW "B R1"
        DRAW "B U1"
        DRAW "U2"
        DRAW "B D3"
        DRAW "B R2"
        EXIT SUB

    CASE "-" ' Hyphen or dash "-"
        DRAW "B U4"
        DRAW "R4"
        DRAW "B D4"
        DRAW "B R2"
        EXIT SUB

    CASE "'" ' Apostrophe "'"
        DRAW "B U4"
        DRAW "U3"
        DRAW "B D7"
        DRAW "B R2"
        EXIT SUB

    CASE "!" ' Exclamation point "!"
        DRAW "U1"
        DRAW "B U2"
        DRAW "U4"
        DRAW "B D7"
        DRAW "B R3"
        EXIT SUB

    CASE ELSE
        EXIT SUB

END SELECT
END SUB

SUB Sentence (Text$)
LET TextData$ = Text$ 'Get the variable so I can use it
IF TextData$ = "" THEN EXIT SUB 'If there's nothing there, EXIT SUB.
LET nl% = 1 'Set number of spaces to 1.
DO 'Begin loop
    LET r$ = LEFT$(TextData$, nl%) 'Get text from the LEFT side
    LET l$ = RIGHT$(r$, 1) 'Read one letter from the RIGHT side
    GameFont l$ 'Draw the corresponding letter/number
    LET nl% = nl% + 1 'Increase the number of spaces
    LET EndText$ = (EndText$ + l$) 'Try to spell the text in TextData$.
LOOP UNTIL EndText$ = TextData$ 'If EndText$ matches TextData$, STOP!
END SUB

