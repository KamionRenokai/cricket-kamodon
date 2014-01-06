' THE TAME - Tile And Map Editor for the Cricket Kamodon Game Engine Tech Demo
' Copyright 2012-2013 Robbie Bruce (Kamion R. Shanoriko)
'
' I should probably give this the ability to load map and sprite data files
' no matter what they're named, so if someone decides to mod Cricket, and
' change the global number of worlds to something other than 11, it'll be able
' to edit them all without a problem.

SCREEN _NEWIMAGE(640, 480, 256)
_TITLE "The TAME"
PAINT (0, 0), 1
COLOR 14, 1
' Let's have a little fun, shall we?
RANDOMIZE TIMER
da = INT(RND * 3)
IF da = 0 THEN
    DRAW "C14 B M273,14": Sentence "Creeeeeeeepy..."
ELSEIF da = 1 THEN
    DRAW "C14 B M196,14": Sentence "Looks like a horror movie title, doesn't it?"
ELSEIF da = 2 THEN
    DRAW "C14 B M221,14": Sentence "A little humor isn't gonna KILL ya!"
END IF
LOCATE 3, 23: PRINT "€€€€€€€€€€   €€     €€   €€€€€€€€€€"
LOCATE 4, 23: PRINT "    ≤≤       ≤≤     ≤≤   ≤≤        "
LOCATE 5, 23: PRINT "    ≤≤       ≤≤     ≤≤   ≤≤        "
LOCATE 6, 23: PRINT "    ≤≤       ≤≤     ≤≤   ≤≤        "
LOCATE 7, 23: PRINT "    ±±       ±±±±±±±±±   ±±±±±±±   "
LOCATE 8, 23: PRINT "    ±±       ±±     ±±   ±±        "
LOCATE 9, 23: PRINT "    ±±       ±±     ±±   ±±        "
LOCATE 10, 23: PRINT "    ∞∞       ∞∞     ∞∞   ∞∞        "
LOCATE 11, 23: PRINT "    ∞∞       ∞∞     ∞∞   ∞∞        "
LOCATE 12, 23: PRINT "    ∞∞       ∞∞     ∞∞   ∞∞∞∞∞∞∞∞∞∞"
LOCATE 14, 7: PRINT " €€€€€€€€€€         €€€€€€         €€              €€   €€€€€€€€€€"
LOCATE 15, 7: PRINT "     ≤≤           ≤≤      ≤≤       ≤≤≤≤          ≤≤≤≤   ≤≤        "
LOCATE 16, 7: PRINT "     ≤≤           ≤≤      ≤≤       ≤≤  ≤≤      ≤≤  ≤≤   ≤≤        "
LOCATE 17, 7: PRINT "     ≤≤           ≤≤      ≤≤       ≤≤    ≤≤  ≤≤    ≤≤   ≤≤        "
LOCATE 18, 7: PRINT "     ±±         ±±          ±±     ±±      ±±      ±±   ±±±±±±±   "
LOCATE 19, 7: PRINT "     ±±         ±±±±±±±±±±±±±±     ±±              ±±   ±±        "
LOCATE 20, 7: PRINT "     ±±         ±±          ±±     ±±              ±±   ±±        "
LOCATE 21, 7: PRINT "     ∞∞       ∞∞              ∞∞   ∞∞              ∞∞   ∞∞        "
LOCATE 22, 7: PRINT "     ∞∞       ∞∞              ∞∞   ∞∞              ∞∞   ∞∞        "
LOCATE 23, 7: PRINT "     ∞∞       ∞∞              ∞∞   ∞∞              ∞∞   ∞∞∞∞∞∞∞∞∞∞"
DRAW "B M254,390"
DRAW "C15": Font "T": DRAW "C14": Font "ILE ": DRAW "C15": Font "A"
DRAW "C14": Font "ND ": DRAW "C15": Font "M": DRAW "C14": Font "AP "
DRAW "C15": Font "E": DRAW "C14": Font "DITOR"
DRAW "B M219,420"
Sentence "Copyright 2012 Kamion R. Shanoriko"
DRAW "B M185,430"
Sentence "for the Cricket Kamodon Game Engine Tech Demo"
DRAW "B M161,450"
Sentence "...even though you're welcome to tweak it for any game."

LET TimeIsNow! = TIMER
DO WHILE WhatIsTime! < 5! AND keycheck& <> 13
    LET ClockCheck! = TIMER
    LET WhatIsTime! = ClockCheck! - TimeIsNow!
    keycheck& = _KEYHIT
    IF keycheck& = 13 THEN EXIT DO
LOOP

' TODO: Make a world and level selector, at least. Maybe even a world renamer.
IF _FILEEXISTS("/media/PHANTOM/QBX/CRICKET/WLDXL1S0.KMD") THEN
    OPEN "/media/PHANTOM/QBX/CRICKET/WLDXL1S0.KMD" FOR INPUT AS #1
    ' This should loop until it either reaches the end, or reads blank data.
    CLS
    loopz = 0
    DIM SHARED testpixel(0 TO 63) AS STRING * 3
    DO UNTIL EOF(1)
        ' First, we'll find out how many sprites are in this file (if we can).
        FOR n = 0 TO 63
            INPUT #1, testpixel(n)
            '   PRINT testpixel(n)
            '    IF testpixel(n) = "" THEN                   ' I'm gonna change these
            '      IF loopz = 0 THEN GOTO FileMistake1       ' lines around, before I un-
            '      ELSE EXIT DO                              ' comment them.
            '    END IF
        NEXT n
        INPUT #1, tos$ ' Should just be the end-of-line marker (END).
        IF tos$ <> "END" GOTO FileMistake2 ' We'll make sure.
        loopz = loopz + 1
    LOOP
    CLOSE 1
    mainfont& = _LOADFONT("/media/PHANTOM/QBX/CRICKET/DejaVuSans.ttf", 12)
    _FONT mainfont&
    PRINT "There are" + STR$(loopz) + " sprites in WLDXL1S0.KMD."
    SLEEP
    PRINT "Sprites are being loaded..."
ELSE 'TODO: Throw an error if the file doesn't exist.
END IF

OPEN "/media/PHANTOM/QBX/CRICKET/WLDXL1S0.KMD" FOR INPUT AS #1
' We open this a third time, so that we can get the sprite data the RIGHT way.
DIM SHARED pixeldata(0 TO (loopz - 1), 0 TO 15, 0 TO 63) AS INTEGER
DO UNTIL EOF(1)
    FOR s = 0 TO (loopz - 1)
        FOR p = 0 TO 63
            INPUT #1, flip$
            pixeldata(s, 0, p) = VAL(flip$)
        NEXT p
        INPUT #1, flip$ ' This should just be "END".
    NEXT s
LOOP

CLS 'Re-plot the points for 640x480, since this won't be fullscreen, like CK.
_TITLE "The TAME: MUSHROOM 1 (Sprite Table)"
LINE (0, 0)-(639, 479), 1, BF
LINE (0, 34)-(639, 479), 8, B
LINE (29, 34)-(29, 479), 8
LINE (58, 34)-(58, 479), 8
LINE (87, 34)-(87, 479), 8
LINE (116, 34)-(116, 479), 8
LINE (145, 34)-(145, 479), 8
LINE (174, 34)-(174, 479), 8
LINE (203, 34)-(203, 479), 8
LINE (232, 34)-(232, 479), 8
LINE (261, 34)-(261, 479), 8
LINE (290, 34)-(290, 479), 8
LINE (319, 34)-(319, 479), 8
LINE (348, 34)-(348, 479), 8
LINE (377, 34)-(377, 479), 8
LINE (406, 34)-(406, 479), 8
LINE (435, 34)-(435, 479), 8
LINE (464, 34)-(464, 479), 8
LINE (493, 34)-(493, 479), 8
LINE (522, 34)-(522, 479), 8
LINE (551, 34)-(551, 479), 8
LINE (580, 34)-(580, 479), 8
LINE (609, 34)-(609, 479), 8
LINE (0, 60)-(639, 60), 8
LINE (0, 86)-(639, 86), 8
LINE (0, 112)-(639, 112), 8
DRAW "B M0, 6 C15"
' TODO: Make this scroll horizontally, so I can display more help information.
'Font "CLICK A SPRITE TO EDIT, OR CLICK ''SHOW MAP LAYOUT''."
LOCATE 1, 1: PRINT "This screen shows every single sprite available for Mushroom 1. To edit one, just double-click it."
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
DRAW "B M8, 25 C15"
Font "OPTIONS"
DRAW "B M74, 21 C7"
Font "CHANGE"
DRAW "B M80, 28"
Font "LEVEL"
DRAW "B M134, 21"
Font "MAKE NEW"
DRAW "B M141, 28"
Font "SPRITE"
DRAW "B M205, 21"
Font "DELETE"
DRAW "B M205, 28"
Font "SPRITE"
DRAW "B M295, 25 C15"
Font "AREA X, ZONE 1"
DRAW "B M419,21 C14"
Font "SHOW THE"
DRAW "B M423,28"
Font "SPRITES"
DRAW "B M495,25 C15"
Font "VIEW MODE"
' TODO: Add buttons for "Change Level", "New Sprite" and "Delete Sprite".
'       Individually number each sprite, with a three-digit number.
DRAW "B M579,21 C7"
Font "SHOW THE"
DRAW "B M595,28"
Font "MAP"

DRAW "B M5,41 C15"
Font "000"
DRAW "B M35,41 C15"
Font "001"
DRAW "B M63,41 C15"
Font "002"

' TODO: These will animate, sooner or later. I'm planning on doing sixteen
'       different sprite files, named WLD?L?S0 through WLD?L?SF, using
'       hexadecimal characters 0-F, to store one frame of each sprite. Both
'       this, and the game engine itself, will load each frame into part of
'       the pixeldata variable, and switch between each frame of animation
'       while it waits for the user to click on something. It should also let
'       the user pick which frame of animation they want to edit.
'DRAW "B M3,43"
FOR p = 0 TO 21
    DRAW "B M" + STR$(3 + (29 * p)) + ",43"
    FOR c = 0 TO 7
        FOR r = 0 TO 7
            DRAW "C" + STR$(pixeldata(p, 0, (r + (c * 8))))
            DRAW "R2BD1BL2R2BR1BU1"
        NEXT r
        DRAW "BD2BL24"
    NEXT c
NEXT p


END
BadFile:

FileMistake1:

FileMistake2:
PRINT "There was no END of the sprite number chain!"

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
        LINE (3, 138)-(316, 196), 0, B
        LINE (4, 139)-(315, 195), , B
        DRAW "B M5,140"
        DRAW "C0"
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
        LINE (3, 3)-(316, 56), 0, B
        LINE (4, 4)-(315, 55), , B
        DRAW "B M5,5"
        DRAW "C0"
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
        DRAW "C6"
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
        DRAW "C8"
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

