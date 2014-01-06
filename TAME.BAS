DECLARE SUB DrawBox (BoxType%)
DECLARE SUB Sentence (Text$)
DECLARE SUB Font (Sentence$)
' THE TAME - Tile And Map Editor for any game using some form of CK's engine
' Copyright 2012 Robbie Bruce (Kamion R. Shanoriko)
'
' I should probably give this the ability to load map and sprite data files
' no matter what they're named, so if someone decides to mod Cricket, and
' change the global number of worlds to something other than 11, it'll be able
' to edit them all without a problem.

' We'll start in 320x200x16 colors (EGA), but just for the title screen.
SCREEN 7
PAINT (0, 0), 1
COLOR 14, 1
' Let's have a little fun, shall we?
RANDOMIZE TIMER
da = INT(RND * 3)
IF da = 0 THEN
  DRAW "C14 B M113,14": Sentence "Creeeeeeeepy..."
ELSEIF da = 1 THEN
  DRAW "C14 B M36,14": Sentence "Looks like a horror movie title, doesn't it?"
ELSEIF da = 2 THEN
  DRAW "C14 B M61,14": Sentence "A little humor isn't gonna KILL ya!"
END IF
LOCATE 3, 5:  PRINT "     €€€€€   €   €   €€€€€     "
LOCATE 4, 5:  PRINT "       ≤     ≤   ≤   ≤         "
LOCATE 5, 5:  PRINT "       ±     ±±±±±   ±±±±±     "
LOCATE 6, 5:  PRINT "       ±     ±   ±   ±         "
LOCATE 7, 5:  PRINT "       ∞     ∞   ∞   ∞∞∞∞∞     "
LOCATE 9, 5:  PRINT " €€€€€    €€€    €   €   €€€€€ "
LOCATE 10, 5: PRINT "   ≤     ≤   ≤   ≤≤ ≤≤   ≤     "
LOCATE 11, 5: PRINT "   ±     ±±±±±   ± ± ±   ±±±±± "
LOCATE 12, 5: PRINT "   ±     ±   ±   ±   ±   ±     "
LOCATE 13, 5: PRINT "   ∞     ∞   ∞   ∞   ∞   ∞∞∞∞∞ "
DRAW "B M94,110"
DRAW "C15": Font "T": DRAW "C14": Font "ILE ": DRAW "C15": Font "A"
DRAW "C14": Font "ND ": DRAW "C15": Font "M": DRAW "C14": Font "AP "
DRAW "C15": Font "E": DRAW "C14": Font "DITOR"
DRAW "B M59,140"
Sentence "Copyright 2012 Kamion R. Shanoriko"
DRAW "B M41,150"
Sentence "For any game that uses some version of"
DRAW "B M88, 160"
Sentence "Cricket Kamodon's engine"
DRAW "B M1,180"
Sentence "...even though you're welcome to tweak it for any game."
DRAW "C6 B M26,190"
Font "LET'S OPEN UP MUSHL1S0.KMD AND TAKE A LOOK!"

' For right now, this'll just look at MUSHL1S0.KMD, Mushroom 1's sprite data.
' TODO: Make a world and level selector, at least. Maybe even a world renamer.
ON ERROR GOTO BadFile
OPEN "MUSHL1S0.KMD" FOR INPUT AS 1
INPUT #1, look$
' Let's say it's there. Then we close it, and reopen it from the beginning.
CLOSE 1
OPEN "MUSHL1S0.KMD" FOR INPUT AS #1
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
  INPUT #1, tos$        ' Should just be the end-of-line marker (END).
  IF tos$ <> "END" GOTO FileMistake2    ' We'll make sure.
  loopz = loopz + 1
LOOP
CLOSE 1
PRINT "There are" + STR$(loopz) + " sprites in MUSHL1SD.KMD."

ON ERROR GOTO 0
OPEN "MUSHL1S0.KMD" FOR INPUT AS #1
' We open this a third time, so that we can get the sprite data the RIGHT way.
DIM SHARED pixeldata(0 TO (loopz - 1), 0 TO 63) AS INTEGER
DO UNTIL EOF(1)
FOR s = 0 TO (loopz - 1)
  FOR p = 0 TO 63
    INPUT #1, flip$
    pixeldata(s, p) = VAL(flip$)
  NEXT p
  INPUT #1, flip$               ' This should just be "END".
NEXT s
LOOP

SLEEP

SCREEN 13       ' Shifting to 256 color mode to show all of the sprite colors.
LINE (0, 0)-(319, 199), 1, BF
LINE (0, 30)-(319, 199), 8, B
LINE (29, 30)-(29, 199), 8
LINE (58, 30)-(58, 199), 8
LINE (87, 30)-(87, 199), 8
LINE (116, 30)-(116, 199), 8
LINE (0, 56)-(319, 56), 8
LINE (0, 82)-(319, 82), 8
LINE (0, 108)-(319, 108), 8
DRAW "B M0, 6 C15"
' TODO: Make this scroll horizontally, so I can display more help information.
Font "CLICK A SPRITE TO EDIT, OR CLICK ''SHOW MAP LAYOUT''."
DRAW "B M1, 10"
DrawBox 4
DRAW "B M64, 10"
DrawBox 4
DRAW "B M127, 10"
DrawBox 4
DRAW "B M194, 10"
DrawBox 3
DRAW "B M257, 10"
DrawBox 4
DRAW "B M12, 17 C7"
Font "CHANGE"
DRAW "B M16, 24"
Font "LEVEL"
DRAW "B M69, 17"
Font "MAKE NEW"
DRAW "B M75, 24"
Font "SPRITE"
DRAW "B M138, 17"
Font "DELETE"
DRAW "B M138, 24"
Font "SPRITE"
DRAW "B M205,17 C14"
Font "SPRITE"
DRAW "B M208,24"
Font "TABLE"
'DRAW "B M126,21 C15"
'Sentence "Looking at"
' TODO: Add buttons for "Change Level", "New Sprite" and "Delete Sprite".
'       Individually number each sprite, with a three-digit number.
DRAW "B M261,17 C7"
Font "SHOW MAP"
DRAW "B M268,24"
Font "LAYOUT"

DRAW "B M5,37 C15"
Font "000"
DRAW "B M35,37 C15"
Font "001"
DRAW "B M63,37 C15"
Font "002"

' TODO: These will animate, sooner or later. I'm planning on doing sixteen
'       different sprite files, named WLD?L?S0 through WLD?L?SF, using
'       hexadecimal characters 0-F, to store one frame of each sprite. Both
'       this, and the game engine itself, will load each frame into part of
'       the pixeldata variable, and switch between each frame of animation
'       while it waits for the user to click on something. It should also let
'       the user pick which frame of animation they want to edit.
DRAW "B M3,39"
FOR c = 0 TO 7
  FOR r = 0 TO 7
    DRAW "C" + STR$(pixeldata(0, (r + (c * 8))))
    DRAW "R2BD1BL2R2BR1BU1"
  NEXT r
  DRAW "BD2BL24"
NEXT c

DRAW "B M32,39"
FOR c = 0 TO 7
  FOR r = 0 TO 7
    DRAW "C" + STR$(pixeldata(1, (r + (c * 8))))
    DRAW "R2BD1BL2R2BR1BU1"
  NEXT r
  DRAW "BD2BL24"
NEXT c

DRAW "B M61,39"
FOR c = 0 TO 7
  FOR r = 0 TO 7
    DRAW "C" + STR$(pixeldata(2, (r + (c * 8))))
    DRAW "R2BD1BL2R2BR1BU1"
  NEXT r
  DRAW "BD2BL24"
NEXT c

DRAW "B M90,39"
FOR c = 0 TO 7
  FOR r = 0 TO 7
    DRAW "C" + STR$(pixeldata(3, (r + (c * 8))))
    DRAW "R2BD1BL2R2BR1BU1"
  NEXT r
  DRAW "BD2BL24"
NEXT c

DRAW "B M120,39"
FOR c = 0 TO 7
  FOR r = 0 TO 7
    DRAW "C" + STR$(pixeldata(4, (r + (c * 8))))
    DRAW "R2BD1BL2R2BR1BU1"
  NEXT r
  DRAW "BD2BL24"
NEXT c

DRAW "B M150,39"
FOR c = 0 TO 7
  FOR r = 0 TO 7
    DRAW "C" + STR$(pixeldata(5, (r + (c * 8))))
    DRAW "R2BD1BL2R2BR1BU1"
  NEXT r
  DRAW "BD2BL24"
NEXT c

DRAW "B M177,39"
FOR c = 0 TO 7
  FOR r = 0 TO 7
    DRAW "C" + STR$(pixeldata(6, (r + (c * 8))))
    DRAW "R2BD1BL2R2BR1BU1"
  NEXT r
  DRAW "BD2BL24"
NEXT c

DRAW "B M209,39"
FOR c = 0 TO 7
  FOR r = 0 TO 7
    DRAW "C" + STR$(pixeldata(7, (r + (c * 8))))
    DRAW "R2BD1BL2R2BR1BU1"
  NEXT r
  DRAW "BD2BL24"
NEXT c

DRAW "B M229,39"
FOR c = 0 TO 7
  FOR r = 0 TO 7
    DRAW "C" + STR$(pixeldata(8, (r + (c * 8))))
    DRAW "R2BD1BL2R2BR1BU1"
  NEXT r
  DRAW "BD2BL24"
NEXT c

DRAW "B M258,39"
FOR c = 0 TO 7
  FOR r = 0 TO 7
    DRAW "C" + STR$(pixeldata(9, (r + (c * 8))))
    DRAW "R2BD1BL2R2BR1BU1"
  NEXT r
  DRAW "BD2BL24"
NEXT c

DRAW "B M287,39"
FOR c = 0 TO 7
  FOR r = 0 TO 7
    DRAW "C" + STR$(pixeldata(10, (r + (c * 8))))
    DRAW "R2BD1BL2R2BR1BU1"
  NEXT r
  DRAW "BD2BL24"
NEXT c


SYSTEM
BadFile:

FileMistake1:

FileMistake2:
PRINT "There was no END of the sprite number chain!"

