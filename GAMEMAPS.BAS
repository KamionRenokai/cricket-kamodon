DECLARE SUB Font (Sentence$)
' Cricket Kamodon
' Game Map Module
' Code borrowed from a tile sprite tutorial by DarkDread (Matt Zuchowski).
' Cricket Kamodon copyright 2011 Robbie Bruce (Kamion Shanoriko).
' CHANGES: Original tutorial sets up 20x20 sprites. Since it's designed for
'          a QuickBASIC RPG, and I'm programming a 2D sidescroller, I changed
'          them to 16x16. Cricket himself will either be 16x32 or 32x32.

DECLARE SUB SetupMapData ()             ' Set up the map contents (SMB1 1-1).
DECLARE SUB SetupTileData ()            ' Set up tile appearances (SMB1 1-1).
DECLARE SUB RedrawMapData ()

DIM SHARED Map%(0 TO 19, 0 TO 12)       ' This is the 2D array which will be
                                        ' used to hold the map data.

DIM SHARED Ground%(194)                 ' These are the arrays we need to
DIM SHARED GroundB%(146)
DIM SHARED Sky%(194)                    ' dimension in order to hold all of
DIM SHARED SkyB%(146)
DIM SHARED Desert%(194)                 ' our graphics.
DIM SHARED DesertB%(146)
DIM SHARED Dirt%(194)                   ' Arrays ending with "B" are half-size
DIM SHARED DirtB%(146)                  ' versions of tiles that will be used
DIM SHARED DeadBox%(194)                 ' as the floor tile (row 20). Coded
DIM SHARED BridgeB%(146)                ' this way because QuickBASIC chokes
DIM SHARED Water%(194)                  ' if a full-size tile is used instead.
DIM SHARED WaterB%(146)
DIM SHARED Rock%(194)
DIM SHARED RockB%(146)
DIM SHARED Wall%(194)
DIM SHARED WallB%(146)

SUB RedrawMapData
' Basically, this code will check what number is at every position in the
' map and put the right graphic, in the right spot on the screen according
' to what number is associated with what graphic. If the graphic is on row 13,
' the code will put the half-sized graphic, instead.

FOR Y = 0 TO 12
  FOR X = 0 TO 19
    IF Map%(X, Y) = 0 THEN
      IF Y = 12 THEN
        PUT (X * 16, (Y * 16) - 16), GroundB%, PSET
      ELSE
        PUT (X * 16, (Y * 16) - 16), Ground%, PSET
      END IF
    ELSEIF Map%(X, Y) = 1 THEN
      IF Y = 12 THEN
        PUT (X * 16, (Y * 16) - 16), SkyB%, PSET
      ELSE
        PUT (X * 16, (Y * 16) - 16), Sky%, PSET
      END IF
    ELSEIF Map%(X, Y) = 2 THEN
      IF Y = 12 THEN
        PUT (X * 16, (Y * 16) - 16), DesertB%, PSET
      ELSE
        PUT (X * 16, (Y * 16) - 16), Desert%, PSET
      END IF
    ELSEIF Map%(X, Y) = 3 THEN
      IF Y = 12 THEN
        PUT (X * 16, (Y * 16) - 16), DirtB%, PSET
      ELSE
        PUT (X * 16, (Y * 16) - 16), Dirt%, PSET
      END IF
    ELSEIF Map%(X, Y) = 4 THEN
      IF Y = 12 THEN
        PUT (X * 16, (Y * 16) - 16), BridgeB%, PSET
      ELSE
        PUT (X * 16, (Y * 16) - 16), DeadBox%, PSET
      END IF
    ELSEIF Map%(X, Y) = 5 THEN
      IF Y = 12 THEN
        PUT (X * 16, (Y * 16) - 16), WaterB%, PSET
      ELSE
        PUT (X * 16, (Y * 16) - 16), Water%, PSET
      END IF
    ELSEIF Map%(X, Y) = 6 THEN
      IF Y = 12 THEN
        PUT (X * 16, (Y * 16) - 16), RockB%, PSET
      ELSE
        PUT (X * 16, (Y * 16) - 16), Rock%, PSET
      END IF
    ELSEIF Map%(X, Y) = 7 THEN
      IF Y = 12 THEN
        PUT (X * 16, (Y * 16) - 16), WallB%, PSET
      ELSE
        PUT (X * 16, (Y * 16) - 16), Wall%, PSET
      END IF
    END IF
  NEXT X
NEXT Y

DRAW "C15"
DRAW "B M0,0"
LINE (0, 0)-(319, 22), 10, BF
LINE (0, 0)-(40, 22), 1, BF
LINE (160, 0)-(212, 11), 1, BF
LINE (245, 0)-(285, 11), 1, BF
LINE (120, 12)-(150, 22), 1, BF
LINE (245, 12)-(319, 22), 2, BF
DRAW "C15"
DRAW "B M7,8"
Font "ZONE"
DRAW "C0"
DRAW "B M44,8"
Font "MUSHROOM 1"
DRAW "C15"
DRAW "B M164,8"
Font "CRICKET"
DRAW "C0"
DRAW "B M226,8"
Font "3"
DRAW "C15"
DRAW "B M252,8"
Font "TIME"
DRAW "C0"
DRAW "B M292,8"
Font "400"
DRAW "C15"
DRAW "B M3,20"
Font "SCORE"
DRAW "B M44,20"
DRAW "C0"
Font "0000000000"
DRAW "C15"
DRAW "B M125,20"
Font "AMP"
DRAW "C0"
DRAW "B M261,20"
Font "NORMAL"

END SUB

SUB SetupMapData

FOR Y = 0 TO 13                         ' This little routine reads the DATA
  FOR X = 0 TO 19                       ' into the map array.  We will use
    READ Map%(X, Y)                     ' this DATA to draw the map and test
  NEXT X                                ' where the player can and can't
NEXT Y                                  ' move.

' This is the tile setup routine for the first screen of a mock-up of World
' 1-1 from SMB1 for the NES. I'm just using this to test the character
' sprite's motor functions, like moving and jumping, for starters.

END SUB

SUB SetupTileData
' This draws each tile on-screen, then saves them into arrays, one for full-
' size, and one for half-size. This will usually happen while the screen is
' black, so if the computer slows down and actually shows them being drawn,
' the player won't see them. We must be secretive.

LINE (0, 0)-(15, 15), 6, BF             ' This is our ground tile.
LINE (1, 1)-(1, 1), 92
LINE (1, 2)-(2, 2), 42
LINE (2, 2)-(2, 1), 42
LINE (1, 14)-(1, 14), 92
LINE (1, 13)-(2, 13), 42
LINE (2, 13)-(2, 14), 42
LINE (14, 14)-(14, 14), 92
LINE (13, 14)-(13, 13), 42
LINE (13, 13)-(14, 13), 42
LINE (14, 1)-(14, 1), 92
LINE (13, 1)-(13, 2), 42
LINE (13, 2)-(14, 2), 42
LINE (3, 3)-(11, 11), 92, BF
LINE (4, 12)-(12, 12), 42
LINE (12, 12)-(12, 4), 42

GET (0, 0)-(15, 15), Ground%            ' This puts what we have just drawn
GET (0, 0)-(15, 7), GroundB%            'into the Ground% and GroundB% arrays.

LINE (0, 0)-(15, 15), 0, BF             ' This command clears what we have
                                        ' just drawn so that we may draw our
                                        ' next tile.

LINE (0, 0)-(15, 15), 77, BF            ' This is our Sky tile.

GET (0, 0)-(15, 15), Sky%               ' This puts what we have just drawn
GET (0, 0)-(15, 7), SkyB%               ' into the Sky% and SkyB% arrays.

LINE (0, 0)-(15, 15), 0, BF             ' This command clears what we have
                                        ' just drawn so that we may draw our
                                        ' next tile.

LINE (0, 0)-(15, 15), 43, BF            ' This is our desert tile.

GET (0, 0)-(15, 15), Desert%            ' This puts what we have just drawn
GET (0, 0)-(15, 7), DesertB%            'into the Desert% and DesertB% arrays.

LINE (0, 0)-(15, 15), 0, BF             ' This command clears what we have
                                        ' just drawn so that we may draw our
                                        ' next tile.

LINE (0, 0)-(15, 15), 113, BF           ' This is our dirt path tile.

GET (0, 0)-(15, 15), Dirt%              ' This puts what we have just drawn
GET (0, 0)-(15, 7), DirtB%              ' into the Dirt% and DirtB% arrays.

LINE (0, 0)-(15, 15), 0, BF             ' This command clears what we have
                                        ' just drawn so that we may draw our
                                        ' next tile.

LINE (0, 0)-(15, 15), 6, BF             ' This is our bridge tile.

GET (0, 0)-(15, 15), DeadBox%            ' This puts what we have just drawn
GET (0, 0)-(15, 7), BridgeB%            'into the DeadBox% and BridgeB% arrays.

LINE (0, 0)-(15, 15), 0, BF             ' This command clears what we have
                                        ' just drawn so that we may draw our
                                        ' next tile.

LINE (0, 0)-(15, 15), 105, BF           ' This is our water tile.

GET (0, 0)-(15, 15), Water%             ' This puts what we have just drawn
GET (0, 0)-(15, 7), WaterB%             ' into the Water% and WaterB% arrays.

LINE (0, 0)-(15, 15), 0, BF             ' This command clears what we have
                                        ' just drawn so that we may draw our
                                        ' next tile.

LINE (0, 0)-(15, 15), 124, BF           ' This is our rock/mountain tile.

GET (0, 0)-(15, 15), Rock%              ' This puts what we have just drawn
GET (0, 0)-(15, 7), RockB%              ' into the Rock% and RockB% arrays.

LINE (0, 0)-(15, 15), 0, BF             ' This command clears what we have
                                        ' just drawn so that we may draw our
                                        ' next tile.

LINE (0, 0)-(15, 15), 23, BF            ' This is our wall tile.

GET (0, 0)-(15, 15), Wall%              ' This puts what we have just drawn
GET (0, 0)-(15, 7), WallB%              ' into the Wall% and WallB% arrays.

LINE (0, 0)-(15, 15), 0, BF             ' This command clears what we have
                                        ' just drawn.

END SUB

