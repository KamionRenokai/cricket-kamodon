' Cricket Kamodon (The Pseudo-Hero)
' Copyright 2011 Robbie Bruce (Kamion R. Shanoriko)
'
' This and all of the files that come with this game are open-source,
' released under v3 of the GNU General Public License. A copy of the license
' is readable in the LICENSE file.
'
' I've made this available as open-source because my knowledge of programming
' for major operating systems like Windows, Mac or Linux is... well, limited,
' to say the least. In fact, I barely know anything. I could learn C++, and
' possibly port Cricket Kamodon to Windows, or Linux, but that probably
' wouldn't happen right away. So, anyone else who likes this game, knows quite
' a bit of C or C++, and would like to change the code from QuickBASIC to C
' or C++, so it could be ported to another OS, feel free to do so, provided
' you give me credit for the original game and version.
'
' TODO: Speed up Cricket, implement "jumping", and maybe more of the level
'       layout of SMB1 World 1-1 for testing purposes?

DEFINT A-Z
'$DYNAMIC

DECLARE SUB Font (Sentence$)                    'From SUB90FNT.BAS
'NOTE: These two subcommands were borrowed from Mysterious Song, an RPG by
'DarkDread. (Darkness Ethereal)
DECLARE SUB LoadPal ()                          'Get the palette.
DECLARE SUB FadePal (Direction%, PaletteArray&()) 'Make the screen fade in/out

DIM SHARED Pal&(0 TO 255)                       ' Palette setup (GAME.PAL)

DIM SHARED Map%(0 TO 19, 0 TO 12)       ' This is the 2D array which will be
                                        ' used to hold the map data.

DIM SHARED Ground%(194)                 ' These are the arrays we need to
DIM SHARED GroundB%(146)
DIM SHARED Sky%(194)                    ' dimension in order to hold all of
DIM SHARED SkyB%(146)
DIM SHARED Desert%(194)                 ' our graphics.
DIM SHARED DesertB%(146)
DIM SHARED HitBox%(194)                 ' Arrays ending with "B" are half-size
DIM SHARED BoxB%(146)                   ' versions of tiles that will be used
DIM SHARED DeadBox%(194)                ' as the floor tile (row 13). Coded
DIM SHARED BridgeB%(146)                ' this way because QuickBASIC chokes
DIM SHARED Water%(194)                  ' if a full-size tile is used instead.
DIM SHARED WaterB%(146)
DIM SHARED Rock%(194)
DIM SHARED RockB%(146)
DIM SHARED Wall%(194)
DIM SHARED WallB%(146)

LoadPal                                 ' Get the palette.

' Set up the demo map display in memory.
FOR Y = 0 TO 12                         ' This little routine reads the DATA
  FOR X = 0 TO 19                       ' into the map array.  We will use
    READ Map%(X, Y)                     ' this DATA to draw the map and test
  NEXT X                                ' where the player can and can't
NEXT Y                                  ' move.

DATA 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
DATA 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
DATA 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
DATA 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
DATA 1,1,1,1,1,1,1,1,1,4,1,1,1,1,1,1,1,1,1,1
DATA 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
DATA 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
DATA 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
DATA 1,1,1,1,4,1,1,0,3,0,3,0,1,1,1,1,1,1,1,1
DATA 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
DATA 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
DATA 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

SCREEN 13                               ' Switch to 320x200x256 colors (13h).

GOSUB SetupTileData

DRAW "C2"
DRAW "B M0,55"
DRAW "U55"
DRAW "R318"
DRAW "D55"
DRAW "L319"
PAINT STEP(3, -3), 10, 2
DRAW "B U5"
DRAW "B R92"
DRAW "C0"
DRAW "B U40"
DRAW "B R6"
Font "CRICKET KAMODON"
DRAW "B L165"
DRAW "B D45"
Font "COPYRIGHT 2011 FLAMEWARE CORPORATION"

'Now for the bottom half of the loading screen...
DRAW "B M0,55"
DRAW "C2"
DRAW "D144"
DRAW "R318"
DRAW "U144"
DRAW "B D134"
DRAW "L318"
DRAW "B D7"
DRAW "B R80"
DRAW "C15"
Font "CHECKING THE HARDWARE"
DRAW "B M120,55"
DRAW "C2"
DRAW "D133"
DRAW "B M120,65"
DRAW "R197"
DRAW "B M130,62"
Font "SOUND DEVICES DETECTED"
DRAW "B M130,75"
Font "SOUND BLASTER OR COMPATIBLE"
DRAW "B M130,85"
Font "SOUND BLASTER PRO"
DRAW "B M130,95"
Font "SOUND BLASTER 16 16ASP AWE32"
DRAW "B M120,105"
DRAW "R197"
DRAW "B M130,112"
Font "AVAILABLE MAIN MEMORY"
DRAW "B M120,115"
DRAW "R197"
DRAW "B M132,125"
Font "         BYTES"
DRAW "B M120,133"
DRAW "R197"
DRAW "B M130,140"
Font "INPUT DEVICES DETECTED"
DRAW "B M120,143"
DRAW "R197"
DRAW "B M130,154"
Font "KEYBOARD"
DRAW "B M130,164"
Font "MOUSE"
DRAW "B M130,174"
Font "JOYSTICK"

'First, here's the obvious part: there's a keyboard in this PC.
DRAW "C10"
DRAW "B M130,154"
Font "KEYBOARD"
DRAW "C2"

'Second, we check to see if a Sound Blaster card exists in the PC.
BlastEnv$ = ENVIRON$("BLASTER")
CardType% = VAL(MID$(BlastEnv$, INSTR(BlastEnv$, "T") + 1, 1))
SELECT CASE CardType%
 CASE IS = 1 OR 3
    DRAW "C10"
    DRAW "B M130,75"
    Font "SOUND BLASTER OR COMPATIBLE"
 CASE IS = 2 OR 4 OR 5
    DRAW "C10"
    DRAW "B M130,85"
    Font "SOUND BLASTER PRO"
 CASE 6
    DRAW "C10"
    DRAW "B M130,95"
    Font "SOUND BLASTER 16 16ASP AWE32"
 CASE ELSE
 END SELECT
DRAW "C2"

'Third, we find out how much conventional memory (in bytes) is available.
LET ConvRAM! = FRE(-1)
DRAW "C10"
DRAW "B M132,125"
Font "         BYTES"
DRAW "B M130,125"
Font LTRIM$(STR$(ConvRAM!))

'Fourth, we pause for a second, then display "LET'S START!"
PAINT (25, 195), 0, 2
DRAW "B M50,196"
DRAW "C15"
Font "HIT THE ENTER KEY TO START THE GAME"
DRAW "B M10,60"
DO
LOOP UNTIL INKEY$ = CHR$(13)
PAINT (25, 195), 0, 2
DRAW "B M95,196"
DRAW "C15"
Font "STARTING THE GAME"
FadePal 0, Pal&()

FlameWareLogo:
CLS
DRAW "C7"
DRAW "B M50,180"
Font "F L A M E W A R E   P R E S E N T S"
DRAW "B M70,160"
DRAW "C4"
DRAW "R172"
DRAW "U140"
DRAW "H3"
DRAW "L166"
DRAW "G3"
DRAW "D140"
DRAW "B R10"
DRAW "U80"
DRAW "R152"
DRAW "D80"
DRAW "B U90"
DRAW "B L20"
PAINT STEP(5, 5), 4, 4
DRAW "C7"
DRAW "L123"
DRAW "U52"
DRAW "R123"
DRAW "D52"
DRAW "L123"
DRAW "B U46"
DRAW "B R25"
DRAW "L10"
DRAW "D40"
DRAW "R10"
DRAW "U40"
PAINT STEP(1, 1), 7, 7
DRAW "B M155,125"
DRAW "C4"
CIRCLE STEP(0, 0), 20, 4
DRAW "B M135,121"
DRAW "U20"
DRAW "F10"
DRAW "B E5"
DRAW "B L5"
DRAW "B D4"
DRAW "E10"
DRAW "F10"
DRAW "E10"
DRAW "D21"
PAINT STEP(-3, -0), 4, 4
PAINT STEP(-2, -13), 4, 4
PAINT STEP(-8, 0), 4, 4
PAINT STEP(-25, 2), 4, 4
FadePal 1, Pal&()
SLEEP 2
FadePal 0, Pal&()
CLS

'Let's see if I can't draw up a logo for our friend, master Kamodon...
'The "C"
DRAW "B M13,0"
DRAW "C10"
DRAW "R20"
DRAW "D8"
DRAW "L12"
DRAW "G8"
DRAW "D16"
DRAW "F8"
DRAW "R12"
DRAW "D8"
DRAW "L20"
DRAW "H12"
DRAW "U24"
DRAW "E12"
DRAW "B D2"
DRAW "P10,10"

'The "R"
DRAW "B U2"
DRAW "B R29"
DRAW "R28"
DRAW "F8"
DRAW "D10"
DRAW "G8"
DRAW "L12"
DRAW "F22"
DRAW "L8"
DRAW "H22"
DRAW "D22"
DRAW "L8"
DRAW "U47"
DRAW "B M+8,+5"
DRAW "R17"
DRAW "F4"
DRAW "D5"
DRAW "G4"
DRAW "L17"
DRAW "U13"
DRAW "B U1"
DRAW "P10,10"

'The "I"
DRAW "B U5"
DRAW "B R39"
DRAW "R40"
DRAW "D8"
DRAW "L12"
DRAW "D32"
DRAW "R12"
DRAW "D8"
DRAW "L40"
DRAW "U8"
DRAW "R12"
DRAW "U32"
DRAW "L12"
DRAW "U8"
DRAW "B M+2,+2"
DRAW "P10,10"

'The other "C"
DRAW "B M-2,-2"
DRAW "B R61"
DRAW "R20"
DRAW "D8"
DRAW "L12"
DRAW "G8"
DRAW "D16"
DRAW "F8"
DRAW "R12"
DRAW "D8"
DRAW "L20"
DRAW "H12"
DRAW "U24"
DRAW "E12"
DRAW "B D2"
DRAW "P10,10"

'The "K"
DRAW "B U2"
DRAW "B R28"
DRAW "D48"
DRAW "R12"
DRAW "U20"
DRAW "F20"
DRAW "R12"
DRAW "H24"
DRAW "E24"
DRAW "L12"
DRAW "G20"
DRAW "U20"
DRAW "L12"
DRAW "B M+2,+2"
DRAW "P10,10"

'The "E"
DRAW "B M-2,-2"
DRAW "B R53"
DRAW "R40"
DRAW "D8"
DRAW "L28"
DRAW "D12"
DRAW "R28"
DRAW "D8"
DRAW "L28"
DRAW "D12"
DRAW "R28"
DRAW "D8"
DRAW "L40"
DRAW "U47"
DRAW "B M+2,+2"
DRAW "P10,10"

'The "T"
DRAW "B M-2,-3"
DRAW "B R48"
DRAW "R40"
DRAW "D8"
DRAW "L12"
DRAW "D40"
DRAW "L16"
DRAW "U40"
DRAW "L12"
DRAW "U8"
DRAW "B M+2,+2"
DRAW "P10,10"

' Second row!

'The other "K"
DRAW "B M0,150"
DRAW "D48"
DRAW "R12"
DRAW "U20"
DRAW "F20"
DRAW "R12"
DRAW "H24"
DRAW "E24"
DRAW "L12"
DRAW "G20"
DRAW "U20"
DRAW "L12"
DRAW "B M+2,+2"
DRAW "P10,10"

'The "A"
DRAW "B M-2,-2"
DRAW "B R58"
DRAW "R22"
DRAW "F8"
DRAW "D40"
DRAW "L8"
DRAW "U20"
DRAW "L22"
DRAW "D20"
DRAW "L8"
DRAW "U40"
DRAW "E8"
DRAW "B D18"
DRAW "U6"
DRAW "E4"
DRAW "R14"
DRAW "F4"
DRAW "D6"
DRAW "L22"
DRAW "B M-2,0"
DRAW "P10,10"

'The "M"
DRAW "B D30"
DRAW "B R38"
DRAW "U48"
DRAW "R8"
DRAW "F14"
DRAW "E14"
DRAW "R8"
DRAW "D48"
DRAW "L8"
DRAW "U34"
DRAW "G14"
DRAW "H14"
DRAW "D34"
DRAW "L8"
DRAW "B E2"
DRAW "P10,10"

'The "O"
DRAW "B D2"
DRAW "B R54"
DRAW "H6"
DRAW "U36"
DRAW "E6"
DRAW "R30"
DRAW "F6"
DRAW "D36"
DRAW "G6"
DRAW "L30"
DRAW "B U8"
DRAW "B R5"
DRAW "H3"
DRAW "U26"
DRAW "E3"
DRAW "R20"
DRAW "F3"
DRAW "D26"
DRAW "G3"
DRAW "L20"
DRAW "B D2"
DRAW "P10,10"

'The "D"
DRAW "B D6"
DRAW "B R37"
DRAW "U48"
DRAW "R30"
DRAW "F6"
DRAW "D36"
DRAW "G6"
DRAW "L30"
DRAW "BU8BR8"
DRAW "U31"
DRAW "R16"
DRAW "F3"
DRAW "D25"
DRAW "G3"
DRAW "L16"
DRAW "B D2"
DRAW "P10,10"

'The other "O"
DRAW "B D6"
DRAW "B R40"
DRAW "H6"
DRAW "U36"
DRAW "E6"
DRAW "R30"
DRAW "F6"
DRAW "D36"
DRAW "G6"
DRAW "L30"
DRAW "B U8"
DRAW "B R5"
DRAW "H3"
DRAW "U26"
DRAW "E3"
DRAW "R20"
DRAW "F3"
DRAW "D26"
DRAW "G3"
DRAW "L20"
DRAW "B D2"
DRAW "P10,10"

'The "N"
DRAW "B R36"
DRAW "B D6"
DRAW "U48"
DRAW "R12"
FOR i% = 1 TO 13
DRAW "BD1BR1"
DRAW "D2"
NEXT
DRAW "BD1BR1"
DRAW "U40"
DRAW "R12"
DRAW "D48"
DRAW "L14"
FOR i% = 1 TO 13
DRAW "BU1BL1"
DRAW "U2"
NEXT
DRAW "BU1BL1"
DRAW "D40"
DRAW "L10"
DRAW "BU2BR2"
DRAW "P10,10"
FadePal 1, Pal&()

' This captures the number of seconds that have passed since the logo came
' up, and waits for the player to press the ENTER key (CHR$(13)). For every
' 3/4ths of a second the player doesn't press anything, the words "PRESS
' ENTER" will flash on and off the screen.
' TimeIsNow! represents the original starting time.
' ClockCheck! is the additional starting time, which keeps counting.
' WhatIsTime! is the difference between ClockCheck! and TimeIsNow!. This is
'             how the time-passing statements work.
' FlashColor is either a 0 (flash off) or a 1 (flash on), and changes every
'            time WhatIsTime! (which is ClockCheck! minus TimeIsNow!) hits .75
'            seconds.
DO WHILE INKEY$ <> ""                   ' This fixes a bug where the game will
LOOP                                    ' keep going past the title screen if
                                        ' the player presses any key while the
                                        ' FlameWare logo is on the screen.
LET TimeIsNow! = TIMER
DO WHILE INKEY$ <> CHR$(13)
LET ClockCheck! = TIMER
LET WhatIsTime! = ClockCheck! - TimeIsNow!
IF WhatIsTime! >= .75 THEN
  IF FlashColor THEN LET FlashColor = 0 ELSE LET FlashColor = 1
  IF FlashColor = 0 THEN
    DRAW "B M116,100"
    DRAW "C0"
    Font "PRESS ENTER"
  ELSE
    DRAW "B M116,100"
    DRAW "C15"
    Font "PRESS ENTER"
  END IF
LET TimeIsNow! = TIMER
LET ClockCheck! = TIMER
LET WhatIsTime! = 0
END IF
LOOP

' This triggers a fade-out, and starts drawing the scoreboard at the top of
' the screen, along with information about the test level.
FadePal 0, Pal&()
CLS
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
DRAW "B M109,20"
DRAW "C0"
Font "0"
DRAW "C15"
DRAW "B M125,20"
Font "AMP"
DRAW "C0"
DRAW "B M261,20"
Font "NORMAL"

DRAW "C15"
DRAW "B M120,80"
Font "MUSHROOM 1"
DRAW "B M116,100"
Font "3 LIVES LEFT"
DRAW "B M120,120"
Font "GO, CRICKET!"
FadePal 1, Pal&()

' With the "GO, CRICKET!" message on the screen, using the
' TimeIsNow!/ClockCheck!/WhatIsTime! method mentioned earlier, the game either
' waits for the player to press ENTER, or keeps going, after 3 seconds.
LET TimeIsNow! = TIMER
DO WHILE WhatIsTime! < 3! AND NOT INKEY$ = CHR$(13)
LET ClockCheck! = TIMER
LET WhatIsTime! = ClockCheck! - TimeIsNow!
IF INKEY$ = CHR$(13) THEN EXIT DO
LOOP

' This command sequence uses the time-trapping method again, but uses it to
' gradually fade the level information screen out, color by color, from color
' 31 (white) to 16 (black), stepping backwards. If you're wondering why I
' didn't just call FadePal, it's because I wanted to only fade out the text,
' and keep the scoreboard on the screen. FadePal doesn't pick and choose.
LET TimeIsNow! = TIMER
FOR i% = 31 TO 16 STEP -1
LET ColorMode$ = "C" + STR$(i%)
DRAW ColorMode$
DRAW "B M120,80"
Font "MUSHROOM 1"
DRAW "B M116,100"
Font "3 LIVES LEFT"
DRAW "B M120,120"
Font "GO, CRICKET!"
LET ClockCheck! = TIMER
LET WhatIsTime! = 0
DO UNTIL WhatIsTime! >= .01
LET ClockCheck! = TIMER
LET WhatIsTime! = ClockCheck! - TimeIsNow!
LOOP
LET TimeIsNow! = TIMER
NEXT

GOSUB RedrawMapData

DRAW "C11"
'For testing purposes, I'll make a mock up of SMB1 World 1-1 in wireframe.
'LINE (0, 193)-(319, 193), 11, B
'LINE (110, 121)-(251, 136), 11, B
'LINE (50, 121)-(70, 136), 11, B
'LINE (170, 56)-(190, 71), 11, B

'Define Cricket's coordinates, for detecting sprite collision later on.
'LET CricketUL% = 0
'LET CricketUR% = 20
'LET CricketDL% = 160
'LET CricketDR% = 192
LET CricketX = 0
LET CricketY1 = 10
LET CricketY2 = 11

'Then draw abstract him, using two sprites, in a 16x32 fashion, just for now.
'PUT (CricketUL%, CricketDL%), Wall%, PSET
'PUT (CricketUL%, CricketDL% + 16), Wall%, PSET
PUT (CricketX * 16, (CricketY1 * 16) - 8), Wall%, PSET
PUT (CricketX * 16, (CricketY2 * 16) - 8), Wall%, PSET

' Key-trapping time! INKEY$ is the variable that gives out what's called an
' "ASCII character code" each time the player presses a key. Since just
' doing a DO...LOOP with INKEY$ alone makes character movement look weird,
' you instead set up a LOOP like the one I've done below, which waits for the
' player to press a key, sets INKEY$ to a char code, and sets that to Key$.
' Then Key$ is what we work with.
LoopStart:
DO WHILE Key$ = ""
LET Key$ = INKEY$
LOOP

' Arrow key trapping is funny, because rather than just giving you one char
' code, like most of the other keys do, it gives you two: a blank character
' (which you grab by putting CHR$(0) in front), and another letter.

' This is the only problem with this: putting a really small timeout for the
' movement animation makes the soon-to-be-Cricket tiles move slower than I'd
' like. If I increase the timeout, you can actually see him "run", but if I
' get rid of the timeout altogether, he just zooms like he's hyped on coffee.
IF Key$ = CHR$(0) + "K" THEN     'Left arrow key
  IF CricketX > 0 THEN           'Is Cricket all the way to the left side?
    LET CricketMove = CricketX * 16
    LET CricketX = CricketX - 1
    LET CricketStop = CricketX * 16
    DO UNTIL CricketMove = CricketStop  'Here's our super slow timeout code.
      LET TimeIsNow! = TIMER
      DO WHILE WhatIsTime! < .00009
        LET PressCheck$ = INKEY$
        LET ClockCheck! = TIMER
        LET WhatIsTime! = ClockCheck! - TimeIsNow!
        'Theoretically, if the player holds down the left arrow key, this
        'should speed up the animation, so Cricket will start to "run".
        '"Should" and "will" are two different things... doesn't seem to work.
        IF PressCheck$ = CHR$(0) + "K" THEN IF WhatIsTime! >= .000001 THEN EXIT DO
        IF WhatIsTime! >= .00009 THEN EXIT DO
      LOOP
      LET WhatIsTime! = 0
      LET CricketMove = CricketMove - 1
      GOSUB RedrawMapData  ' Redraw the screen, and change Cricket's position.
      PUT (CricketMove, (CricketY1 * 16) - 8), Wall%, PSET
      PUT (CricketMove, (CricketY2 * 16) - 8), Wall%, PSET
    LOOP
    'GOSUB RedrawMapData
    'PUT (CricketX * 16, CricketY1 * 16), Wall%, PSET
    'PUT (CricketX * 16, CricketY2 * 16), Wall%, PSET
  END IF
ELSEIF Key$ = CHR$(0) + "M" THEN 'Right arrow key
  IF CricketX < 19 THEN         ' Is Cricket all the way to the right side?
    CricketMove = CricketX * 16
    CricketX = CricketX + 1
    CricketStop = CricketX * 16
    DO UNTIL CricketMove = CricketStop
      TimeIsNow! = TIMER
      DO UNTIL WhatIsTime! > .00009
        PressCheck$ = INKEY$
        ClockCheck! = TIMER
        WhatIsTime! = ClockCheck! - TimeIsNow!
        'Theoretically, if the player holds down the right arrow key, this
        'should speed up the animation, so Cricket will start to "run".
        '"Should" and "will" are two different things. Doesn't seem to work.
        IF PressCheck$ = CHR$(0) + "M" THEN IF WhatIsTime! >= .000001 THEN EXIT DO
        IF WhatIsTime! >= .00009 THEN EXIT DO
      LOOP
      WhatIsTime! = 0
      CricketMove = CricketMove + 1
      GOSUB RedrawMapData  ' Redraw the screen, and change Cricket's position.
      PUT (CricketMove, (CricketY1 * 16) - 8), Wall%, PSET
      PUT (CricketMove, (CricketY2 * 16) - 8), Wall%, PSET
    LOOP
    'LET CricketX = CricketX + 1
    'GOSUB RedrawMapData
    'PUT (CricketX * 16, CricketY1 * 16), Wall%, PSET
    'PUT (CricketX * 16, CricketY2 * 16), Wall%, PSET
  END IF
ELSEIF Key$ = "z" OR Key$ = "Z" THEN 'The "Z" key is jump.
'I've yet to code this in, since I wanna get the movement speed down, first.
ELSEIF Key$ = CHR$(27) THEN END
END IF
LET Key$ = ""
GOTO LoopStart

RedrawMapData:          ' Redraw map subroutine, redone to fit a row of 16x16
FOR Y = 2 TO 12         ' tiles below the status bar, 10 across, 19 down.
  FOR X = 0 TO 19
    IF Map%(X, Y) = 0 THEN
        PUT (X * 16, (Y * 16) - 8), Ground%, PSET
    ELSEIF Map%(X, Y) = 1 THEN
        PUT (X * 16, (Y * 16) - 8), Sky%, PSET
    ELSEIF Map%(X, Y) = 2 THEN
        PUT (X * 16, (Y * 16) - 8), Desert%, PSET
    ELSEIF Map%(X, Y) = 3 THEN
        PUT (X * 16, (Y * 16) - 8), HitBox%, PSET
    ELSEIF Map%(X, Y) = 4 THEN
        PUT (X * 16, (Y * 16) - 8), DeadBox%, PSET
    ELSEIF Map%(X, Y) = 5 THEN
        PUT (X * 16, (Y * 16) - 8), Water%, PSET
    ELSEIF Map%(X, Y) = 6 THEN
        PUT (X * 16, (Y * 16) - 8), Rock%, PSET
    ELSEIF Map%(X, Y) = 7 THEN
        PUT (X * 16, (Y * 16) - 8), Wall%, PSET
    END IF
  NEXT X
NEXT Y

RETURN  ' Why did I stop it here? Probably because it doesn't scroll yet?
' For absolute grins and giggles, it says "Mushroom 1", since it's SMB1 1-1.
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
Font "NORMAL"  ' I remember what the AMP was for, but what'd I make this for?
RETURN

SetupTileData:  ' Setting up the tiles, super fast, before the game starts.
LINE (0, 0)-(15, 15), 6, BF             ' This is the flooring tile.
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

GET (0, 0)-(15, 15), Ground%            ' Now that's in the Ground% array.

LINE (0, 0)-(15, 15), 0, BF             ' Then we blank the screen.

LINE (0, 0)-(15, 15), 108, BF           ' This is our Sky tile, minus clouds.

GET (0, 0)-(15, 15), Sky%               ' Now that's in the Sky% array.

LINE (0, 0)-(15, 15), 0, BF             ' Then we blank the screen, again.

LINE (0, 0)-(15, 15), 43, BF            ' UNUSED TILE #2 (Desert)

GET (0, 0)-(15, 15), Desert%            ' Now that's in the Desert% array.

LINE (0, 0)-(15, 15), 0, BF             ' Then we blank the screen, again.

LINE (0, 0)-(15, 15), 56, BF            ' This is our hit box tile.
LINE (1, 1)-(14, 14), 57, BF
LINE (2, 2)-(13, 13), 58, BF
LINE (3, 3)-(12, 12), 59, BF
LINE (4, 4)-(11, 11), 60, BF
LINE (5, 5)-(10, 10), 61, BF
LINE (6, 6)-(9, 9), 62, BF
LINE (7, 7)-(8, 8), 63, BF
LINE (2, 5)-(2, 9), 15
LINE (2, 7)-(5, 7), 15
LINE (5, 5)-(5, 9), 15
LINE (7, 5)-(7, 9), 15
LINE (9, 5)-(13, 5), 15
LINE (11, 5)-(11, 9), 15

GET (0, 0)-(15, 15), HitBox%            ' Now that's in the HitBox% array.

LINE (0, 0)-(15, 15), 0, BF             ' Then we blank the screen, again.

LINE (0, 0)-(15, 15), 36, BF            ' This is our "dead" hit box tile.
LINE (1, 1)-(14, 14), 37, BF
LINE (2, 2)-(13, 13), 38, BF
LINE (3, 3)-(12, 12), 39, BF
LINE (4, 4)-(11, 11), 40, BF
LINE (5, 5)-(10, 10), 41, BF
LINE (6, 6)-(9, 9), 42, BF
LINE (7, 7)-(8, 8), 43, BF
LINE (3, 5)-(5, 5), 15
LINE (2, 6)-(2, 9), 15
LINE (3, 10)-(5, 10), 15
LINE (6, 6)-(6, 9), 15
LINE (8, 5)-(8, 10), 15
LINE (8, 10)-(10, 8), 15
LINE (10, 8)-(12, 10), 15
LINE (12, 10)-(12, 5), 15

GET (0, 0)-(15, 15), DeadBox%           ' Now that's in the DeadBox% array.

LINE (0, 0)-(15, 15), 0, BF             ' Then we blank the screen, again.

LINE (0, 0)-(15, 15), 59, BF            ' This is our water tile.

GET (0, 0)-(15, 15), Water%             ' Now that's in the Water% array.

LINE (0, 0)-(15, 15), 0, BF             ' Then we blank the screen, again.

LINE (0, 0)-(15, 15), 124, BF           ' UNUSED TILE #6 (Rock)

GET (0, 0)-(15, 15), Rock%              ' Now that's in the Rock% array.

LINE (0, 0)-(15, 15), 0, BF             ' Then we blank the screen, again.

LINE (0, 0)-(15, 15), 23, BF            ' This will probably be part of
                                        ' the Cricket sprite, at some point.

GET (0, 0)-(15, 15), Wall%              ' Now that's in the Wall% array.

LINE (0, 0)-(15, 15), 0, BF             ' Then we blank the screen, again.
RETURN

'This was also from Mysterious Song by DarkDread. I needed it for fadeout and
'fade in techniques.
SUB FadePal (Direction%, PaletteArray&())

    IF Direction% = 0 THEN
        '*** Fade palette down ***

        'Break down all 256 colours into their RGB values and
        'calculate how much each will need fading down by.
        DIM RGBval!(0 TO 255, 0 TO 2)
        DIM SubVal!(0 TO 255, 0 TO 2)
        FOR n = 0 TO 255
            C& = PaletteArray&(n)
            B = C& \ 65536: C& = C& - B * 65536
            g = C& \ 256: C& = C& - g * 256
            r = C&
            RGBval!(n, 0) = r
            RGBval!(n, 1) = g
            RGBval!(n, 2) = B
            SubVal!(n, 0) = r / 63
            SubVal!(n, 1) = g / 63
            SubVal!(n, 2) = B / 63
        NEXT n

        'Fade down all 256 colours in 63 steps.
        FOR j = 1 TO 63
            'Calculate new faded down RGB values.
            FOR n = 0 TO 255
                RGBval!(n, 0) = RGBval!(n, 0) - SubVal!(n, 0)
                RGBval!(n, 1) = RGBval!(n, 1) - SubVal!(n, 1)
                RGBval!(n, 2) = RGBval!(n, 2) - SubVal!(n, 2)
            NEXT n

            'Write faded down colours directly to the video card.
            WAIT &H3DA, &H8, &H8: WAIT &H3DA, &H8
            FOR n = 0 TO 255
                OUT &H3C8, n             'Select attribute.
                OUT &H3C9, RGBval!(n, 0) 'Write red.
                OUT &H3C9, RGBval!(n, 1) 'Write green.
                OUT &H3C9, RGBval!(n, 2) 'Write blue.
            NEXT n
        NEXT j
    ELSE
        '*** Fade palette up ***

        'Break down all 256 colours into their RGB values and
        'calculate how much each will need fading up by.
        DIM RGBval!(0 TO 255, 0 TO 2)
        DIM AddVal!(0 TO 255, 0 TO 2)
        FOR n = 0 TO 255
            C& = PaletteArray&(n)
            B = C& \ 65536: C& = C& - B * 65536
            g = C& \ 256: C& = C& - g * 256
            r = C&
            AddVal!(n, 0) = r / 63
            AddVal!(n, 1) = g / 63
            AddVal!(n, 2) = B / 63
        NEXT n

        'Fade up all 256 colours in 63 steps.
        FOR j = 1 TO 63
            'Calculate new faded up RGB values.
            FOR n = 0 TO 255
                RGBval!(n, 0) = RGBval!(n, 0) + AddVal!(n, 0)
                RGBval!(n, 1) = RGBval!(n, 1) + AddVal!(n, 1)
                RGBval!(n, 2) = RGBval!(n, 2) + AddVal!(n, 2)
            NEXT n

            'Write faded up colours directly to the video card.
            WAIT &H3DA, &H8, &H8: WAIT &H3DA, &H8
            FOR n = 0 TO 255
                OUT &H3C8, n             'Select attribute.
                OUT &H3C9, RGBval!(n, 0) 'Write red.
                OUT &H3C9, RGBval!(n, 1) 'Write green.
                OUT &H3C9, RGBval!(n, 2) 'Write blue.
            NEXT n
        NEXT j
    END IF

END SUB

'LoadPal was from Mysterious Song by DarkDread.
SUB LoadPal

    FileNum = FREEFILE
    OPEN "game.pal" FOR BINARY AS #FileNum
    FOR n = 0 TO 255
        GET #FileNum, , Colour&
        Pal&(n) = Colour&
    NEXT n
    CLOSE #FileNum

    DIM RGBval(0 TO 255, 0 TO 2)
    FOR n = 0 TO 255
        C& = Pal&(n)
        B = C& \ 65536: C& = C& - B * 65536
        g = C& \ 256: C& = C& - g * 256
        r = C&
        RGBval(n, 0) = r
        RGBval(n, 1) = g
        RGBval(n, 2) = B
    NEXT n

    WAIT &H3DA, &H8, &H8: WAIT &H3DA, &H8
    FOR n = 0 TO 255
        OUT &H3C8, n
        OUT &H3C9, RGBval(n, 0)
        OUT &H3C9, RGBval(n, 1)
        OUT &H3C9, RGBval(n, 2)
    NEXT n
    ERASE RGBval

END SUB

