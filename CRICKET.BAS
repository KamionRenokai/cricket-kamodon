' Cricket Kamodon (The Pseudo-Hero)
' Copyright 2011-2012 Robbie Bruce (Kamion R. Shanoriko)
'
' This and all of the files that come with this game are open-source,
' released under v3 of the GNU General Public License. A copy of the license
'ÿis readable in the LICENSE file.
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
'       layout of SMB1 World 1-1 for testing purposes? Now let's code some
'       levels, like Generitica 1, for example!
' ^^    Let's design some clouds, a sun, and some shadowed buildings for the
'       background layer, then either add more to the level, or think about
'       how we're going to implement the sprite plane, or object layer. I
'       actually thought having two tile planes was gonna slow things down,
'       but surprisingly, it practically didn't miss a beat. How about three?

DEFINT A-Z
'$DYNAMIC

DECLARE SUB Font (Sentence$)                    'From SUB90FNT.BAS
'NOTE: These two subcommands were borrowed from Mysterious Song, an RPG by
'DarkDread. (Darkness Ethereal)
DECLARE SUB LoadPal ()                          'Get the palette.
DECLARE SUB FadePal (Direction%, PaletteArray&()) 'Make the screen fade in/out
' From "Multikey Function Update" by Joe Huber, Jr. (with parts by Eric Carr)
DECLARE SUB KEYTEST (LOWERLIMIT, UPPERLIMIT)
DECLARE FUNCTION MULTIKEY (KEYNUM)

DECLARE SUB HardwareCheck
DECLARE SUB FlameWareLogo
DECLARE SUB CricketMenu

DIM SHARED Pal&(0 TO 255)                       ' Palette setup (GAME.PAL)

' Engine change, using Aaron Severn's idea, instead. This actually lets me
' scroll the screen, and optimizes the code, which works just the same.

DIM SHARED tile(22, 63)                 ' Sets of 8x8 tiles instead of 12x12
DIM SHARED fglay&(4861)                 ' Foreground layer
DIM SHARED bglay&(4861)                 ' Background layer
'DIM SHARED splay&(4861)                ' Sprite layer (implementing soon)

LoadPal                                 ' Get the palette.

' Set up the tiles, and the demo sprite planes in memory.
FOR j = 0 TO 22
  FOR i = 0 TO 63
    READ tile(j, i)
  NEXT
NEXT

FOR i = 0 TO 4861: READ fglay&(i): NEXT
FOR i = 0 TO 4861: READ bglay&(i): NEXT
'FOR i = 0 TO 4861: READ splay&(i): NEXT

SCREEN 13                               ' Switch to 320x200x256 colors (13h).

HardwareCheck                           ' Calls the GoFLAME! screen
FlameWareLogo                           ' Calls "FlameWare Presents"

CricketMenu                             ' For the demo, just the demo logo.

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
DO WHILE NOT MULTIKEY(0)               ' This fixes a bug where the game will
LOOP                                   ' keep going past the title screen if
				       ' the player presses any key while the
				       ' FlameWare logo is on the screen.
LET TimeIsNow! = TIMER
DO              ' EDIT: Unconditional loop, instead of waiting for CHR$(13).
LET ClockCheck! = TIMER
LET WhatIsTime! = ClockCheck! - TimeIsNow!
IF WhatIsTime! >= .75 THEN
  IF FlashColor THEN LET FlashColor = 0 ELSE LET FlashColor = 1
  IF FlashColor = 0 THEN
    DRAW "B M116,138"
    DRAW "C0"
    Font "PRESS ENTER"
  ELSE
    DRAW "B M116,138"
    DRAW "C15"
    Font "PRESS ENTER"
  END IF
LET TimeIsNow! = TIMER
LET ClockCheck! = TIMER
LET WhatIsTime! = 0
END IF
IF MULTIKEY(28) THEN EXIT DO
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
'DO WHILE WhatIsTime! < 3! AND NOT INKEY$ = CHR$(13)
DO WHILE WhatIsTime! < 3! AND NOT MULTIKEY(28)
LET ClockCheck! = TIMER
LET WhatIsTime! = ClockCheck! - TimeIsNow!
'IF INKEY$ = CHR$(13) THEN EXIT DO
IF MULTIKEY(28) THEN EXIT DO
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

'Define Cricket's coordinates, for detecting sprite collision later on.
'LET CricketUL% = 0
'LET CricketUR% = 20
'LET CricketDL% = 160
'LET CricketDR% = 192
LET CricketX = 0
LET CricketY1 = 10
LET CricketY2 = 11

' Time to draw the sprite planes! There's two so far, but I plan on adding a
' third, at some point. It'll either be a second foreground plane, or it'll
' be the player sprite plane. I haven't decided, yet.
LoopStart:
DEF SEG = &HA000

hScroll = 0
vScroll = 0

' See if I can't do some MMC3-style side-scrolling...
' Placed before "WAIT &H3DA" because if I don't, this flickers like crazy.
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
  
DO
  verticalVal = vScroll

  fgtileX = hScroll \ 8                'Calculate all original values
  fgtileY = verticalVal \ 8            'fg = foreground, bg = background
  fgElem& = fgtileY * 221 + fgtileX     'sp = sprite plane

  bgtileX = hScroll \ 8
  bgtileY = verticalVal \ 8
  bgElem& = bgtileY * 221 + bgtileX

  'sptileX = hScroll \ 8
  'sptileY = verticalVal \ 8
  'spElem& = sptileY * 221 + sptileX

  spriteX = hScroll MOD 8
  spriteY = vScroll MOD 8
  spriteElem = spriteY * 8 + spriteX

  WAIT &H3DA, 8                         'Wait for vertical retrace
  
  ' The two FOR..NEXT loops loop through the visible screen which is the box
  ' from (24, 1) to (199, 320).
  FOR screenY = 24 TO 199

    ' Inside the second loop, the offset will only be increased by 1 each
    ' time through, so it's pointless to keep on recalculating it.  If we
    ' calculate it once for each y loop and then just add 1 inside the x
    ' loop we can speed things up.
    offset& = screenY * 320&

    FOR screenX = 1 TO 320

      ' Use the faster PSET.  The color location of the pixel in the tile
      ' array has been precalculated, we get the tile number from the map
      ' and then take the specific pixel.  The offset is increased by 1
      ' each time through the loop to move horizontally across the screen.
      ' Additional edit: for a sort of translucency effect, the background
      ' layer is drawn first, and if any pixel in the foreground or sprite
      ' layers is set to 0 (black), it will be skipped over, so it doesn't
      ' overwrite any pixels on the background layer. Hopefully this means we
      ' can use parallax scrolling later on.
      'LOCATE 1, 1: PRINT "POS:" + STR$(bgElem&) + "  BG:" + STR$(tile(bglay&(bgElem&), spriteElem)) + "  FG:" + STR$(tile(fglay&(fgElem&), spriteElem)) + "  OBJ:"
      POKE offset&, tile(bglay&(bgElem&), spriteElem)
      IF tile(fglay&(fgElem&), spriteElem) > 0 THEN
	POKE offset&, tile(fglay&(fgElem&), spriteElem)
      END IF
      offset& = offset& + 1

      ' Move over one pixel in the sprite, if we've moved on to the next tile
      ' then spriteElem will be a multiple of 8 (we move through pixels 0 to
      ' 7 and then when we hit 8, the width of the tile, we've moved on to
      ' the next tile).  So spriteElem is knocked back to the first pixel of
      ' the row and we move 1 forward along the map.
      spriteElem = spriteElem + 1
      IF spriteElem MOD 8 = 0 THEN
	spriteElem = spriteElem - 8
	fgElem& = fgElem& + 1
	bgElem& = bgElem& + 1
      END IF
    NEXT

    ' Recalculate the map element now that we've moved down one row in the
    ' sprite, and possibly down one row on the map.  VerticalVal keeps track
    ' of the vertical motion down the map.
    verticalVal = verticalVal + 1
    fgtileY = verticalVal \ 8
    bgtileY = verticalVal \ 8
    fgElem& = fgtileY * 221 + fgtileX
    bgElem& = bgtileY * 221 + bgtileX

    ' Recalculate the sprite element now that we've moved down one row in
    ' the sprite.  If we've moved down to the next tile, knock the spriteY
    ' value back to the top of the tile.
    spriteY = spriteY + 1
    IF spriteY = 8 THEN spriteY = 0
    spriteElem = spriteY * 8 + spriteX
  NEXT

' Arrow key trapping is funny, because rather than just giving you one char
' code, like most of the other keys do, it gives you two: a blank character
' (which you grab by putting CHR$(0) in front), and another letter.

' This is the only problem with this: putting a really small timeout for the
' movement animation makes the soon-to-be-Cricket tiles move slower than I'd
' like. If I increase the timeout, you can actually see him "run", but if I
' get rid of the timeout altogether, he just zooms like he's hyped on coffee.
IF MULTIKEY(75) THEN     'Left arrow key
'  IF CricketX > 0 THEN           'Is Cricket all the way to the left side?
    IF hScroll > 0 THEN LET hScroll = hScroll - 1
    LET CricketMove = CricketX * 16
'    LET CricketX = CricketX - 1
    LET CricketStop = CricketX * 16
'    DO UNTIL CricketMove = CricketStop  'Here's our super slow timeout code.
'      LET TimeIsNow! = TIMER
'      DO WHILE ! < .00009
'        LET PressCheck$ = INKEY$
'        LET ClockCheck! = TIMER
'        LET WhatIsTime! = ClockCheck! - TimeIsNow!
'        'Theoretically, if the player holds down the left arrow key, this
'        'should speed up the animation, so Cricket will start to "run".
'        '"Should" and "will" are two different things... doesn't seem to work.
'        IF PressCheck$ = CHR$(0) + "K" THEN IF ! >= .000001 THEN EXIT DO
'        IF WhatIsTime! >= .00009 THEN EXIT DO
'      LOOP
'      LET WhatIsTime! = 0
      LET CricketMove = CricketMove - 1
'      GOSUB RedrawMapData  ' Redraw the screen, and change Cricket's position.
'      PUT (CricketMove, (CricketY1 * 16) - 8), Wall%, PSET
'      PUT (CricketMove, (CricketY2 * 16) - 8), Wall%, PSET
'    LOOP
    'GOSUB RedrawMapData
    'PUT (CricketX * 16, CricketY1 * 16), Wall%, PSET
    'PUT (CricketX * 16, CricketY2 * 16), Wall%, PSET
'  END IF
ELSEIF MULTIKEY(77) THEN 'Right arrow key
'  IF CricketX < 19 THEN         ' Is Cricket all the way to the right side?
    IF hScroll < 1448 THEN LET hScroll = hScroll + 1
    CricketMove = CricketX * 16
'    CricketX = CricketX + 1
    CricketStop = CricketX * 16
'    DO UNTIL CricketMove = CricketStop
'      TimeIsNow! = TIMER
'      DO UNTIL ! > .00009
'        PressCheck$ = INKEY$
'        ClockCheck! = TIMER
'        ! = ClockCheck! - TimeIsNow!
'        'Theoretically, if the player holds down the right arrow key, this
'        'should speed up the animation, so Cricket will start to "run".
'       '"Should" and "will" are two different things. Doesn't seem to work.
'        IF PressCheck$ = CHR$(0) + "M" THEN IF ! >= .000001 THEN EXIT DO
'        IF ! >= .00009 THEN EXIT DO
'      LOOP
'      ! = 0
      CricketMove = CricketMove + 1
'      GOSUB RedrawMapData  ' Redraw the screen, and change Cricket's position.
'      PUT (CricketMove, (CricketY1 * 16) - 8), Wall%, PSET
'      PUT (CricketMove, (CricketY2 * 16) - 8), Wall%, PSET
'    LOOP
    'LET CricketX = CricketX + 1
    'GOSUB RedrawMapData
    'PUT (CricketX * 16, CricketY1 * 16), Wall%, PSET
    'PUT (CricketX * 16, CricketY2 * 16), Wall%, PSET
'  END IF
ELSEIF MULTIKEY(29) THEN        ' CTRL should be jump, and ALT will be attack.
A = 0 'I wanna get the movement speed down, first. Then I'll make this work.
ELSEIF MULTIKEY(1) THEN END
END IF
LOOP

SYSTEM                  ' To stop it from running farther than it should.

'$INCLUDE: 'SSENDEMO.BI'

REM $STATIC
' For the engine demo, it'll probably just be a working logo, since I plan on
' replacing it when I get closer to an actual beta. I do plan on putting a
' main menu in here, at some point.
SUB CricketMenu
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
DRAW "B M20,65"
DRAW "C15"
Font "COPYRIGHT 2011-2012 FLAMEWARE CORPORATION"
DRAW "B M70,84"
Font "PROGRAMMED BY ROBBIE BRUCE"
DRAW "B M68,94"
Font "GRAPHICS BY MOBIUS AQUARIAN"
DRAW "B M44,104"
Font "MUSIC AND SOUND BY KAMION SHANORIKO"
FadePal 1, Pal&()

END SUB

REM $DYNAMIC
'This was also from Mysterious Song by DarkDread. I needed it for fade out and
'fade in techniques.
SUB FadePal (Direction%, PaletteArray&())

    IF Direction% = 0 THEN
	'*** Fade palette down ***

	'Break down all 256 colours into their RGB values and
	'calculate how ÿÿch e
	DIM RGBval!(0 TO 255, 0 TO 2)
	DIM SubVal!(0 TO 255, 0 TO 2)
	FOR N = 0 TO 255
	    C& = PaletteArray&(N)
	    B = C& \ 65536: C& = C& - B * 65536
	    g = C& \ 256: C& = C& - g * 256
	    r = C&
	    RGBval!(N, 0) = r
	    RGBval!(N, 1) = g
	    RGBval!(N, 2) = B
	    SubVal!(N, 0) = r / 63
	    SubVal!(N, 1) = g / 63
	    SubVal!(N, 2) = B / 63
	NEXT N

	'Fade down all 256 colours in 63 steps.
	FOR j = 1 TO 63
	    'Calculate new faded down RGB values.
	    FOR N = 0 TO 255
		RGBval!(N, 0) = RGBval!(N, 0) - SubVal!(N, 0)
		RGBval!(N, 1) = RGBval!(N, 1) - SubVal!(N, 1)
		RGBval!(N, 2) = RGBval!(N, 2) - SubVal!(N, 2)
	    NEXT N

	    'Write faded down colours directly to the video card.
	    WAIT &H3DA, &H8, &H8: WAIT &H3DA, &H8
	    FOR N = 0 TO 255
		OUT &H3C8, N             'Select attribute.
		OUT &H3C9, RGBval!(N, 0) 'Write red.
		OUT &H3C9, RGBval!(N, 1) 'Write green.
		OUT &H3C9, RGBval!(N, 2) 'Write blue.
	    NEXT N
	NEXT j
    ELSE
	'*** Fade palette up ***

	'Break down all 256 colours into their RGB values and
	'calculate how much each will need fading up by.
	DIM RGBval!(0 TO 255, 0 TO 2)
	DIM AddVal!(0 TO 255, 0 TO 2)
	FOR N = 0 TO 255
	    C& = PaletteArray&(N)
	    B = C& \ 65536: C& = C& - B * 65536
	    g = C& \ 256: C& = C& - g * 256
	    r = C&
	    AddVal!(N, 0) = r / 63
	    AddVal!(N, 1) = g / 63
	    AddVal!(N, 2) = B / 63
	NEXT N

	'Fade up all 256 colours in 63 steps.
	FOR j = 1 TO 63
	    'Calculate new faded up RGB values.
	    FOR N = 0 TO 255
		RGBval!(N, 0) = RGBval!(N, 0) + AddVal!(N, 0)
		RGBval!(N, 1) = RGBval!(N, 1) + AddVal!(N, 1)
		RGBval!(N, 2) = RGBval!(N, 2) + AddVal!(N, 2)
	    NEXT N

	    'Write faded up colours directly to the video card.
	    WAIT &H3DA, &H8, &H8: WAIT &H3DA, &H8
	    FOR N = 0 TO 255
		OUT &H3C8, N             'Select attribute.
		OUT &H3C9, RGBval!(N, 0) 'Write red.
		OUT &H3C9, RGBval!(N, 1) 'Write green.
		OUT &H3C9, RGBval!(N, 2) 'Write blue.
	    NEXT N
	NEXT j
    END IF

END SUB

REM $STATIC
SUB FlameWareLogo
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

END SUB

SUB HardwareCheck

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
DRAW "B L121"
DRAW "B D10"
DRAW "C2"
Font "ENGINE TEST DEMO V0.1.1"
DRAW "B L195"
DRAW "B D35"
DRAW "C0"
Font "COPYRIGHT 2011-2012 FLAMEWARE CORPORATION"

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
IF MULTIKEY(28) THEN EXIT DO
LOOP WHILE NOT MULTIKEY(28)
PAINT (25, 195), 0, 2
DRAW "B M95,196"
DRAW "C15"
Font "STARTING THE GAME"
FadePal 0, Pal&()

END SUB

REM $DYNAMIC
'From "Multikey Function Update" by Joe Huber, Jr. (with parts by Eric Carr)
SUB KEYTEST (LOWERLIMIT, UPPERLIMIT)
DO
X = 1
Y = 1

 FOR i = LOWERLIMIT TO UPPERLIMIT
  TEST = MULTIKEY(i)
  LOCATE Y, X
  PRINT TEST; i
 
  IF Y < 23 THEN
   Y = Y + 1
  ELSE
   Y = 1
   X = X + 7
  END IF
 NEXT i

LOOP WHILE MULTIKEY(1) = 0
END

END SUB

'LoadPal was from Mysterious Song by DarkDread.
SUB LoadPal

    FileNum = FREEFILE
    OPEN "game.pal" FOR BINARY AS #FileNum
    FOR N = 0 TO 255
	GET #FileNum, , Colour&
	Pal&(N) = Colour&
    NEXT N
    CLOSE #FileNum

    DIM RGBval(0 TO 255, 0 TO 2)
    FOR N = 0 TO 255
	C& = Pal&(N)
	B = C& \ 65536: C& = C& - B * 65536
	g = C& \ 256: C& = C& - g * 256
	r = C&
	RGBval(N, 0) = r
	RGBval(N, 1) = g
	RGBval(N, 2) = B
    NEXT N

    WAIT &H3DA, &H8, &H8: WAIT &H3DA, &H8
    FOR N = 0 TO 255
	OUT &H3C8, N
	OUT &H3C9, RGBval(N, 0)
	OUT &H3C9, RGBval(N, 1)
	OUT &H3C9, RGBval(N, 2)
    NEXT N
    ERASE RGBval

END SUB

'From "Multikey Function Update" by Joe Huber, Jr. (with parts by Eric Carr)
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

