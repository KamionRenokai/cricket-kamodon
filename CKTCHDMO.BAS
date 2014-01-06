' Cricket Kamodon (The Pseudo-Hero)
' Copyright 2011-2012 Robbie Bruce (Kamion R. Shanoriko)
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
' EDIT: Well, maybe I can port this to Windows, without changing much of the
'       code... I discovered a library called DS4QB++ that'll let me keep the
'       game mainly the same, but change the music and sound to use Windows'
'       DirectSound system, instead (of BWSB).
'
' TODO: Speed up Cricket, implement "jumping", and maybe more of the level
'       layout of SMB1 World 1-1 for testing purposes? Now let's code some
'       levels, like Generitica 1, for example!
' #2    Let's design some clouds, a sun, and some shadowed buildings for the
'       background layer, then either add more to the level, or think about
'       how we're going to implement the sprite plane, or object layer. I
'       actually thought having two tile planes was gonna slow things down,
'       but surprisingly, it practically didn't miss a beat. How about three?
' #3    The idea worked! Hopefully it does what I hope it does, and reads up
'       to five sets of 22 lines (110 total), so either I, or future modders,
'       can put in some pretty long levels. I'm debating on whether or not to
'       actually put in more than one level for the tech demo (like world 1-1
'       through 1-4), to show the engine reading more than one pair of files.
'       But I should finish 1-1 and consider working on the sprite layer. I'm
'       also hoping to find a way to speed up the sprite rendering engine.
' #4    I'm actually pretty close to finishing world 1-1. I just need to make
'       the rest of the level complete "house", and extend the map by just a
'       few more columns of sprites, so I can finish everything else out. I
'       was thinking about having a door on the far right that opens, after
'       Cricket triggers the end-of-level slot machine, which he will then
'       walk out of, afterward. I'm also tempted to put random things in the
'       level complete "house", like a family watching TV, a married couple
'       sleeping in their bed, and stuff like that, just for kicks. They
'       wouldn't be surprised; they'd just watch Cricket trigger the slot
'       machine, then walk out. I should also move the level loading commands
'       to the "starting level" display, like how they'll be in the full game,
'       and move the "engine tech demo information" screen to either before
'       hardware detection or the FlameWare logo, or on the game logo screen.
' #5 >> Swap out the tile set (this uses 10x10 sprites instead of 8x8 sprites
'       like I was using before) by drawing and using GET to fill the extras
'       in, then we just DEF SEG = VARSEG(Tiles(0, 0)) to get the exact spot
'       and BSAVE our own file, like WLDXL1TS.KMD! Ka-POW! That way, I can
'       give each individual level its own set of tiles, so I don't have to
'       worry about running out of memory by loading all of Generitica's tiles
'       at once, or all of Banzikor's tiles all at once, for example. The way
'       Tyler did this was pretty ingenious. I never thought of loading
'       sprites that way. The only fun thing would be trying to retrofit the
'       engine to use 8x8 sprites, like I was using before. I get the feeling
'       some of the sprites the old engine was using might look a bit weird,
'       once I adapt them to 10x10. I wonder if I should name the tile editor
'       "Theodore", just as a pun on John Romero's tile editor, "TED"...
'       I think it might be easier for me to make a Windows port of Cricket,
'       just by modifying the existing code slightly, without changing it from
'       QuickBASIC to C++ or something similar. I found a cool library called
'       DS4QB++, which uses the Microsoft DirectSound library from DirectX.
'       That would give the game the ability to play back music and sound
'       within Windows NT, 2000 and XP (and hopefully Vista and 7, as well).
'       The best part is, I don't have to convert the songs I compose in
'       Impulse/Schism Tracker to S3M (ScreamTracker) format first! ...well,
'       at least not for the Windows port. The DOS port will still use the
'       Bells, Whistles and Sound Boards library (hopefully), but I'm gonna
'       try my hardest to make sure that the music and sound effects from the
'       DOS port sound exactly the same as, or at least incredibly similar to,
'       the Impulse/Schism Tracker songs and WAV sound effects from the
'       Windows port, in terms of sound playback quality. Development should
'       be able to pick back up now, getting back up to the same speed as
'       before, hopefully.
'
' LIST OF GAME FIXES I'VE DONE SO FAR (FOR REFERENCE):
' 0.1.0 Ran out of space and memory using DATA and READ statements, and would
'       even get a "Module level code too large" message.
' FIX:  Moved the hardware detection screen, FlameWare logo drawing commands
'       and the Cricket Kamodon logo (and probably menu, soon) into SUBs, and
'       moved the DATA commands into two files (WLDXL1BG.KMD for background,
'       and WLDXL1FG.KMD for foreground; I'll follow the same method for the
'       actual game (WLDAL1FG.KMD, WLDBL4FG.KMD, etc.)
' 0.1.1 Background and foreground data file reader would get stuck at the end
'       of the first 22 lines in the data files (after 1024 characters), and
'       would start redrawing the level from the beginning, one row of sprites
'       higher, about 3/4ths into the level.
' FIX:  Reprogram the reader to grab every single line from the data files,
'       one after the other, and combine each line (1st and 23rd, 2nd and
'       24th, etc.), before reading each set of numbers between the commas,
'       and filling the foreground and background data arrays. This also makes
'       level design a bit more modder-friendly, since this game will be open-
'       source, after all. The current code supports 110 rows of sprites.
' 0.1.2 Drawing engine was updating the screen too slowly. Gohan would give
'       me around 22-24 FPS normally, Goku gave me about the same, and
'       sometimes slightly faster (26-27 FPS, give or take). Only Piccolo
'       gave me anything even remotely close to 60 FPS, and that's a first-
'       generation Core i7 with 12GB of RAM.
' FIX:  Change the engine to one that runs smoothly (anywhere between 30 and
'       45 FPS under DOSBox at default settings of 3000 cycles). It was a bit
'       tricky to figure out the drawing settings and everything at first,
'       though. Now we get to re-adapt the sprites from the old engine to the
'       new system, especially since it uses 10x10 sprites, rather than 8x8.
DEFINT A-Z
'$DYNAMIC

' From SUB90FNT, "Sub '90", my font inspired by early '90s PC game fonts
DECLARE SUB Font (Sentence$)
' From the source code to the PC version of "Mysterious Song" by DarkDread
DECLARE SUB loadpal ()                          'Get the palette.
' From "Multikey Function Update" by Joe Huber, Jr. (with parts by Eric Carr)
DECLARE SUB KEYTEST (LOWERLIMIT, UPPERLIMIT)
DECLARE FUNCTION MULTIKEY (KEYNUM)
' From ID91FONT, my hand-copied version of the font Id used in its '90s games
DECLARE SUB GameText (Sentence$)
' From "Pixel-by-Pixel Scroller" by Tyler Spaulding
DECLARE SUB paltwo (colour%, R%, G%, b%)

DECLARE SUB Clip (tilenum%, x1%, y1%, x2%, y2%, xput%, yput%)
DECLARE SUB clrkbuffer ()

' Extra SUBs, to make as much room for the level data as possible.
DECLARE SUB HardwareCheck ()
DECLARE SUB FlameWareLogo ()
DECLARE SUB CricketMenu ()

' For the engine tech demo, this is the initial loading screen.
PRINT "ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป"
PRINT "บ CRICKET KAMODON!          ** GAME ENGINE TECH DEMO **          Version 0.2.0 บ"
PRINT "บ Copyright 2011, 2012 Kamion Shanoriko.                       Coda ??.??.2012 บ"
PRINT "ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
PRINT
PRINT "Initializing...";

TYPE rgb
	R AS INTEGER
	G AS INTEGER
	b AS INTEGER
END TYPE
DIM SHARED pal&(0 TO 255)                       ' Palette setup (GAME.PAL)
DIM SHARED Red%(255), Green%(255), Blue%(255)   ' New palette fade technique
DIM SHARED colors(255) AS rgb
DIM SHARED scroll(32001) AS INTEGER
DIM SHARED Cliptile(0) AS INTEGER
DIM SHARED RGBval AS STRING * 1

' Now for the map tiles. I gotta swap in my own custom tiles one of these
' times, then make a tile editor for future modders.

COLOR 15, 0: PRINT "OK!"
COLOR 7, 0: PRINT "Loading sprites...";
OPEN "NEWGAME.PUT" FOR BINARY AS 3
Ints = (LOF(3) - 7) / 2
CLOSE #3
'Number sequence: Reserved (use 0 only), animation frame (0-15), sprite.
DIM SHARED Tiles(51, 15, Ints / 52 - 1) AS INTEGER
DEF SEG = VARSEG(Tiles(0, 0, 0))
BLOAD "NEWGAME.PUT", 0
DEF SEG
COLOR 15, 0: PRINT "OK!"

' Set up the palette (Mysterious Song code) and map data.

COLOR 7, 0: PRINT "Setting up the graphics...": PRINT
ON ERROR GOTO MissingPal                ' Error if GAME.PAL is missing
loadpal                                 ' Get the palette.
GOSUB FadeInit                          ' Set up FadeIn and FadeOut

ON ERROR GOTO ThisIsNotVGA              ' Error if we can't do screen 13
SCREEN 13                               ' Switch to 320x200x256 colors (13h).

ON ERROR GOTO 0
' Manually overwriting the default tiles, before I BSAVE them
DIM tile(52, 63)                 ' Sets of 8x8 tiles instead of 12x12
FOR j = 0 TO 52
  FOR I = 0 TO 63
    READ tile(j, I)
  NEXT
NEXT

FOR a = 0 TO 52
  c = 0: d = 0
  FOR b = 0 TO 63
    PSET (c, d), tile(a, b)
    c = c + 1
    IF c = 8 THEN c = 0: d = d + 1
  NEXT
  FOR e = 0 TO 15
    GET (0, 0)-(7, 7), Tiles(0, e, a)
  NEXT
  CLS
NEXT

ON ERROR GOTO 0                         ' Kill the error-trapping subroutine
' While I'm programming and debugging, I'm going to skip the hardware
' detection and FlameWare logo screens. I don't really need them yet, anyway.
'CALL HardwareCheck                      ' Engine demo info and hardware check
'GOSUB FadeOut                           ' Fade the screen to black
'CALL FlameWareLogo                      ' FLAMEWARE PRESENTS screen
'GOSUB FadeIn                            ' Fade the screen from black
'CheckThat! = TIMER
'DO
'WhatClock! = TIMER
'IF MULTIKEY(28) THEN EXIT DO
'LOOP UNTIL (WhatClock! - CheckThat!) = 5!
'GOSUB FadeOut
'CLS

CricketMenu                             ' For the demo, just the demo logo.
'GOSUB FadeIn
' This captures the number of seconds that have passed since the logo came
' up, and waits for the player to press the ENTER key. For every 3/4ths of a
' second the player doesn't press anything, the words "PRESS ENTER" will
' flash on and off the screen.
' TimeIsNow! represents the original starting time.
' ClockCheck! is the additional starting time, which keeps counting.
' WhatIsTime! is the difference between ClockCheck! and TimeIsNow!. This is
'             how the time-passing statements work.
' FlashColor is either a 0 (flash off) or a 1 (flash on), and changes every
'            time WhatIsTime! (which is ClockCheck! minus TimeIsNow!) hits .75
'            seconds.

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

LINE (0, 53)-(320, 145), 0, BF
DRAW "B M70,65"
DRAW "C15"
Font "START FROM WHICH LEVEL?"
DRAW "B M110,84"
Font "MUSHROOM 1"
DRAW "B M110,94"
Font "MUSHROOM 2"
DRAW "B M110,104"
Font "MUSHROOM 3"
DRAW "B M110,114"
Font "MUSHROOM 4"
DRAW "B M69,138"
Font "NEITHER -- EXIT THE DEMO"

LET FlashColor = 1
LET Highlight = 1
LET TimeIsNow! = TIMER
DO              ' EDIT: Unconditional loop, instead of waiting for CHR$(13).
LET ClockCheck! = TIMER
LET WhatIsTime! = ClockCheck! - TimeIsNow!
IF WhatIsTime! >= .75 THEN
  IF FlashColor THEN LET FlashColor = 0 ELSE LET FlashColor = 1
  IF FlashColor = 0 THEN
    IF Highlight = 1 THEN DRAW "B M110,84": DRAW "C0": Font "MUSHROOM 1"
    IF Highlight = 2 THEN DRAW "B M110,94": DRAW "C0": Font "MUSHROOM 2"
    IF Highlight = 3 THEN DRAW "B M110,104": DRAW "C0": Font "MUSHROOM 3"
    IF Highlight = 4 THEN DRAW "B M110,114": DRAW "C0": Font "MUSHROOM 4"
    IF Highlight = 5 THEN DRAW "B M69,138": DRAW "C0": Font "NEITHER -- EXIT THE DEMO"
  ELSE
    IF Highlight = 1 THEN DRAW "B M110,84": DRAW "C15": Font "MUSHROOM 1"
    IF Highlight = 2 THEN DRAW "B M110,94": DRAW "C15": Font "MUSHROOM 2"
    IF Highlight = 3 THEN DRAW "B M110,104": DRAW "C15": Font "MUSHROOM 3"
    IF Highlight = 4 THEN DRAW "B M110,114": DRAW "C15": Font "MUSHROOM 4"
    IF Highlight = 5 THEN DRAW "B M69,138": DRAW "C15": Font "NEITHER -- EXIT THE DEMO"
  END IF
LET TimeIsNow! = TIMER
LET ClockCheck! = TIMER
LET WhatIsTime! = 0
END IF
IF MULTIKEY(72) THEN    ' Up
  Highlight = Highlight - 1
  IF Highlight = 0 THEN Highlight = 5
  FlashColor = 1
  DRAW "C15 BM110,84": Font "MUSHROOM 1"
  DRAW "C15 BM110,94": Font "MUSHROOM 2"
  DRAW "C15 BM110,104": Font "MUSHROOM 3"
  DRAW "C15 BM110,114": Font "MUSHROOM 4"
  DRAW "C15 BM69,138": Font "NEITHER -- EXIT THE DEMO"
END IF
IF MULTIKEY(80) THEN ' Down
  Highlight = Highlight + 1
  IF Highlight = 6 THEN Highlight = 1
  FlashColor = 1
  DRAW "C15 BM110,84": Font "MUSHROOM 1"
  DRAW "C15 BM110,94": Font "MUSHROOM 2"
  DRAW "C15 BM110,104": Font "MUSHROOM 3"
  DRAW "C15 BM110,114": Font "MUSHROOM 4"
  DRAW "C15 BM69,138": Font "NEITHER -- EXIT THE DEMO"
END IF
IF MULTIKEY(28) THEN ' ENTER
  IF Highlight = 1 THEN EXIT DO
  IF Highlight = 2 THEN THAT = 0
  IF Highlight = 3 THEN THAT = 0
  IF Highlight = 4 THEN THAT = 0
  IF Highlight = 5 THEN CLS : SCREEN 0: WIDTH 80, 25: SYSTEM
END IF
LOOP

' This triggers a fade-out, and starts drawing the scoreboard at the top of
' the screen, along with information about the test level.
GOSUB FadeOut
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
DRAW "B M293,8"
Font "250"
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
GOSUB FadeIn

' Load the contents of our test map (interestingly, this works practically
' just like the system I was using before. Maybe it won't be hard to in-
' corporate this new engine after all...)
OPEN "WLDXL1BG.KMD" FOR INPUT AS 4      ' Mushroom 1 background map
INPUT #4, xs, ys                        ' Get size of the map (in tiles)
DIM SHARED Map(ys, xs) AS INTEGER       ' Get ready to set up said map
FOR y = 1 TO ys
  FOR x = 1 TO xs
    INPUT #4, Map(y, x)
  NEXT x
NEXT y

' With the "GO, CRICKET!" message on the screen, using the
' TimeIsNow!/ClockCheck!/WhatIsTime! method mentioned earlier, the game either
' waits for the player to press ENTER, or keeps going, after 3 seconds.
LET TimeIsNow! = TIMER
DO WHILE WhatIsTime! < 3! AND NOT MULTIKEY(28)
LET ClockCheck! = TIMER
LET WhatIsTime! = ClockCheck! - TimeIsNow!
IF MULTIKEY(28) THEN EXIT DO
LOOP

' This command sequence uses the time-trapping method again, but uses it to
' gradually fade the level information screen out, color by color, from color
' 31 (white) to 16 (black), stepping backwards. If you're wondering why I
' didn't just call FadeOut, it's because I wanted to only fade out the text,
' and keep the scoreboard on the screen. FadeOut doesn't pick and choose.
LET TimeIsNow! = TIMER
FOR I% = 31 TO 16 STEP -1
LET ColorMode$ = "C" + STR$(I%)
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

'x = 8 * INT(xs / 2)                     ' Starting "x" point
'y = 8 * INT(ys / 2)                     ' Starting "y" point
x = 0
y = 0
screenx1 = 20                           ' Maximum number of columns
screeny1 = 11                           ' Maximum number of sprites per column
screenx2 = 2 * screenx1                 ' Maximum "x" point
screeny2 = 2 * screeny1                 ' Maximum "y" point

'Define Cricket's coordinates, for detecting sprite collision later on.
'TODO: Redefine these for collision detection, after I finally get the sprite
'      plane (and any other extra foreground/background planes) implemented.
'LET CricketUL% = 0
'LET CricketUR% = 20
'LET CricketDL% = 160
'LET CricketDR% = 192
LET CricketX = 0
LET CricketY1 = 10
LET CricketY2 = 11
'LET tick = 250                          ' Set the timer
'ON TIMER(1) GOSUB ClockChange           ' Call a subroutine every second
'TIMER ON                                ' to change the countdown clock

' The techniques this engine uses are interesting, given QuickBASIC's
' capabilities. You can switch the colors a tile uses, and even mirror
' certain tiles, kinda like an NES/SNES/Genesis game. It's almost as if
' I showed the original game to John Carmack, and he said "I bet I can
' make a better engine", to which I would answer, "I know you can, and if
' you'd like to, feel free". lol
FOR x2 = 1 TO screenx2
  FOR y2 = 1 TO screeny2
    IF x2 = 1 THEN PUT (0, 16 + 8 * y2), Tiles(0, 0, Map(y2, x2)), PSET
    IF x2 > 1 THEN PUT (8 * x2 - 8, 16 + 8 * y2), Tiles(0, 0, Map(y2, x2)), PSET
  NEXT y2
NEXT x2

scrollstep = 1                          ' Would this be like "frameskip"?
					' If this is, I could use this for
					' speeding up Cricket when he gets
					' invincibility.
f = 0
DO
'Separate loop, which I know I'll have to remove for tile animations later on
IF MULTIKEY(75) AND f > 0 THEN     'Left arrow key
  f = f - scrollstep                    ' Move by "scrollstep"
  x2 = INT(x / 8)                       ' Center the "x" tile
  'x2 = INT(f / 8)
  GET (0, 24)-(319 - scrollstep, 199), scroll
  PUT (0 + scrollstep, 24), scroll, PSET       ' Scroll the screen left
  IF y / 8 = INT(y / 8) THEN          ' Did I do it right?
    FOR y2 = 1 TO screeny2
      y1 = y / 8 + y2 - screeny1 - 1
      tile% = Map(y2, x2)                    ' Or maybe x2 - screenx1 - 1?
      'tile% = Map(y2, f + 1)                ' Or maybe even f - 1?
      Clip tile%, x - 8 * x2, 0, x - 8 * x2 - 1 + scrollstep, 9, 0, 16 + 8 * y2
    NEXT
  ELSE
  y1 = INT(y / 8) - screeny1
  y3 = y - 8 * INT(y / 8)
  tile% = Map(y2, x2 - screenx1)
  'tile% = Map(y2, f + 1)
  Clip tile%, x - 8 * x2, y3, x - 8 * x2 - 1 + scrollstep, 9, 0, 24
  FOR y2 = 2 TO screeny2                ' Every tile that ISN'T on the corner
    y1 = INT(y / 8) + y2 - screeny1 - 1
    tile% = Map(y2, x2 - screenx1)
    'tile% = Map(y2, f + 1)
    Clip tile%, x - 8 * x2, 0, x - 8 * x2 - 1 + scrollstep, 9, 0, 16 + 8 * y2 - y3
  NEXT
  y1 = INT(y / 8) + screeny2 - screeny1
  y3 = y - 8 * INT(y / 8)
  tile% = Map(y2, x2 - screenx1)
  'tile% = Map(y2, f + 1)
  Clip tile%, x - 8 * x2, 0, x - 8 * x2 - 1 + scrollstep, y3 - 1, 0, 200 - y3
  END IF
END IF
IF MULTIKEY(77) AND x / 8 < xs - screenx1 THEN 'Right arrow key
  x2 = INT(x / 8)
  f = f + scrollstep
  GET (0 + scrollstep, 24)-(319, 199), scroll
  PUT (0, 24), scroll, PSET
  IF y / 8 = INT(y / 8) THEN
    FOR y2 = 1 TO screeny2
      y1 = y / 8 + y2 - screeny1 - 1
      tile% = Map(y2, x2 + screenx1 + 1)
      'tile% = Map(y2, f + screenx1 + 1)
      Clip tile%, x - 8 * x2, 0, x - 8 * x2 - 1 + scrollstep, 9, 319 - scrollstep, 16 + 8 * y2
    NEXT
  ELSE
  y1 = INT(y / 8) - screeny1
  y3 = y - 8 * INT(y / 8)
  tile% = Map(y2, x2 + screenx1)
  'tile% = Map(y2, f + screenx1)
  Clip tile%, x - 8 * x2, y3, x - 8 * x2 - 1 + scrollstep, 9, 319 - scrollstep, 24
  FOR y2 = 2 TO screeny2
    y1 = INT(y / 8) + y2 - screeny1 - 1
    tile% = Map(y2, x2 + screenx1 + 1)
    Clip tile%, x - 8 * x2, 0, x - 8 * x2 - 1 + scrollstep, 9, 319 - scrollstep, 16 + 8 * y2 - y3
  NEXT
  y1 = INT(y / 8) + screeny2 - screeny1
  y3 = y - 8 * INT(y / 8)
  tile% = Map(y2, x2 + screenx1 + 1)
  Clip tile%, x - 8 * x2, 0, x - 8 * x2 - 1 + scrollstep, y3 - 1, 319 - scrollstep, 200 - y3
  END IF
x = x + scrollstep
END IF
IF MULTIKEY(29) THEN        ' CTRL should be jump, and ALT will be attack.
a = 0 'I wanna get the sprite layers, monsters and everything else in, first.
END IF
IF MULTIKEY(56) THEN        ' Holding spot. ALT will be attack.
a = 0
END IF
IF MULTIKEY(1) THEN END     ' ESC ends our fancy little simulation.
LOOP

SYSTEM                  ' To stop it from running farther than it should.

' Setting up the tiles, super fast, before the game starts.
' NOTE: You HAVE to put the sprites BEFORE the FG/BG/sprite layers!
' I'm wondering if I'll have to move the sprites into a data file, as well...

' Tile 0: Ground
DATA 10,10,10,10,10,10,10,10
DATA 2,2,10,10,2,16,16,16
DATA 245,245,2,10,2,16,245,245
DATA 244,244,16,2,16,244,244,244
DATA 243,243,243,16,243,243,243,243
DATA 242,242,242,242,242,242,242,242
DATA 241,241,241,241,241,241,241,241
DATA 240,240,240,240,240,240,240,240

' Tile 1: Sky
DATA 175,175,175,175,175,175,175,175
DATA 175,175,175,175,175,175,175,175
DATA 175,175,175,175,175,175,175,175
DATA 175,175,175,175,175,175,175,175
DATA 175,175,175,175,175,175,175,175
DATA 175,175,175,175,175,175,175,175
DATA 175,175,175,175,175,175,175,175
DATA 175,175,175,175,175,175,175,175

' Tile 2: Tree Trunk (L)
DATA 16,197,198,199,200,200,200,200
DATA 16,197,198,199,200,195,200,200
DATA 16,197,198,199,200,200,200,200
DATA 16,197,198,199,200,200,200,200
DATA 16,197,198,199,200,200,195,200
DATA 16,197,198,199,200,200,200,200
DATA 16,197,198,199,200,200,200,195
DATA 16,197,198,199,200,200,200,200

' Tile 3: Tree Trunk (R)
DATA 200,200,200,200,199,198,197,16
DATA 200,200,195,200,199,198,197,16
DATA 200,200,200,200,199,198,197,16
DATA 200,200,200,200,199,198,197,16
DATA 200,200,200,195,199,198,197,16
DATA 200,200,200,200,199,198,197,16
DATA 200,200,200,200,199,198,197,16
DATA 195,200,200,200,199,198,197,16

' Tile 4: Random Box (TL)
DATA 56,56,56,56,56,56,56,56
DATA 56,57,57,57,57,57,57,57
DATA 56,57,58,58,58,58,58,58
DATA 56,57,58,59,59,59,59,59
DATA 56,57,58,59,60,60,60,60
DATA 56,57,58,59,60,61,61,61
DATA 56,57,58,59,60,61,62,62
DATA 56,57,58,59,60,61,62,63

' Tile 5: Random Box (BL)
DATA 56,57,58,59,60,61,62,63
DATA 56,57,58,59,60,61,62,62
DATA 56,57,58,59,60,61,61,61
DATA 56,57,58,59,60,60,60,60
DATA 56,57,58,59,59,59,59,59
DATA 56,57,58,58,58,58,58,58
DATA 56,57,57,57,57,57,57,57
DATA 56,56,56,56,56,56,56,56

' Tile 6: Random Box (TR)
DATA 56,56,56,56,56,56,56,56
DATA 57,57,57,57,57,57,57,56
DATA 58,58,58,58,58,58,57,56
DATA 59,59,59,59,59,58,57,56
DATA 60,60,60,60,59,58,57,56
DATA 61,61,61,60,59,58,57,56
DATA 62,62,61,60,59,58,57,56
DATA 63,62,61,60,59,58,57,56

' Tile 7: Random Box (BR)
DATA 63,62,61,60,59,58,57,56
DATA 62,62,61,60,59,58,57,56
DATA 61,61,61,60,59,58,57,56
DATA 60,60,60,60,59,58,57,56
DATA 59,59,59,59,59,58,57,56
DATA 58,58,58,58,58,58,57,56
DATA 57,57,57,57,57,57,57,56
DATA 56,56,56,56,56,56,56,56

' Tile 8: Broken Random Box (TL)
DATA 36,36,36,36,36,36,36,36
DATA 36,37,37,37,37,37,37,37
DATA 36,37,38,38,38,38,38,38
DATA 36,37,38,39,39,39,39,39
DATA 36,37,38,39,40,40,40,40
DATA 36,37,38,39,40,41,41,41
DATA 36,37,38,39,40,41,42,42
DATA 36,37,38,39,40,41,42,43

' Tile 9: Broken Random Box (BL)
DATA 36,37,38,39,40,41,42,43
DATA 36,37,38,39,40,41,42,42
DATA 36,37,38,39,40,41,41,41
DATA 36,37,38,39,40,40,40,40
DATA 36,37,38,39,39,39,39,39
DATA 36,37,38,38,38,38,38,38
DATA 36,37,37,37,37,37,37,37
DATA 36,36,36,36,36,36,36,36

' Tile 10: Broken Random Box (TR)
DATA 36,36,36,36,36,36,36,36
DATA 37,37,37,37,37,37,37,36
DATA 38,38,38,38,38,38,37,36
DATA 39,39,39,39,39,38,37,36
DATA 40,40,40,40,39,38,37,36
DATA 41,41,41,40,39,38,37,36
DATA 42,42,41,40,39,38,37,36
DATA 43,42,41,40,39,38,37,36

' Tile 11: Broken Random Box (BR)
DATA 43,42,41,40,39,38,37,36
DATA 42,42,41,40,39,38,37,36
DATA 41,41,41,40,39,38,37,36
DATA 40,40,40,40,39,38,37,36
DATA 39,39,39,39,39,38,37,36
DATA 38,38,38,38,38,38,37,36
DATA 37,37,37,37,37,37,37,36
DATA 36,36,36,36,36,36,36,36

' Tile 12: Blank Space (for translucency effects on the foreground layer)
DATA 0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0

' Tile 13: Valley Background (first layer)
DATA 69,70,69,70,69,70,69,70
DATA 70,69,70,69,70,69,70,69
DATA 69,70,69,70,69,70,69,70
DATA 70,69,70,69,70,69,70,69
DATA 69,70,69,70,69,70,69,70
DATA 70,69,70,69,70,69,70,69
DATA 69,70,69,70,69,70,69,70
DATA 70,69,70,69,70,69,70,69

' Tile 14: Valley Background (second layer)
DATA 70,71,70,71,70,71,70,71
DATA 71,70,71,70,71,70,71,70
DATA 70,71,70,71,70,71,70,71
DATA 71,70,71,70,71,70,71,70
DATA 70,71,70,71,70,71,70,71
DATA 71,70,71,70,71,70,71,70
DATA 70,71,70,71,70,71,70,71
DATA 71,70,71,70,71,70,71,70

' Tile 15: Valley Background (third layer)
DATA 72,71,72,71,72,71,72,71
DATA 71,72,71,72,71,72,71,72
DATA 72,71,72,71,72,71,72,71
DATA 71,72,71,72,71,72,71,72
DATA 72,71,72,71,72,71,72,71
DATA 71,72,71,72,71,72,71,72
DATA 72,71,72,71,72,71,72,71
DATA 71,72,71,72,71,72,71,72

' Tile 16: Valley Background (fourth layer)
DATA 73,72,73,72,73,72,73,72
DATA 72,73,72,73,72,73,72,73
DATA 73,72,73,72,73,72,73,72
DATA 72,73,72,73,72,73,72,73
DATA 73,72,73,72,73,72,73,72
DATA 72,73,72,73,72,73,72,73
DATA 73,72,73,72,73,72,73,72
DATA 72,73,72,73,72,73,72,73

' Tile 17: Valley Background (fifth layer)
DATA 74,73,74,73,74,73,74,73
DATA 73,74,73,74,73,74,73,74
DATA 74,73,74,73,74,73,74,73
DATA 73,74,73,74,73,74,73,74
DATA 74,73,74,73,74,73,74,73
DATA 73,74,73,74,73,74,73,74
DATA 74,73,74,73,74,73,74,73
DATA 73,74,73,74,73,74,73,74

' Tile 18: Valley Background (sixth layer)
DATA 75,74,75,74,75,74,75,74
DATA 74,75,74,75,74,75,74,75
DATA 75,74,75,74,75,74,75,74
DATA 74,75,74,75,74,75,74,75
DATA 75,74,75,74,75,74,75,74
DATA 74,75,74,75,74,75,74,75
DATA 75,74,75,74,75,74,75,74
DATA 74,75,74,75,74,75,74,75

' Tile 19: Valley Background (seventh layer)
DATA 76,75,76,75,76,75,76,75
DATA 75,76,75,76,75,76,75,76
DATA 76,75,76,75,76,75,76,75
DATA 75,76,75,76,75,76,75,76
DATA 76,75,76,75,76,75,76,75
DATA 75,76,75,76,75,76,75,76
DATA 76,75,76,75,76,75,76,75
DATA 75,76,75,76,75,76,75,76

' Tile 20: Valley Background (eighth layer)
DATA 77,76,77,76,77,76,77,76
DATA 76,77,76,77,76,77,76,77
DATA 77,76,77,76,77,76,77,76
DATA 76,77,76,77,76,77,76,77
DATA 77,76,77,76,77,76,77,76
DATA 76,77,76,77,76,77,76,77
DATA 77,76,77,76,77,76,77,76
DATA 76,77,76,77,76,77,76,77

' Tile 21: Valley Background (ninth layer)
DATA 78,77,78,77,78,77,78,77
DATA 77,78,77,78,77,78,77,78
DATA 78,77,78,77,78,77,78,77
DATA 77,78,77,78,77,78,77,78
DATA 78,77,78,77,78,77,78,77
DATA 77,78,77,78,77,78,77,78
DATA 78,77,78,77,78,77,78,77
DATA 77,78,77,78,77,78,77,78

' Tile 22: Valley Background (tenth layer)
DATA 78,79,78,79,78,79,78,79
DATA 79,78,79,78,79,78,79,78
DATA 78,79,78,79,78,79,78,79
DATA 79,78,79,78,79,78,79,78
DATA 78,79,78,79,78,79,78,79
DATA 79,78,79,78,79,78,79,78
DATA 78,79,78,79,78,79,78,79
DATA 79,78,79,78,79,78,79,78

' Tile 23: Illusion of a Black Hole (just for the tech demo, maybe)
DATA 169,169,169,169,169,169,169,169
DATA 168,168,168,168,168,168,168,168
DATA 167,167,167,167,167,167,167,167
DATA 166,166,166,166,166,166,166,166
DATA 165,165,165,165,165,165,165,165
DATA 164,164,164,164,164,164,164,164
DATA 163,163,163,163,163,163,163,163
DATA 162,162,162,162,162,162,162,162

' Tile 24: Brick Block (Super Mario Bros. style, just for the tech demo)
DATA 15,15,15,15,15,15,15,15
DATA 06,06,06,06,06,06,06,16
DATA 06,06,06,06,06,06,06,16
DATA 16,16,16,16,16,16,16,16
DATA 06,06,06,16,06,06,06,06
DATA 06,06,06,16,06,06,06,06
DATA 06,06,06,16,06,06,06,06
DATA 16,16,16,16,16,16,16,16

' Tile 25: Brick Block #2 (just like the above one, for the tech demo only)
DATA 06,06,06,06,06,06,06,16
DATA 06,06,06,06,06,06,06,16
DATA 06,06,06,06,06,06,06,16
DATA 16,16,16,16,16,16,16,16
DATA 06,06,06,16,06,06,06,06
DATA 06,06,06,16,06,06,06,06
DATA 06,06,06,16,06,06,06,06
DATA 16,16,16,16,16,16,16,16

' Tile 26: Level Complete Wall Rope Holding Stake (for lack of a better name)
DATA 000,143,143,143,143,143,016,127
DATA 000,143,143,143,143,016,127,143
DATA 000,016,016,016,016,127,143,127
DATA 000,127,127,127,127,143,127,016
DATA 000,143,143,143,143,127,016,000
DATA 000,127,127,127,127,016,000,000
DATA 000,016,016,016,016,143,000,000
DATA 000,143,140,137,140,143,000,000

' Tile 27: Level Complete Wall-Holding Rope (for lack of a better name)
DATA 000,000,000,000,143,127,143,127
DATA 000,000,000,143,127,143,127,143
DATA 000,000,143,127,143,127,143,127
DATA 000,143,127,143,127,143,127,143
DATA 000,127,143,127,143,127,143,000
DATA 127,143,127,143,127,143,000,000
DATA 143,127,143,127,143,000,000,000
DATA 127,143,127,000,000,000,000,000

' Tile 28: Level Complete Wall Rope-Holding Ring (for lack of a better name)
DATA 000,000,000,008,008,000,000,000
DATA 000,000,008,007,007,008,000,000
DATA 000,008,007,000,000,007,008,000
DATA 008,143,000,000,000,000,007,008
DATA 008,127,143,000,000,000,007,008
DATA 000,143,127,143,000,007,008,000
DATA 143,127,143,007,007,008,000,000
DATA 127,143,127,008,008,000,000,000

' Tile 29: Top of the Level Complete Wall
DATA 109,109,109,109,109,109,109,016
DATA 016,109,109,109,109,109,109,109
DATA 104,016,109,109,109,109,109,109
DATA 104,104,016,109,109,109,109,109
DATA 104,104,104,016,016,016,016,016
DATA 104,104,104,016,109,109,109,109
DATA 104,104,104,016,109,109,109,109
DATA 104,104,104,016,109,109,109,109

' Tile 30: Level Complete Wall Repeating Edge
DATA 104,104,104,016,109,109,109,109
DATA 104,104,104,016,109,109,109,109
DATA 104,104,104,016,109,109,109,109
DATA 104,104,104,016,109,109,109,109
DATA 104,104,104,016,109,109,109,109
DATA 104,104,104,016,109,109,109,109
DATA 104,104,104,016,109,109,109,109
DATA 104,104,104,016,109,109,109,109

' Tile 31: Bottom of the Level Complete Wall
DATA 104,104,104,016,109,109,109,109
DATA 104,104,104,016,109,109,109,109
DATA 104,104,104,016,109,109,109,109
DATA 104,104,104,016,109,109,109,109
DATA 016,104,104,016,109,109,109,109
DATA 000,016,104,016,109,109,109,109
DATA 000,000,016,016,109,109,109,109
DATA 000,000,000,016,016,016,016,016

' Tile 32: Top Right Side of the Level Complete Wall
DATA 109,109,109,109,109,109,109,109
DATA 016,109,109,109,109,109,109,109
DATA 109,016,109,109,109,109,109,109
DATA 109,109,016,109,109,109,109,109
DATA 016,016,016,016,016,016,016,016
DATA 109,109,109,016,106,106,106,106
DATA 109,109,109,016,106,106,106,106
DATA 109,109,109,016,106,106,106,106

' Tile 33: Level Complete Wall Repeating Edge (Side)
DATA 109,109,109,016,106,106,106,106
DATA 109,109,109,016,106,106,106,106
DATA 109,109,109,016,106,106,106,106
DATA 109,109,109,016,106,106,106,106
DATA 109,109,109,016,106,106,106,106
DATA 109,109,109,016,106,106,106,106
DATA 109,109,109,016,106,106,106,106
DATA 109,109,109,016,106,106,106,106

' Tile 34: Bottom Right Side of the Level Complete Wall (1/3)
DATA 109,109,109,016,106,106,106,106
DATA 109,109,109,016,106,106,106,106
DATA 109,109,109,016,106,106,106,106
DATA 109,109,109,016,106,106,106,106
DATA 109,109,109,016,106,106,106,106
DATA 109,109,109,016,106,106,106,106
DATA 109,109,109,016,106,106,106,106
DATA 109,109,109,016,016,016,016,016

' Tile 35: Bottom Right Side of the Level Complete Wall (2/3)
DATA 109,109,109,016,109,109,109,109
DATA 109,109,109,016,109,109,109,109
DATA 109,109,109,016,109,109,109,109
DATA 109,109,109,016,109,109,109,109
DATA 109,109,109,016,109,109,109,109
DATA 109,109,109,016,109,109,109,109
DATA 109,109,109,016,109,109,109,109
DATA 016,016,016,016,016,016,016,016

' Tile 36: Extreme Top Left Side of the Level Complete Wall
DATA 104,103,104,104,104,104,104,016
DATA 103,104,104,104,104,104,104,104
DATA 104,103,104,104,104,104,104,104
DATA 103,104,104,104,104,104,104,104
DATA 104,103,104,104,104,104,104,104
DATA 103,104,104,104,104,104,104,104
DATA 104,103,104,104,104,104,104,104
DATA 103,104,104,104,104,104,104,104

' Tile 37: Repeating Extreme Top Left Side of the Level Complete Wall (1/3)
DATA 104,103,104,104,104,104,104,104
DATA 103,104,104,104,104,104,104,104
DATA 104,103,104,104,104,104,104,104
DATA 103,104,104,104,104,104,104,104
DATA 104,103,104,104,104,104,104,104
DATA 103,104,104,104,104,104,104,104
DATA 104,103,104,104,104,104,104,104
DATA 103,104,104,104,104,104,104,104

' Tile 38: Extreme Almost Bottom Left Side of the Level Complete Wall (1/5)
DATA 104,103,104,104,104,104,104,104
DATA 103,104,104,104,104,104,104,104
DATA 104,103,104,104,104,104,104,104
DATA 103,104,104,104,104,104,104,104
DATA 016,103,104,104,104,104,104,104
DATA 070,016,104,104,104,104,104,104
DATA 071,070,016,104,104,104,104,104
DATA 070,071,070,016,104,104,104,104

' Tile 39: Extreme Bottom Left Side of the Level Complete Wall (1/3)
DATA 069,070,069,070,016,104,104,104
DATA 070,069,070,069,070,016,104,104
DATA 069,070,069,070,069,070,016,104
DATA 070,069,070,069,070,069,070,016
DATA 069,070,069,070,069,070,069,070
DATA 070,069,070,069,070,069,070,069
DATA 069,070,069,070,069,070,069,070
DATA 070,069,070,069,070,069,070,069

' Tile 40: Extreme Left Repeating Side of the Level Complete Wall
DATA 104,103,104,103,104,103,104,103
DATA 103,104,103,104,103,104,103,104
DATA 104,103,104,103,104,103,104,103
DATA 103,104,103,104,103,104,103,104
DATA 104,103,104,103,104,103,104,103
DATA 103,104,103,104,103,104,103,104
DATA 104,103,104,103,104,103,104,103
DATA 103,104,103,104,103,104,103,104

' Tile 41: Extreme Almost Bottom Left Side of the Level Complete Wall (2/5)
DATA 104,103,104,103,104,103,104,103
DATA 103,104,103,104,103,104,103,104
DATA 104,103,104,103,104,103,104,103
DATA 103,104,103,104,103,104,103,104
DATA 016,103,104,103,104,103,104,103
DATA 071,016,103,104,103,104,103,104
DATA 072,071,016,103,104,103,104,103
DATA 071,072,071,016,103,104,103,104

' Tile 42: Extreme Almost Bottom Left Side of the Level Complete Wall (3/5)
DATA 071,070,071,070,016,103,104,103
DATA 070,071,070,071,070,016,103,104
DATA 071,070,071,070,071,070,016,103
DATA 070,071,070,071,070,071,070,016
DATA 071,070,071,070,071,070,071,070
DATA 070,071,070,071,070,071,070,071
DATA 071,070,071,070,071,070,071,070
DATA 070,071,070,071,070,071,070,071

' Tile 43: Repeating Left-Most Side of the Level Complete Wall
DATA 016,103,104,103,104,103,104,103
DATA 016,104,103,104,103,104,103,104
DATA 016,103,104,103,104,103,104,103
DATA 016,104,103,104,103,104,103,104
DATA 016,103,104,103,104,103,104,103
DATA 016,104,103,104,103,104,103,104
DATA 016,103,104,103,104,103,104,103
DATA 016,104,103,104,103,104,103,104

' Tile 44: Extreme Almost Bottom Left Side of the Level Complete Wall (4/5)
DATA 016,103,104,103,104,103,104,103
DATA 016,104,103,104,103,104,103,104
DATA 016,103,104,103,104,103,104,103
DATA 016,104,103,104,103,104,103,104
DATA 016,103,104,103,104,103,104,103
DATA 072,016,103,104,103,104,103,104
DATA 073,072,016,103,104,103,104,103
DATA 072,073,072,016,103,104,103,104

' Tile 45: Extreme Almost Bottom Left Side of the Level Complete Wall (5/5)
DATA 072,071,072,071,016,103,104,103
DATA 071,072,071,072,071,016,103,104
DATA 072,071,072,071,072,071,016,103
DATA 071,072,071,072,071,072,071,016
DATA 072,071,072,071,072,071,072,071
DATA 071,072,071,072,071,072,071,072
DATA 072,071,072,071,072,071,072,071
DATA 071,072,071,072,071,072,071,072

' Tile 46: Level Complete "House" Ceiling Tile
DATA 109,109,109,109,109,109,109,109
DATA 109,109,109,109,109,109,109,109
DATA 109,109,109,109,109,109,109,109
DATA 109,109,109,109,109,109,109,109
DATA 016,016,016,016,016,016,016,016
DATA 106,106,106,106,106,106,106,106
DATA 106,106,106,106,106,106,106,106
DATA 106,106,106,106,106,106,106,106

' Tile 47: Level Complete "House" Shadowed Wall Repeating Tile
DATA 106,106,106,106,106,106,106,106
DATA 106,106,106,106,106,106,106,106
DATA 106,106,106,106,106,106,106,106
DATA 106,106,106,106,106,106,106,106
DATA 106,106,106,106,106,106,106,106
DATA 106,106,106,106,106,106,106,106
DATA 106,106,106,106,106,106,106,106
DATA 106,106,106,106,106,106,106,106

' Tile 48: Level Complete "House" Flooring (1/2)
DATA 106,106,106,106,106,106,106,106
DATA 106,106,106,106,106,106,106,106
DATA 106,106,106,106,106,106,106,106
DATA 106,106,106,106,106,106,106,106
DATA 106,106,106,106,106,106,106,106
DATA 106,106,106,106,106,106,106,106
DATA 106,106,106,106,106,106,106,106
DATA 016,016,016,016,016,016,016,016

' Tile 49: Level Complete "House" Flooring (2/2)
DATA 109,109,109,109,109,109,109,109
DATA 109,109,109,109,109,109,109,109
DATA 109,109,109,109,109,109,109,109
DATA 109,109,109,109,109,109,109,109
DATA 109,109,109,109,109,109,109,109
DATA 109,109,109,109,109,109,109,109
DATA 109,109,109,109,109,109,109,109
DATA 016,016,016,016,016,016,016,016

' Tile 50: Level Complete "House" Room Divider Wall Upper Left Tile
DATA 109,109,109,109,109,109,109,109
DATA 016,109,109,109,109,109,109,109
DATA 109,016,109,109,109,109,109,109
DATA 109,109,016,109,109,109,109,109
DATA 016,016,016,016,016,016,016,016
DATA 104,104,104,016,109,109,109,109
DATA 104,104,104,016,109,109,109,109
DATA 104,104,104,016,109,109,109,109

' Tile 51: Level Complete "House" Room Divider Wall Left Repeating Tile
DATA 104,104,104,016,109,109,109,109
DATA 104,104,104,016,109,109,109,109
DATA 104,104,104,016,109,109,109,109
DATA 104,104,104,016,109,109,109,109
DATA 104,104,104,016,109,109,109,109
DATA 104,104,104,016,109,109,109,109
DATA 104,104,104,016,109,109,109,109
DATA 104,104,104,016,109,109,109,109

' Tile 52: Level Complete "House" Room Divider Wall Left Shaded Tile
DATA 104,104,104,104,104,104,104,104
DATA 104,104,104,104,104,104,104,104
DATA 104,104,104,104,104,104,104,104
DATA 104,104,104,104,104,104,104,104
DATA 104,104,104,104,104,104,104,104
DATA 104,104,104,104,104,104,104,104
DATA 104,104,104,104,104,104,104,104
DATA 104,104,104,104,104,104,104,104

'Now for the error subroutines, for if the game won't run properly.
MissingPal:             ' GAME.PAL is missing.
COLOR 31, 0: PRINT "Oh, no! I can't find GAME.PAL!"
PRINT : COLOR 7, 0
PRINT "Unfortunately, this is a file I need to run Cricket Kamodon, and since I can't"
PRINT "find it, I can't start the game."
PRINT
COLOR 31, 0: PRINT "** HOW DO I FIX THIS?"
PRINT : COLOR 7, 0
PRINT "You just need to find "; : COLOR 15, 0: PRINT "GAME.PAL"; : COLOR 7, 0: PRINT ", and put it into the same"
PRINT "folder as the rest of the game. If you can't find it, you might have to reinstall"
PRINT "Cricket Kamodon."
PRINT
SYSTEM 1

MissingDemoFG:          ' WLDXL1FG.KMD (tech demo only) is missing.
SCREEN 0: WIDTH 80, 25
CLOSE #1
'KILL "WLDXL1FG.KMD"    ' Disaster purposes. Can't do that during the demo!
COLOR 31, 0: PRINT "Oh, no! I can't find WLDXL1FG.KMD!"
PRINT : COLOR 7, 0
PRINT "Unfortunately, this is a file I need to run Cricket Kamodon, and since I can't"
PRINT "find it, I can't start the game."
PRINT
COLOR 31, 0: PRINT "** HOW DO I FIX THIS?"
PRINT : COLOR 7, 0
PRINT "You just need to find "; : COLOR 15, 0: PRINT "WLDXL1FG.KMD"; : COLOR 7, 0: PRINT ", and put it into the same"
PRINT "folder as the rest of the game. If you can't find it, you might have to reinstall"
PRINT "Cricket Kamodon."
PRINT
SYSTEM 1

MissingDemoBG:          ' WLDXL1BG.KMD (tech demo only) is missing.
SCREEN 0: WIDTH 80, 25
CLOSE #2
'KILL "WLDXL1BG.KMD"    ' Disaster purposes. Can't do that during the demo!
COLOR 31, 0: PRINT "Oh, no! I can't find WLDXL1BG.KMD!"
PRINT : COLOR 7, 0
PRINT "Unfortunately, this is a file I need to run Cricket Kamodon, and since I can't"
PRINT "find it, I can't start the game."
PRINT
COLOR 31, 0: PRINT "** HOW DO I FIX THIS?"
PRINT : COLOR 7, 0
PRINT "You just need to find "; : COLOR 15, 0: PRINT "WLDXL1BG.KMD"; : COLOR 7, 0: PRINT ", and put it into the same"
PRINT "folder as the rest of the game. If you can't find it, you might have to reinstall"
PRINT "Cricket Kamodon."
PRINT
SYSTEM 1

ThisIsNotVGA:           ' Just in case the computer doesn't have a VGA card.
COLOR 31, 0: PRINT "Oh, no! This computer doesn't have a VGA video card!"
PRINT : COLOR 7, 0
PRINT "Unfortunately, Cricket Kamodon uses 256 colors, and only a VGA video card (or better)"
PRINT "can support that."
PRINT
COLOR 31, 0: PRINT "** HOW DO I FIX THIS?"
PRINT : COLOR 7, 0
PRINT "You'd have to upgrade the video card in your computer to a VGA card. And chances are,"
PRINT "you'll also have to upgrade your computer's monitor as well, since VGA cards hook up"
PRINT "to different monitors than EGA, CGA or monochrome video cards."
PRINT
PRINT "Buuuuut..."
PRINT
PRINT "You could always leave Kamion a message about this, and ask him to make an EGA version"
PRINT "of Cricket Kamodon. Who knows? If enough people ask him, he just might!"
SYSTEM 1

ClockChange: ' Change the time on the clock subroutine (once every second)
' This might make it flicker on certain systems, but I'll work something out.
TIMER OFF
tick = tick - 1
IF tick < 60 THEN LINE (286, 0)-(320, 11), 4, BF ELSE LINE (286, 0)-(320, 11), 10, BF
DRAW "B M293,8"
IF tick < 10 THEN
  DRAW "C15": Font "00" + LTRIM$(STR$(tick))
ELSEIF tick < 100 THEN
  IF tick < 60 THEN DRAW "C15" ELSE DRAW "C0"
  Font "0" + LTRIM$(STR$(tick))
ELSE
  DRAW "C0": Font LTRIM$(STR$(tick))
END IF
IF tick > 0 THEN ON TIMER(1) GOSUB ClockChange: TIMER ON
RETURN

FadeInit:
FOR I% = 0 TO 255
  OUT &H3C7, I%
  Red%(I%) = INP(&H3C9)
  Green%(I%) = INP(&H3C9)
  Blue%(I%) = INP(&H3C9)
NEXT I%
RETURN

FadeIn:
FOR I% = 0 TO 63
  FOR x% = 0 TO 255
    OUT &H3C7, x%
    R% = INP(&H3C9) + 1: IF R% > Red%(x%) THEN R% = Red%(x%)
    G% = INP(&H3C9) + 1: IF G% > Green%(x%) THEN G% = Green%(x%)
    b% = INP(&H3C9) + 1: IF b% > Blue%(x%) THEN b% = Blue%(x%)
    OUT &H3C8, x%
    OUT &H3C9, R%
    OUT &H3C9, G%
    OUT &H3C9, b%
  NEXT x%
NEXT I%
RETURN

FadeOut:
FOR I% = 0 TO 63
  FOR x% = 0 TO 255
    OUT &H3C7, x%
    R% = INP(&H3C9) - 1: IF R% < 0 THEN R% = 0
    G% = INP(&H3C9) - 1: IF G% < 0 THEN G% = 0
    b% = INP(&H3C9) - 1: IF b% < 0 THEN b% = 0
    OUT &H3C8, x%
    OUT &H3C9, R%
    OUT &H3C9, G%
    OUT &H3C9, b%
  NEXT x%
NEXT I%
RETURN

SUB Clip (tilenum%, x1%, y1%, x2%, y2%, xput%, yput%)

xsize = Tiles(0, 0, tilenum%) / 8                  'x size of array
ysize = Tiles(1, 0, tilenum%)                      'y size of array

FOR Clipy = y1% TO y2%                          'every y
	FOR Clipx = x1% TO x2%                  'every x
		clipspace! = 2 + (xsize * Clipy + Clipx) / 2    'space of tile
		cliparray = INT(clipspace!)                     'that's used
		IF clipspace! = cliparray THEN  'first pixel stored in space
			colour = Tiles(cliparray, 0, tilenum%) - 256 * INT(Tiles(cliparray, 0, tilenum%) / 256)
			'color used
		ELSE
			colour = INT(Tiles(cliparray, 0, tilenum%) / 256)
			'color used
		END IF
		PSET (xput% + Clipx - x1, yput% + Clipy - y1), colour
	NEXT
NEXT

END SUB

REM $STATIC
' For the engine demo, it'll probably just be a working logo, since I plan on
' replacing it when I get closer to an actual beta. I do plan on putting a
' main menu in here, at some point.
SUB CricketMenu
'Let's see if I can't draw up a demo logo for our friend, master Kamodon...
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
FOR I% = 1 TO 13
DRAW "BD1BR1"
DRAW "D2"
NEXT
DRAW "BD1BR1"
DRAW "U40"
DRAW "R12"
DRAW "D48"
DRAW "L14"
FOR I% = 1 TO 13
DRAW "BU1BL1"
DRAW "U2"
NEXT
DRAW "BU1BL1"
DRAW "D40"
DRAW "L10"
DRAW "BU2BR2"
DRAW "P10,10"
DRAW "B M24,65"
DRAW "C15"
Font "COPYRIGHT 2011-2012 FLAMEWARE CORPORATION"
DRAW "B M32,84"
Font "SIDESCROLLING ENGINE BY TYLER SPAULDING"
DRAW "B M6,94"
Font "GAME CODE AND MODDED ENGINE BY KAMION SHANORIKO"
DRAW "B M22,104"
Font "LEVELS INSPIRED BY S. MIYAMOTO AND T. TEZUKA"
DRAW "B M109,114"
Font "2013 OR BUST!!!"
END SUB

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
END SUB

SUB HardwareCheck

DRAW "C2"
DRAW "B M0,55"
DRAW "U55"
DRAW "R319"
DRAW "D55"
DRAW "L319"
PAINT STEP(3, -3), 10, 2
DRAW "B U5"
DRAW "B R92"
DRAW "C0"
DRAW "B U40"
DRAW "B R12"
Font "CRICKET KAMODON"
DRAW "B L121"
DRAW "B D10"
DRAW "C2"
Font "ENGINE TEST DEMO V0.2.0"
DRAW "B L204"
DRAW "B D35"
DRAW "C0"
Font "COPYRIGHT 2011-2012 FLAMEWARE CORPORATION"

'Now for the bottom half of the loading screen...
DRAW "B M0,55"
DRAW "C2"
DRAW "D144"
DRAW "R319"
DRAW "U144"
DRAW "B D134"
DRAW "L319"
DRAW "B D7"
DRAW "B R32"
DRAW "C15"
Font "PRESS ANY KEY TO MOVE TO THE NEXT SCREEN"
DRAW "B M19,70"
DRAW "C2"
Font "THIS TECH DEMO ESSENTIALLY SHOWS OFF WHAT I"
DRAW "B M3,80"
Font "THINK THIS ENGINE IS CAPABLE OF. WHAT YOU'RE ABOUT"
DRAW "B M4,90"
Font "TO SEE IS WHAT I WOULD CALL MY ": DRAW "C10": Font "DANGEROUS DAVE IN"
DRAW "B M21,100"
Font "COPYRIGHT INFRINGEMENT": DRAW "C2": Font ", IN A SENSE. THE ONLY"
DRAW "B M30,110"
Font "DIFFERENCES WOULD BE CRICKET IN PLACE OF"
DRAW "B M8,120"
Font "DANGEROUS DAVE, AND A CLONE OF WORLD 1, LEVELS 1"
DRAW "B M20,130"
Font "THROUGH 4 FROM SUPER MARIO BROS. INSTEAD OF"
DRAW "B M7,140"
Font "WORLD 1, LEVEL 1 FROM SUPER MARIO BROS. 3. I ALSO"
DRAW "B M2,150"
Font "WANT TO POINT OUT THAT THE NEXT DEMO, AND THE FULL"
DRAW "B M2,160"
Font "RELEASE, WHEN IT DOES COME OUT, WILL HAVE AT LEAST"
DRAW "B M6,170"
Font "THE FIRST LEVEL FROM THIS DEMO, BUT A COMPLETELY"
DRAW "B M11,180"
Font "DIFFERENT LEVEL ARRANGEMENT, WITH MORE LEVELS."

DO WHILE INKEY$ = ""
LOOP

clrkbuffer
' Clear the keyboard buffer, so if you hit enter on the last screen, you won't
' automatically skip over the hardware detection screen. Hopefully.

LINE (1, 55)-(318, 185), 0, BF
PAINT (2, 197), 0, 2
DRAW "B M88,196"
DRAW "C15"
Font "CHECKING THE HARDWARE"
DRAW "B M120,55"
DRAW "C2"
DRAW "D133"
DRAW "B M120,65"
DRAW "R198"
DRAW "B M130,62"
Font "SOUND DEVICES DETECTED"
DRAW "B M130,75"
Font "SOUND BLASTER PRO 16 OR 32"
DRAW "B M130,85"
Font "PRO AUDIOSPECTRUM"
DRAW "B M130,95"
Font "GRAVIS ULTRASOUND"
DRAW "B M120,105"
DRAW "R198"
DRAW "B M130,112"
Font "AVAILABLE MEMORY"
DRAW "B M120,115"
DRAW "R198"
DRAW "B M132,125"
Font "    KB BASE         KB HIGH"
DRAW "B M120,133"
DRAW "R198"
DRAW "B M130,140"
Font "INPUT DEVICES DETECTED"
DRAW "B M120,143"
DRAW "R198"
DRAW "B M130,154"
Font "KEYBOARD"
DRAW "B M130,164"
Font "MOUSE"
DRAW "B M130,174"
Font "JOYSTICK OR GAMEPAD"

'First, here's the obvious part: there's a keyboard in this PC.
DRAW "C10"
DRAW "B M130,154"
Font "KEYBOARD"
DRAW "C2"

'Second, we check to see if a Sound Blaster card exists in the PC.
'I should see if I can find a better way to detect a sound card.
BlastEnv$ = ENVIRON$("BLASTER")
CardType% = VAL(MID$(BlastEnv$, INSTR(BlastEnv$, "T") + 1, 1))
SELECT CASE CardType%
 CASE IS = 1 OR 3
    DRAW "C10"
    DRAW "B M130,75"
    Font "SOUND BLASTER"
 CASE IS = 2 OR 4 OR 5
    DRAW "C10"
    DRAW "B M130,75"
    Font "SOUND BLASTER PRO"
 CASE 6
    DRAW "C10"
    DRAW "B M130,75"
    Font "SOUND BLASTER      "
    DRAW "B R1"
    Font "16 OR 32"
 CASE ELSE
 END SELECT
DRAW "C2"

'Third, we find out how much conventional and expanded memory is available.
LET ConvRAM = (FRE(-1) / 1024)
ON LOCAL ERROR GOTO WeHaveNoEMS
LET ExpRAM! = FRE(-3)
ON LOCAL ERROR GOTO 0
DRAW "C10"
DRAW "B M132,125"
Font "    KB BASE"
DRAW "B M130,125"
Font LTRIM$(STR$(ConvRAM))
DRAW "C10"
DRAW "B M242,125"
Font "KB HIGH"
DRAW "B M207,125"
Font LTRIM$(RTRIM$(STR$(ExpRAM!)))
GOTO SegmentFour

WeHaveNoEMS:
'This is to stop the game from crashing, if there's no expanded memory.
DRAW "C2"
DRAW "B M235,125"
Font "0KB HIGH"
DRAW "C10"
DRAW "B M130,125"
Font "      KB BASE"
DRAW "B M130,125"
Font LTRIM$(STR$(ConvRAM))
RESUME SegmentFour

'Fourth, we pause for a second, then display "LET'S START!"
SegmentFour:
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

END SUB

REM $DYNAMIC
'From "Multikey Function Update" by Joe Huber, Jr. (with parts by Eric Carr)
SUB KEYTEST (LOWERLIMIT, UPPERLIMIT)
DO
x = 1
y = 1

 FOR I = LOWERLIMIT TO UPPERLIMIT
  TEST = MULTIKEY(I)
  LOCATE y, x
  PRINT TEST; I
 
  IF y < 23 THEN
   y = y + 1
  ELSE
   y = 1
   x = x + 7
  END IF
 NEXT I

LOOP WHILE MULTIKEY(1) = 0
END

END SUB

'LoadPal was from Mysterious Song by DarkDread.
SUB loadpal

    FileNum = FREEFILE
    OPEN "game.pal" FOR BINARY AS #FileNum
    FOR n = 0 TO 255
	GET #FileNum, , colour&
	pal&(n) = colour&
    NEXT n
    CLOSE #FileNum

    DIM RGBset(0 TO 255, 0 TO 2)
    FOR n = 0 TO 255
	c& = pal&(n)
	b = c& \ 65536: c& = c& - b * 65536
	G = c& \ 256: c& = c& - G * 256
	R = c&
	RGBset(n, 0) = R
	RGBset(n, 1) = G
	RGBset(n, 2) = b
    NEXT n

    WAIT &H3DA, &H8, &H8: WAIT &H3DA, &H8
    FOR n = 0 TO 255
	OUT &H3C8, n
	OUT &H3C9, RGBset(n, 0)
	OUT &H3C9, RGBset(n, 1)
	OUT &H3C9, RGBset(n, 2)
    NEXT n
    ERASE RGBset

END SUB

'From "Multikey Function Update" by Joe Huber, Jr. (with parts by Eric Carr)
FUNCTION MULTIKEY (KEYNUM)
 STATIC FIRSTIME, KEYS(), SC(), DU()

 IF FIRSTIME = 0 THEN
  DIM KEYS(255), SC(255), DU(255)
  FOR e = 0 TO 127              '\
  SC(e) = e: DU(e) = 1          '|
  NEXT                          '|-ERIC CARR'S CODE--------------------\
  FOR e = 128 TO 255            '|                                     |
  SC(e) = e - 128: DU(e) = 0    '|                                     |
  NEXT                          '/                                     |
  FIRSTIME = -1                 '                                      |
 END IF                         '                                      |
				'                                      |
 I$ = INKEY$       ' So the keyb buffer don't get full     \routine/ \ |
 I = INP(&H60)     ' Get keyboard scan code from port 60h   \lines/  |-/
 OUT &H61, INP(&H61) OR &H82: OUT &H20, &H20       '         \!!!/   |
 KEYS(SC(I)) = DU(I) ' This says what keys are pressed        \!/    /


MULTIKEY = KEYS(KEYNUM)

END FUNCTION

