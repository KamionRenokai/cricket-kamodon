'FINISH CONVERTING WLDXL1FG/BG.KMD FOR 1,000 SPRITES IN WLDYL1FG/BG.KMD! (256)
' Cricket Kamodon (The Pseudo-Hero)
' Copyright 2011-2013 Robbie Bruce (Kamion R. Shanoriko)
'
' This and all of the files that come with this game are open-source,
' released under v3 of the GNU General Public License. A copy of the license
'ÿis readable in the LICENSE file.
'
' This is the multi-platform build, forked from the original pseudo-Windows
' build of CK Tech Demo version 0.1.3, after I moved the source code over to
' QB64 from QuickBASIC (PDS) 7.1, and re-instated Aaron Severn's side-
' scrolling engine, and DarkDread's fade-in/out code. I did make a few tweaks,
' though, mainly borrowing bits and pieces of code from the engine I was
' originally going to build for the DOS version (and still might, if I decide
' to pick that project back up). But this one definitely runs without DOS,
' and should hopefully run in OSes other than Linux (like Windows XP, Vista,
' 7, 8 and Mac OS X) without too much trouble.
'
' Thanks to QB64, I can essentially port the code to C++ if I do it right,
' but it's now multi-platform (like I said above), so the only problem I
' would have, is compiling Cricket under any version of Mac OS X. Now, if
' someone would like to do that, so that Cricket can be ported to Mac OS,
' then by all means, feel free. This is open-source, after all.
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
' #5 >> Once I finish with TODO #4, I'm also gonna need to extend the length
'       of maps that the map reader will be capable of using, because I don't
'       think any of the maps I'm gonna create for the actual game will fit
'       in two sets of 600 or so by 22 rows of sprites (for example, only
'       three-fourths of SMB1 world 1-1 fits in one set). I'm thinking I might
'       have to do either four sets, or five, at least. Theoretically, either
'       of those (and 5, preferably) will give me enough space to make some
'       pretty long game maps.
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
' 0.1.3 Iminent problem of not having enough room to put the actual engine
'       code into the main module, because of the DATA statements toward the
'       end that draw each sprite.
' FIX:  Will soon have to add another subroutine for the sprite reader, which
'       will theoretically let anyone add a lot more sprites, and give each
'       sprite up to 16 different frames of animation, through files
'       WLD?L?S0.KMD to WLD?L?SF.KMD. I figure this can make for some
'       incredibly realistic animation, if done right. This will move the
'       DATA statements that put the sprites into memory out of the source
'       code, and make room for the actual engine code.
' 0.1.4 Problem was averted with code conversion to QB64. This gets rid of
'       QuickBASIC's old "module level code too large" error, because now I
'       can put as much code in the main module as I want to, and I can even
'       use new commands, and background music/sound effects without having
'       to use a library. The only problem is, I feel like I'm quickly running
'       out of sprites I can use on one background plane.
' FIX:  Increased the maximum number of different possible sprites one can use
'       per sprite plane to 1,000, from 100. If this isn't enough for any
'       potential modders, the level data files can be switched to hexadecimal
'       format instead, theoretically allowing up to 65,536 different possible
'       sprites anyone can use on a map (no matter which sprite plane they're
'       put onto). Also added a second background plane, so I can work on
'       adding parallax scrolling.

DEFINT A-Z
'$DYNAMIC

DECLARE SUB Font (Sentence$)                    'From SUB90FNT.BAS
' From "Mysterious Song", an RPG in QuickBASIC by DarkDread
DECLARE SUB LoadPal ()                          'Get the palette.
DECLARE SUB FadePal (Direction%, PaletteArray&()) 'Make the screen fade in/out
' From "Multikey Function Update" by Joe Huber, Jr. (with parts by Eric Carr)
DECLARE SUB KEYTEST (LOWERLIMIT, UPPERLIMIT)
DECLARE FUNCTION MULTIKEY (KEYNUM)

' Extra SUBs, to make as much room for the level data as possible.
DECLARE SUB HardwareCheck ()
DECLARE SUB FlameWareLogo ()
DECLARE SUB CricketMenu ()

' For the engine tech demo, this is the initial loading screen.
_TITLE "Cricket Kamodon - v0.1.6 Tech Demo"
PRINT "ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป"
PRINT "บ CRICKET KAMODON!         ** GAME ENGINE TECH DEMO **           Version 0.1.6 บ"
PRINT "บ Copyright 2011-2013 Kamion Shanoriko.                        Coda ??.??.2013 บ"
PRINT "ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
PRINT
PRINT "Do we still have everything? ";

DIM SHARED Pal&(0 TO 255) ' Palette setup (GAME.PAL)

' Engine change, using Aaron Severn's idea, instead. This actually lets me
' scroll the screen, and optimizes the code, which works just the same.
' TODO: Implement a config file for each map, which has the max number of
'       sprites and tiles for each level. Probably WLD?L?LD.KMD, or something.

'Loading sound effects into memory; should probably properly implement later,
'maybe as a LONG array, with certain basic sounds, and others that can be
'dynamically loaded, depending on the level
ckJump& = _SNDOPEN("/media/PHANTOM/Mario_Jumping-Mike_Koenig.wav", "VOL,SYNC")
'Maybe I should have two hammer pound sounds: one for if there's nothing under
'Cricket's hammer, and another if there is (like a monster, for example).
ckSwing1& = _SNDOPEN("/media/PHANTOM/punch_or_whack_-Vladimir.wav", "VOL,SYNC")
ckSwing2& = 0
ckSwing3& = 0
ckExtraLife& = _SNDOPEN("/media/PHANTOM/GFSNESBell.ogg", "VOL,SYNC")
ckPause& = _SNDOPEN("/media/PHANTOM/SQPauseSound.ogg", "VOL,SYNC")
ckHurt& = 0
ckDeath& = _SNDOPEN("/media/PHANTOM/CKDeathSymphony.ogg", "VOL,SYNC")
ckTimeLow& = _SNDOPEN("/media/PHANTOM/TimeRunningOut.ogg", "VOL,SYNC")
ckTimeUp& = 0

COLOR 15, 0: PRINT "YES!"

ON ERROR GOTO MissingPal 'Maybe in the LoadPal sub, do _FILEEXISTS instead?
COLOR 7, 0: PRINT "Is the palette neat and pretty? ";
LoadPal ' Get the palette.
COLOR 15, 0: PRINT "YES!"

'256 per row with a one thousand sprite palette.
'TODO: Throw an error if _FILEEXISTS turns out to be false for any of these
IF _FILEEXISTS("/media/PHANTOM/QBX/CRICKET/WLDXL1FG.KMD") THEN
    OPEN "/media/PHANTOM/QBX/CRICKET/WLDXL1FG.KMD" FOR INPUT AS #1 'Mushroom 1 foreground
    COLOR 7, 0: PRINT "Checking the FG1 layer... ";
    DO UNTIL EOF(1)
        INPUT #1, num$ 'Grab a set of numbers
        grab = grab + 1 'Increase total number by one
    LOOP
    COLOR 15, 0: PRINT LTRIM$(STR$(grab / 22)) + " sprites across 22 rows."
END IF
IF _FILEEXISTS("/media/PHANTOM/QBX/CRICKET/WLDXL1B1.KMD") THEN
    OPEN "/media/PHANTOM/QBX/CRICKET/WLDXL1B1.KMD" FOR INPUT AS #2
    COLOR 7, 0: PRINT "Checking the BG1 layer... ";
    DO UNTIL EOF(2)
        INPUT #2, num$ 'Grab a set of numbers
        hold = hold + 1 'Increase total number by one
    LOOP
    COLOR 15, 0: PRINT LTRIM$(STR$(hold / 22)) + " sprites across 22 rows."
END IF
IF _FILEEXISTS("/media/PHANTOM/QBX/CRICKET/WLDXL1B2.KMD") THEN
    OPEN "/media/PHANTOM/QBX/CRICKET/WLDXL1B1.KMD" FOR INPUT AS #3
    COLOR 7, 0: PRINT "Checking the BG2 layer... ";
    DO UNTIL EOF(3)
        INPUT #3, num$ 'Grab a set of numbers
        take = take + 1 'Increase total number by one
    LOOP
    COLOR 15, 0: PRINT LTRIM$(STR$(take / 22)) + " sprites across 22 rows."
END IF
IF _FILEEXISTS("/media/PHANTOM/QBX/CRICKET/WLDXL1S0.KMD") THEN
    OPEN "/media/PHANTOM/QBX/CRICKET/WLDXL1S0.KMD" FOR INPUT AS #4
    COLOR 7, 0: PRINT "Checking the sprites (1/16)... ";
    DO UNTIL EOF(4)
        FOR n = 0 TO 63
            INPUT #4, drawsomething$
        NEXT n
        INPUT #4, end$ 'Should just say "END", the end-of-line marker
        loopz = loopz + 1
    LOOP
    COLOR 15, 0: PRINT LTRIM$(STR$(loopz)) + " total sprites."
END IF
CLOSE 1, 2, 3, 4
IF grab = hold THEN
    COLOR 15, 0: PRINT "All three sprite layers are working perfectly!"
ELSE
    COLOR 12, 0 'I should probably add statements to check "take", as well.
    IF grab > hold THEN PRINT "The FG1 layer is longer than the BG1 layer! They should be the same length!": END
    IF grab < hold THEN PRINT "The BG1 layer is longer than the FG1 layer! They should be the same length!": END
END IF

'TODO: Move these to the "loading" screen, when we start work on Subcon 2.
OPEN "/media/PHANTOM/QBX/CRICKET/WLDXL1FG.KMD" FOR INPUT AS #1
OPEN "/media/PHANTOM/QBX/CRICKET/WLDXL1B1.KMD" FOR INPUT AS #2
OPEN "/media/PHANTOM/QBX/CRICKET/WLDXL1B2.KMD" FOR INPUT AS #3
OPEN "/media/PHANTOM/QBX/CRICKET/WLDXL1S0.KMD" FOR INPUT AS #4

ON ERROR GOTO 0

' Set up the tiles, and the demo sprite planes in memory.

COLOR 7, 0: PRINT "Have we read all the sprites? ";

'TODO: Read from WLD?L?S0.KMD through WLD?L?SF.KMD to load all sixteen frames
'      for each sprite. It'd probably be a good idea to also share the code
'      between this and THE TAME, to avoid re-inventing the wheel.

'**** Tile reading method since v0.1.4 (from the DOS version's WIP engine)
DIM SHARED spritedata(0 TO (loopz - 1), 0 TO 15, 0 TO 63) AS INTEGER
DIM SHARED Background1(0 TO 21, 0 TO ((hold / 22) - 1)) AS INTEGER
DIM SHARED Background2(0 TO 21, 0 TO ((take / 22) - 1)) AS INTEGER
DIM SHARED Foreground1(0 TO 21, 0 TO ((grab / 22) - 1)) AS INTEGER
'TODO: How will I dedicate a similar array for the sprite layer?
'When I move the level data loading commands to the level loading screen,
'I should probably put sanity checks in, either before the game starts, or on
'each level loading screen for the tech demo (at least), for debugging
'purposes, if needed.
DO UNTIL EOF(4) 'Read the entire file, top to bottom
    FOR s = 0 TO (loopz - 1) 'Read every single sprite arrangement
        FOR p = 0 TO 63 'There are 64 pixels in each 8x8 sprite.
            INPUT #4, flip$ 'Grab a number
            spritedata(s, 0, p) = VAL(flip$) 'Make it an integer and save it
            'TODO: Middle number (the "0") is frame number. Implement this!
        NEXT p
        INPUT #4, flip$ 'Throw out the "END" at the end, and keep going
    NEXT s
LOOP
COLOR 15, 0: PRINT "YES!"

'**** Tile reading method for versions 0.1.2 and 0.1.3
'FOR j = 0 TO 52
'    FOR i = 0 TO 63
'        READ tile(j, i)
'    NEXT
'NEXT

'**** Tile reading method for version 0.1.1
'FOR n& = 0 TO 7969                      ' Load all of the tiles into memory
'  INPUT #1, fglay&(n&)             ' Foreground tile
'  INPUT #2, bglay&(n&)             ' Background tile
'NEXT

'**** Tile reading method for version 0.1.0
'FOR i = 0 TO 7969: 'READ fglay&(i): 'NEXT
'FOR i = 0 TO 7969: 'READ bglay&(i): 'NEXT
'FOR i = 0 TO 7969: 'READ splay&(i): 'NEXT

'I wonder if I should leave this in, even if it just says the name of the OS?
COLOR 7, 0: PRINT "What are we running under? ";
' TODO: Replace this line when you put the DS4QB++ libraries in.
'       For QB64, however, maybe I'll have to put in a fixed line that I'll
'       change, depending on the OS the game is built for.
COLOR 15, 0: PRINT "Linux!"

COLOR 7, 0: PRINT
PRINT "This tech demo essentially gives you a taste of what this engine can do."
PRINT "What you're about to see is what I would call my " + CHR$(34) + "Dangerous Dave in"
PRINT "Copyright Infringement" + CHR$(34) + ", in a sense. The only differences would be"
PRINT "Cricket in place of Dangerous Dave, and a clone of world 1-1 through 1-3"
PRINT "from Super Mario Bros. 2 instead of world 1-1 from Super Mario Bros. 3. I"
PRINT "also want to point out that the next demo, and the full release, when it"
PRINT "does come out, might have at least the first level from this demo, but it"
PRINT "will definitely have bunches of new, different levels."
PRINT
PRINT "** PRESS ANY KEY TO START."
DO: SLEEP: LOOP WHILE INKEY$ = ""

sparevar$ = INKEY$ ' Clear the keyboard buffer (hopefully)

SCREEN 13 ' Switch to 320x200x256 colors (13h). Works the best like this.
'_FULLSCREEN _SQUAREPIXELS
'IF NOT _FULLSCREEN THEN _FULLSCREEN _OFF 'Windowed mode if fullscreen fails.

ON ERROR GOTO 0 ' Kill the error-trapping subroutine
'CALL HardwareCheck ' Windows/Mac OS X/Linux versions won't need this.
CALL FlameWareLogo ' FLAMEWARE PRESENTS screen

CricketMenu ' For the demo, just the demo logo (which I should remake later).
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
DO ' EDIT: Unconditional loop, instead of waiting for CHR$(13).
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
    'IF MULTIKEY(28) THEN EXIT DO
    IF _KEYDOWN(13) THEN EXIT DO
LOOP

'So far, this doesn't clear the keyboard buffer before presenting the level
'select screen. So, when I finally start to implement Subcon 2's map, let's
'try using either _KEYHIT or _KEYDOWN to check for key presses, and maybe
'even mess with the code that makes the highlighted option flash.
DO
    sparevar& = _KEYHIT
LOOP UNTIL sparevar& = 0

'Should I let beta-testers play this tech demo, so they can test and help me
'tweak/fix the game physics? If I do, I should put in options to change the
'control keys (including left- and right-handed "presets"), along with
'anything else I should probably put in.
LINE (0, 53)-(320, 145), 0, BF
DRAW "B M80,65"
DRAW "C15"
Font "START FROM WHICH LEVEL?"
DRAW "B M126,90"
Font "SUBCON 1"
DRAW "B M126,100"
Font "SUBCON 2"
DRAW "B M126,110"
Font "SUBCON 3"
DRAW "B M79,138"
Font "NEITHER -- EXIT THE DEMO"

'Make sure nobody's holding down the ENTER key before we reach this routine
DO
    waitkey& = _KEYHIT
LOOP WHILE waitkey& <> 0 OR _KEYDOWN(13) 'Would this work, too?
'This will work good, especially when I implement Subcon 2, and Subcon 3
LET FlashColor = 1
LET Highlight = 1
LET TimeIsNow! = TIMER
DO ' EDIT: Unconditional loop, instead of waiting for CHR$(13).
    LET ClockCheck! = TIMER
    LET WhatIsTime! = ClockCheck! - TimeIsNow!
    IF WhatIsTime! >= .75 THEN
        IF FlashColor THEN LET FlashColor = 0 ELSE LET FlashColor = 1
        IF FlashColor = 0 THEN
            IF Highlight = 1 THEN DRAW "B M126,90": DRAW "C74": Font "SUBCON 1"
            IF Highlight = 2 THEN DRAW "B M126,100": DRAW "C74": Font "SUBCON 2"
            IF Highlight = 3 THEN DRAW "B M126,110": DRAW "C74": Font "SUBCON 3"
            IF Highlight = 4 THEN DRAW "B M79,138": DRAW "C74": Font "NEITHER -- EXIT THE DEMO"
        ELSE
            IF Highlight = 1 THEN DRAW "B M126,90": DRAW "C15": Font "SUBCON 1"
            IF Highlight = 2 THEN DRAW "B M126,100": DRAW "C15": Font "SUBCON 2"
            IF Highlight = 3 THEN DRAW "B M126,110": DRAW "C15": Font "SUBCON 3"
            IF Highlight = 4 THEN DRAW "B M79,138": DRAW "C15": Font "NEITHER -- EXIT THE DEMO"
        END IF
        LET TimeIsNow! = TIMER
        LET ClockCheck! = TIMER
        LET WhatIsTime! = 0
    END IF
    IF _KEYDOWN(18432) THEN ' Up arrow
        Highlight = Highlight - 1
        IF Highlight = 0 THEN Highlight = 4
        FlashColor = 0
        IF Highlight = 1 THEN DRAW "C74 BM126,90": Font "SUBCON 1" ELSE DRAW "C15 BM126,90": Font "SUBCON 1"
        IF Highlight = 2 THEN DRAW "C74 BM126,100": Font "SUBCON 2" ELSE DRAW "C15 BM126,100": Font "SUBCON 2"
        IF Highlight = 3 THEN DRAW "C74 BM126,110": Font "SUBCON 3" ELSE DRAW "C15 BM126,110": Font "SUBCON 3"
        IF Highlight = 4 THEN DRAW "C74 BM79,138": Font "NEITHER -- EXIT THE DEMO" ELSE DRAW "C15 BM79,138": Font "NEITHER -- EXIT THE DEMO"
        DO
            'Slowdown tactic, for if the user holds the key down
        LOOP WHILE _KEYDOWN(18432) 'Cancel this if they let off the key.
    END IF
    IF _KEYDOWN(20480) THEN ' Down arrow
        Highlight = Highlight + 1
        IF Highlight = 5 THEN Highlight = 1
        FlashColor = 0
        IF Highlight = 1 THEN DRAW "C74 BM126,90": Font "SUBCON 1" ELSE DRAW "C15 BM126,90": Font "SUBCON 1"
        IF Highlight = 2 THEN DRAW "C74 BM126,100": Font "SUBCON 2" ELSE DRAW "C15 BM126,100": Font "SUBCON 2"
        IF Highlight = 3 THEN DRAW "C74 BM126,110": Font "SUBCON 3" ELSE DRAW "C15 BM126,110": Font "SUBCON 3"
        IF Highlight = 4 THEN DRAW "C74 BM79,138": Font "NEITHER -- EXIT THE DEMO" ELSE DRAW "C15 BM79,138": Font "NEITHER -- EXIT THE DEMO"
        DO
            'Slowdown tactic, for if the user holds the key down
        LOOP WHILE _KEYDOWN(20480) 'Cancel this if they let off the key.
    END IF
    IF _KEYDOWN(13) THEN ' ENTER
        SELECT CASE Highlight 'A tiny bit of code optimization, here.
            CASE 1
                zone$ = "SUBCON 1"
                'TODO: Clone these for Subcon 2 and 3 when you start on them
                'OPEN "/media/PHANTOM/QBX/CRICKET/WLDXL1FG.KMD" FOR INPUT AS #1
                'OPEN "/media/PHANTOM/QBX/CRICKET/WLDXL1B1.KMD" FOR INPUT AS #2
                'OPEN "/media/PHANTOM/QBX/CRICKET/WLDXL1B2.KMD" FOR INPUT AS #3
                'OPEN "/media/PHANTOM/QBX/CRICKET/WLDXL1S0.KMD" FOR INPUT AS #4
                EXIT DO
            CASE 2
                zone$ = "SUBCON 2"
                THAT = 0
            CASE 3
                zone$ = "SUBCON 3"
                THAT = 0
            CASE 4
                _FULLSCREEN _OFF 'Switch back to windowed mode (if it's not)
                SYSTEM 'Exit the demo
        END SELECT
    END IF
LOOP

' This triggers a fade-out, and starts drawing the scoreboard at the top of
' the screen, along with information about the test level.
' Moved the statement setting "tick" to here. ON TIMER statement wasn't moved.
FadePal 0, Pal&()
LET tick = 300 ' Set the timer (300 seconds should make it easy... it's 1-1!)
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
Font zone$ 'Interchangeable string variable that displays the level name
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
Font LTRIM$(STR$(tick)) 'Should display the starting time on the clock (300)
DRAW "C15"
DRAW "B M3,20"
Font "SCORE"
DRAW "B M109,20"
DRAW "C0"
Font "0" 'TODO: This should also be a numeric integer. Maybe LONG type?
DRAW "C15"
DRAW "B M125,20"
Font "AMP" 'TODO: This should be implemented, too, along with a bar.
DRAW "C0"
DRAW "B M261,20"
Font "NORMAL"
DRAW "C15"
DRAW "B M126,80"
Font zone$
DRAW "B M116,100"
Font "3 LIVES LEFT" 'TODO: Implement a life counter, too. This WILL be needed.
DRAW "B M120,120"
Font "GO, CRICKET!"
FadePal 1, Pal&()

'Read the foreground and background maps from a file (hopefully save memory)
'Moved these here, since the actual game will load level data on this screen.

'Maybe do a quick sanity check while the loading screen is up, with the data
'of the level to be loaded, to set the "grab", "hold" and "take" variables
'for the level loading routine? Like, besides the one done at game startup.

'First, we see how many times we'll have to run through a loop, and grab the
'background and foreground layer data.
lc% = (grab / 22) / 256 'How many times does the total go into 256?
rc% = (grab / 22) MOD 256 'Is there a remainder? (One extra loop if there is)
IF rc% > 128 THEN lc% = lc% - 1 'Bugfix, so it DOESN'T round up when over 128.
LOCATE 4, 1: PRINT "Divisor is" + STR$(lc%) + ", remainder is" + STR$(rc%) + "."

IF ((grab / 22) / 256) < 1 THEN 'If the total is less than 256... loop once.
    FOR r% = 0 TO 21
        FOR s% = 0 TO ((grab / 22) - 1)
            LOCATE 5, 1: PRINT "DEBUG: Grabbing row" + STR$(r% + 1) + ", tile" + STR$(s% + 1)
            INPUT #1, ns$ 'Grab one of the foreground layer sprites
            Foreground1(r%, s%) = VAL(ns$) 'Turn it into an integer, and save
            ns$ = "" 'Erase what it's set to, just in case
            INPUT #2, ns$ 'Go for background layer one, now
            Background1(r%, s%) = VAL(ns$) 'Turn it into an integer, and save
            ns$ = "" 'One more quick erasing of this variable
            INPUT #3, ns$ 'Grab from background layer two, this time
            Background2(r%, s%) = VAL(ns$) 'Turn it into an integer, and save
        NEXT s%
    NEXT r%

    'TODO: Craft a better solution for the loop count variable (lc%) rounding
    '      up to the next highest number, especially when the maximum number
    '      of sprites is actually higher than 128, or so.
ELSEIF ((grab / 22) / 256) >= 1 THEN 'If the total is at least 256 or more,
    FOR l% = 0 TO (lc% - 1) '         loop as many times as the total number
        FOR r% = 0 TO 21 '            of tiles per row goes into 256.
            FOR s% = 0 TO 255
                LOCATE 5, 1: PRINT "DEBUG: Grabbing row" + STR$(r% + 1) + ", tile" + STR$(256 * l% + (s% + 1))
                INPUT #1, ns$ 'Grab one of the foreground layer sprites
                Foreground1(r%, s% + (256 * l%)) = VAL(ns$) 'Transform!
                ns$ = "" 'Erase what it's set to, just in case
                INPUT #2, ns$ 'Go for the first background layer, now
                Background1(r%, s% + (256 * l%)) = VAL(ns$) 'Transform!
                ns$ = "" 'Erase it again, Tony
                INPUT #3, ns$ 'Take something from background layer two!
                Background2(r%, s% + (256 * l%)) = VAL(ns$) 'Transform!
            NEXT s%
        NEXT r%
    NEXT l%

    'Now, if there was at least 256 or more, but still some left afterward,
    'let's run through one more loop, to get the remaining sprites.

    IF rc% > 0 THEN 'Any sprites left after reading each multiple of 256?
        FOR r% = 0 TO 21
            FOR s% = 0 TO (((grab / 22) - (256 * lc%)) - 1)
                LOCATE 5, 1: PRINT "DEBUG: Grabbing row" + STR$(r% + 1) + ", tile" + STR$((256 * lc%) + s% + 1)
                INPUT #1, ns$ 'Grab a sprite
                Foreground1(r%, ((256 * lc%) + s%)) = VAL(ns$) 'Save it
                ns$ = "" 'Clear the variable, because I'm paranoid
                INPUT #2, ns$ 'Grab another sprite
                Background1(r%, ((256 * lc%) + s%)) = VAL(ns$) 'Save it
                ns$ = "" 'Clear that thing! Get it far away from me! lol
                INPUT #3, ns$ 'Grab yet another sprite
                Background2(r%, ((256 * lc%) + s%)) = VAL(ns$) 'Save it
            NEXT s%
        NEXT r%
    END IF

ELSE PRINT "Double-check what LC% and RC% are set to!": END
END IF

' With the "GO, CRICKET!" message on the screen, using the
' TimeIsNow!/ClockCheck!/WhatIsTime! method mentioned earlier, the game either
' waits for the player to press ENTER, or keeps going, after 3 seconds.
LET TimeIsNow! = TIMER
DO WHILE WhatIsTime! < 3! AND NOT _KEYDOWN(13)
    LET ClockCheck! = TIMER
    LET WhatIsTime! = ClockCheck! - TimeIsNow!
    IF _KEYDOWN(13) THEN EXIT DO
LOOP

' This command sequence uses the time-trapping method again, but uses it to
' gradually fade the level information screen out, color by color, from color
' 31 (white) to 16 (black), stepping backwards. I'm probably just gonna end
' up removing this, and calling FadePal instead, so it all changes over
' seamlessly, rather than having the text fade out, then BOOM! Level map!
LET TimeIsNow! = TIMER
FOR i% = 31 TO 16 STEP -1
    LET ColorMode$ = "C" + STR$(i%)
    DRAW ColorMode$
    DRAW "B M126,80"
    Font zone$
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
'TODO: I'm gonna change it so it calls FadePal, anyway, after the timeout. It
'just won't clear the screen after it fades out.

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
ON TIMER(1) GOSUB ClockChange ' Call the subroutine to subtract every second
TIMER ON ' to change the countdown clock

' Time to draw the sprite planes! There's two so far, but I plan on adding a
' third, at some point. It'll either be a second foreground plane, or it'll
' be the player sprite plane. I haven't decided, yet.
DEF SEG = &HA000

hScroll = 0
vScroll = 0
rowpos% = 0
sprpos% = 0
sprcur% = 0
sprcnt% = 0

DO
    verticalVal = vScroll

    ' I'd like to figure out how to calculate that multiplication factor, so
    ' I don't have to keep changing it every single time I add more columns
    ' to the foreground and background maps. It'd also make things easier for
    ' modders, should Cricket get a nice following.

    fgtileX = hScroll \ 8 'Calculate all original values
    fgtileY = verticalVal \ 8 'fg = foreground, bg = background
    fgElem& = fgtileY * 476 + fgtileX 'sp = sprite plane (TODO)
    'TOTAL NUMBER OF SPRITES ON EACH DISPLAY ROW MINUS ONE!! THAT'S IT!!

    bgtileX = hScroll \ 8 'All original values that are 16 were
    bgtileY = verticalVal \ 8 'originally 221. So if I have to go
    bgElem& = bgtileY * 476 + bgtileX 'back and change them, they were 221.

    ' I'm thinking I might finally implement this after I get the maps done up
    ' for the four engine tech demo levels. This'll be the fun part, though.
    ' That, collision detection, and making some of the sprites animate.
    'sptileX = hScroll \ 8
    'sptileY = verticalVal \ 8
    'spElem& = sptileY * 476 + sptileX

    spriteX = hScroll MOD 8
    spriteY = vScroll MOD 8
    spriteElem = spriteY * 8 + spriteX

    WAIT &H3DA, 8 'Wait for vertical retrace

    ' The two FOR..NEXT loops loop through the visible screen which is the box
    ' from (24, 1) to (199, 320).
    FOR screenY = 24 TO 199

        ' Inside the second loop, the offset will only be increased by 1 each
        ' time through, so it's pointless to keep on recalculating it.  If we
        ' calculate it once for each y loop and then just add 1 inside the x
        ' loop we can speed things up.
        offset& = screenY * 320&
        'offset& = screenY * 640& 'Same video RAM area, this just uses more.

        FOR screenX = 1 TO 320

            'Use the faster PSET, which isn't even called PSET. Just POKE the
            'color setting into the video card's RAM, and it's instantly drawn
            'onto the screen. Isn't technology wonderful?
            'sprpos% = hScroll / 8
            'LOCATE 1, 1: PRINT "LINE:" + STR$(verticalVal) + " ROW:" + STR$(rowpos%) + " SPR:" + STR$(hScroll)
            'Primary background layer (BG1)
            POKE offset&, spritedata(Background1(rowpos%, sprpos%), 0, spriteElem)
            'Secondary background layer (BG2)
            IF spritedata(Background2(rowpos%, sprpos%), 0, spriteElem) > 0 THEN
                POKE offset&, spritedata(Background2(rowpos%, sprpos%), 0, spriteElem)
            END IF
            'Foreground layer (FG1)
            IF spritedata(Foreground1(rowpos%, sprpos%), 0, spriteElem) > 0 THEN
                POKE offset&, spritedata(Foreground1(rowpos%, sprpos%), 0, spriteElem)
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
                sprpos% = sprpos% + 1
                fgElem& = fgElem& + 1
                bgElem& = bgElem& + 1
            END IF
        NEXT

        ' Recalculate the map element now that we've moved down one row in the
        ' sprite, and possibly down one row on the map.  VerticalVal keeps track
        ' of the vertical motion down the map.
        verticalVal = verticalVal + 1
        sprpos% = sprcur%
        fgtileY = verticalVal \ 8
        bgtileY = verticalVal \ 8
        fgElem& = fgtileY * 476 + fgtileX
        bgElem& = bgtileY * 476 + bgtileX

        ' Recalculate the sprite element now that we've moved down one row in
        ' the sprite.  If we've moved down to the next tile, knock the spriteY
        ' value back to the top of the tile.
        spriteY = spriteY + 1
        IF spriteY = 8 THEN spriteY = 0: rowpos% = rowpos% + 1
        spriteElem = spriteY * 8 + spriteX
    NEXT

    'This will be our dummy player sprite (Cricket), while I make this work.
    'He'll start out as eight random sprites, just so I can concentrate on
    'getting the movement down, and everything else. Let's see how this goes.
    'Starting position: 148, 160 (this isn't the exact center, but close)
    FOR sr = 0 TO 3
        FOR scy = 0 TO 7
            FOR scx = 0 TO 7
                PSET (148 + scx, (160 + (sr * 8) + scy)), spritedata(4, 0, (scy * 8 + scx))
                PSET (156 + scx, (160 + (sr * 8) + scy)), spritedata(4 + (sr + 1), 0, (scy * 8 + scx))
                PSET (164 + scx, (160 + (sr * 8) + scy)), spritedata(4 + (sr + 2), 0, (scy * 8 + scx))
            NEXT scx
        NEXT scy
    NEXT sr

    rowpos% = 0 'Resets rowpos, so we can redraw the screen from row 1.
    _DISPLAY 'The screen only changes if the screen scrolls, or time changes.
    kp& = _KEYHIT
    'IF MULTIKEY(75) THEN 'Left arrow key
    IF _KEYDOWN(19200) THEN 'Left arrow key
        'Are they pushing one of the action buttons while moving?
        IF _KEYDOWN(122) THEN 'Scroll the screen faster if Z is held, too.
            IF hScroll > 0 THEN
                IF hScroll = 1 THEN 'This should stop the game from throwing
                    hScroll = 0 '    a "Subscript Out of Range" error if the
                    sprcur% = 0 '    counting integer tries to take 2 away
                    sprcnt% = 0 '    from 1, resulting in -1.
                ELSE
                    hScroll = hScroll - 2
                    IF sprcnt% = 0 THEN sprcnt% = 8: sprcur% = sprcur% - 1
                    IF sprcnt% = 1 THEN sprcnt% = 9: sprcur% = sprcur% - 1
                    LET sprcnt% = sprcnt% - 2
                    LET sprpos% = sprcur%
                END IF
            END IF
        END IF
        IF _KEYDOWN(120) THEN _SNDPLAY (ckExtraLife&) ' Holding down X
        IF _KEYDOWN(99) THEN _SNDPLAY (ckPause&) '  Holding down C
        IF hScroll > 0 AND NOT _KEYDOWN(122) THEN
            LET hScroll = hScroll - 1
            IF sprcnt% = 0 THEN sprcnt% = 8: sprcur% = sprcur% - 1
            LET sprcnt% = sprcnt% - 1
            LET sprpos% = sprcur%
        END IF
        'ELSEIF MULTIKEY(77) THEN 'Right arrow key
    ELSEIF _KEYDOWN(19712) THEN 'Right arrow key
        'Are they pushing one of the action buttons while moving?
        IF _KEYDOWN(122) THEN 'Scroll the screen faster if Z is held, too.
            IF hScroll < (((grab / 22) * 8) - 320) THEN
                IF hScroll = (((grab / 22) * 8) - 321) THEN
                    hScroll = (((grab / 22) * 8) - 320)
                    sprcnt% = sprcnt% + 1
                    IF sprcnt% = 8 THEN sprcnt% = 0: sprcur% = sprcur% + 1
                    LET sprpos% = sprcur%
                ELSE
                    hScroll = hScroll + 2
                    LET sprcnt% = sprcnt% + 2
                    IF sprcnt% = 8 THEN sprcnt% = 0: sprcur% = sprcur% + 1
                    IF sprcnt% = 9 THEN sprcnt% = 1: sprcur% = sprcur% + 1
                    LET sprpos% = sprcur%
                END IF
            END IF
        END IF
        IF _KEYDOWN(120) THEN _SNDPLAY (ckExtraLife&) ' Holding down X
        IF _KEYDOWN(99) THEN _SNDPLAY (ckPause&) '  Holding down C
        IF hScroll < (((grab / 22) * 8) - 320) AND NOT _KEYDOWN(122) THEN
            'Stops at the end of the map (unless it hits a "stop" tile - TODO)
            LET hScroll = hScroll + 1
            LET sprcnt% = sprcnt% + 1
            IF sprcnt% = 8 THEN sprcnt% = 0: sprcur% = sprcur% + 1
            LET sprpos% = sprcur%
        END IF
    ELSEIF kp& = 18432 THEN 'Up arrow key
        'For future vertical scrolling purposes. Right now, it does nothing.
        a = 0
    ELSEIF kp& = 20480 THEN 'Down arrow key
        'For future vertical scrolling purposes. Right now, it does nothing.
        a = 0
        'Original commands: MULTIKEY(29) = CTRL, 100304 = Left SHIFT
    ELSEIF kp& = 122 THEN 'Pressing Z (default RUN key) by itself
        _SNDPLAY (ckJump&) 'Just for testing purposes.
        'I wanna get the movement speed down, first. Then I'll make this work.
        'ELSEIF MULTIKEY(56) THEN ' ALT (original attack key)
    ELSEIF _KEYDOWN(120) THEN 'Pressing X (default JUMP key) by itself
        _SNDPLAY (ckSwing1&) 'Just for testing purposes.
    ELSEIF kp& = 99 THEN 'Pressing C (default ATTACK key) by itself
        _SNDPLAY (ckExtraLife&) 'Just for testing purposes.
        'I wonder if I should make it so you can hold down the attack key?
        'ELSEIF MULTIKEY(1) THEN END ' ESC ends our fancy little simulation.
    ELSEIF kp& = 27 THEN 'ESC ends our fancy little simulation.
        _FULLSCREEN _OFF
        END
    END IF

LOOP

SYSTEM ' To stop it from running farther than it should.

' Setting up the tiles, super fast, before the game starts.
' NOTE: You HAVE to put the sprites BEFORE the FG/BG/sprite layers!
' I'm wondering if I'll have to move the sprites into a data file, as well...

' Actually, I already have. Now I just need to change the loader code from a
' bunch of READ commands to a set of commands similar to the code that reads
' the arrangement of sprites for each map.

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
MissingPal: ' GAME.PAL is missing.
COLOR 31, 0: PRINT "Oh, no! I can't find GAME.PAL!"
PRINT: COLOR 7, 0
PRINT "Unfortunately, this is a file I need to run Cricket Kamodon, and since I can't"
PRINT "find it, I can't start the game."
PRINT
COLOR 31, 0: PRINT "** HOW DO I FIX THIS?"
PRINT: COLOR 7, 0
PRINT "You just need to find ";: COLOR 15, 0: PRINT "GAME.PAL";: COLOR 7, 0: PRINT ", and put it into the same"
PRINT "folder as the rest of the game. If you can't find it, you might have to reinstall"
PRINT "Cricket Kamodon."
PRINT
SYSTEM 1

MissingDemoFG: ' WLDXL1FG.KMD (tech demo only) is missing.
SCREEN 0: WIDTH 80, 25
CLOSE #1
'KILL "WLDXL1FG.KMD"    ' Disaster purposes. Can't do that during the demo!
COLOR 31, 0: PRINT "Oh, no! I can't find WLDXL1FG.KMD!"
PRINT: COLOR 7, 0
PRINT "Unfortunately, this is a file I need to run Cricket Kamodon, and since I can't"
PRINT "find it, I can't start the game."
PRINT
COLOR 31, 0: PRINT "** HOW DO I FIX THIS?"
PRINT: COLOR 7, 0
PRINT "You just need to find ";: COLOR 15, 0: PRINT "WLDXL1FG.KMD";: COLOR 7, 0: PRINT ", and put it into the same"
PRINT "folder as the rest of the game. If you can't find it, you might have to reinstall"
PRINT "Cricket Kamodon."
PRINT
SYSTEM 1

MissingDemoBG: ' WLDXL1BG.KMD (tech demo only) is missing.
SCREEN 0: WIDTH 80, 25
CLOSE #2
'KILL "WLDXL1BG.KMD"    ' Disaster purposes. Can't do that during the demo!
COLOR 31, 0: PRINT "Oh, no! I can't find WLDXL1BG.KMD!"
PRINT: COLOR 7, 0
PRINT "Unfortunately, this is a file I need to run Cricket Kamodon, and since I can't"
PRINT "find it, I can't start the game."
PRINT
COLOR 31, 0: PRINT "** HOW DO I FIX THIS?"
PRINT: COLOR 7, 0
PRINT "You just need to find ";: COLOR 15, 0: PRINT "WLDXL1BG.KMD";: COLOR 7, 0: PRINT ", and put it into the same"
PRINT "folder as the rest of the game. If you can't find it, you might have to reinstall"
PRINT "Cricket Kamodon."
PRINT
SYSTEM 1

ThisIsNotVGA: ' Just in case the computer doesn't have a VGA card.
COLOR 31, 0: PRINT "Oh, no! This computer doesn't have a VGA video card!"
PRINT: COLOR 7, 0
PRINT "Unfortunately, Cricket Kamodon uses 256 colors, and only a VGA video card (or better)"
PRINT "can support that."
PRINT
COLOR 31, 0: PRINT "** HOW DO I FIX THIS?"
PRINT: COLOR 7, 0
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
'IF tick = 59 THEN _SNDPLAY (ckTimeLow&)
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
'_DISPLAY 'To make sure the timer changes, even if the screen isn't scrolling.
RETURN

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
DRAW "B M24,65"
DRAW "C15"
Font "COPYRIGHT 2011-2013 FLAMEWARE CORPORATION"
DRAW "B M24,84"
Font "GAME ENGINE AND MOST OF THE CODE BY KAMION"
DRAW "B M40,94"
Font "GRAPHICS ROUTINES BASED ON A TUTORIAL"
DRAW "B M78,104"
Font "WRITTEN BY AARON SEVERN"
DRAW "B M104,114"
Font "2014 FOR SURE!!!"
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
            OUT &H3C8, n 'Select attribute.
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
            OUT &H3C8, n 'Select attribute.
            OUT &H3C9, RGBval!(n, 0) 'Write red.
            OUT &H3C9, RGBval!(n, 1) 'Write green.
            OUT &H3C9, RGBval!(n, 2) 'Write blue.
        NEXT n
    NEXT j
END IF

END SUB

REM $STATIC
SUB FlameWareLogo
CLS
'NOTE: All of the coordinates have been doubled, to accomodate for 640x400.
DRAW "C7"
DRAW "B M50,180" '50, 180
Font "F L A M E W A R E   P R E S E N T S"
DRAW "B M70,160" '70, 160
DRAW "C4"
DRAW "R172" '172
DRAW "U140" '140
DRAW "H3" '3
DRAW "L166" '166
DRAW "G3" '3
DRAW "D140" '140
DRAW "B R10" '10
DRAW "U80" '80
DRAW "R152" '152
DRAW "D80" '80
DRAW "B U90" '90
DRAW "B L20" '20
PAINT STEP(5, 5), 4, 4 '5, 5
DRAW "C7"
DRAW "L123" '123
DRAW "U52" '52
DRAW "R123" '123
DRAW "D52" '52
DRAW "L123" '123
DRAW "B U46" '46
DRAW "B R25" '25
DRAW "L10" '10
DRAW "D40" '40
DRAW "R10" '10
DRAW "U40" '40
PAINT STEP(1, 1), 7, 7 '1, 1
DRAW "B M155,125" '155, 125
DRAW "C4"
CIRCLE STEP(0, 0), 20, 4 '20 was radius; might have to adjust this again
DRAW "B M135,121" '135, 121
DRAW "U20" '20
DRAW "F10" '10
DRAW "B E5" '5
DRAW "B L5" '5
DRAW "B D4" '4
DRAW "E10" '10
DRAW "F10" '10
DRAW "E10" '10
DRAW "D21" '21
PAINT STEP(-3, -0), 4, 4 '-3, 0
PAINT STEP(-2, -13), 4, 4 '-2, -13
PAINT STEP(-8, 0), 4, 4 '-8, 0
PAINT STEP(-25, 2), 4, 4 '-25, 2
fwlogo& = _SNDOPEN("/media/PHANTOM/FlameWareLogoSlow.ogg")
_SNDPLAY fwlogo&
FadePal 1, Pal&()
CheckThat! = TIMER
DO
    WhatClock! = TIMER
    IF _KEYDOWN(13) THEN EXIT DO
LOOP UNTIL (WhatClock! - CheckThat!) = 5!
isit = _SNDPLAYING(fwlogo&)
IF isit THEN _SNDSTOP fwlogo&
_SNDCLOSE fwlogo&
FadePal 0, Pal&()
CLS

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
Font "ENGINE TEST DEMO V0.1.2"
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

DO UNTIL INKEY$ = ""
    ' Clear the keyboard buffer, so if you hit enter on the last screen, you won't
    ' automatically skip over the hardware detection screen. Hopefully.
LOOP

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
Font "SOUND BLASTER OR COMPATIBLE"
DRAW "B M130,85"
Font "SOUND BLASTER PRO"
DRAW "B M130,95"
Font "SOUND BLASTER 16 16ASP AWE32"
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

'Third, we find out how much conventional and expanded memory is available.
'LET ConvRAM = (FRE(-1) / 1024)
'ON LOCAL ERROR GOTO WeHaveNoEMS
'LET ExpRAM! = FRE(-3)
'ON LOCAL ERROR GOTO 0
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
GOTO SegmentFour

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
FadePal 0, Pal&()

END SUB

REM $DYNAMIC
'From "Multikey Function Update" by Joe Huber, Jr. (with parts by Eric Carr)
SUB KEYTEST (LOWERLIMIT, UPPERLIMIT)
DO
    x = 1
    y = 1

    FOR i = LOWERLIMIT TO UPPERLIMIT
        TEST = MULTIKEY(i)
        LOCATE y, x
        PRINT TEST; i
 
        IF y < 23 THEN
            y = y + 1
        ELSE
            y = 1
            x = x + 7
        END IF
    NEXT i

LOOP WHILE MULTIKEY(1) = 0
END

END SUB

'LoadPal was from Mysterious Song by DarkDread.
SUB LoadPal

FileNum = FREEFILE
OPEN "/media/PHANTOM/QBX/CRICKET/game.pal" FOR BINARY AS #FileNum
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

'From "Multikey Function Update" by Joe Huber, Jr. (with parts by Eric Carr)
FUNCTION MULTIKEY (KEYNUM)
STATIC FIRSTIME, KEYS(), SC(), DU()

IF FIRSTIME = 0 THEN
    DIM KEYS(255), SC(255), DU(255)
    FOR E = 0 TO 127 '\
        SC(E) = E: DU(E) = 1 '|
    NEXT '|-ERIC CARR'S CODE--------------------\
    FOR E = 128 TO 255 '|                                     |
        SC(E) = E - 128: DU(E) = 0 '|                                     |
    NEXT '/                                     |
    FIRSTIME = -1 '                                      |
END IF '                                      |
'                                      |
i$ = INKEY$ ' So the keyb buffer don't get full     \routine/ \ |
i = INP(&H60) ' Get keyboard scan code from port 60h   \lines/  |-/
OUT &H61, INP(&H61) OR &H82: OUT &H20, &H20 '         \!!!/   |
KEYS(SC(i)) = DU(i) ' This says what keys are pressed        \!/    /

MULTIKEY = KEYS(KEYNUM)

END FUNCTION

SUB Font (Sentence$)
LET LeftPos% = 1
LET FullPhrase$ = Sentence$
DO UNTIL FullPhrase$ = BuildPhrase$
    LET Dummy$ = LEFT$(Sentence$, LeftPos%)
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


