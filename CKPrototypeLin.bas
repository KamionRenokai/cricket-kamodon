'SHOOTING FOR BASIC MONSTER AI, A CRICKET SPRITE, WORKING PLAYER ATTACKING, NEW LEVELS AND SQUASHED BUGS!
' Cricket Kamodon (The Pseudo-Hero)
' Copyright 2011-2013 Robbie Bruce (Kamion R. Shanoriko)
'
' This game is open-source, and freeware. Feel completely free to give this
' game away to anyone, like it's a hot potato. Feel free to grab the source
' code from my website (when I build it, I'll put the URL here), and make
' your own game with it, whether you use my characters, or not. Keep in mind
' that, this game, and its source code, is available under version 3 of the
' GNU General Public License, and its text is in the CK-License.txt file.
' Any game you make, that you release, using this game engine (which I've
' named "Kamodon I", after this game), needs to also be released under the
' GPL v3, as well.
' Cricket Kamodon also utilizes the Simple DirectMedia Layer library
' (known by its friends as SDL), which is made available under version 2.1
' of the GNU Lesser General Public License, the text of which is readable
' in the SDL-License.txt file.
'
' This is the multi-platform build, originally forked from the pseudo-Windows
' build of CK Tech Demo alpha 3, after I moved the source code over to QB64
' from QuickBASIC (PDS) 7.1. Aaron Severn's side-scrolling routine, and the
' fading routines from Mysterious Song were originally re-added, but were
' later dropped, starting with alpha 9. I did make a few tweaks, though,
' mainly borrowing bits and pieces of code from the engine I was originally
' going to build for the DOS version (and still might, if I decide to pick
' that project back up). But this one definitely runs without DOS, and
' should hopefully run in OSes other than Linux (like Windows XP, Vista, 7, 8
' and Mac OS X) without too much trouble.
'
' Thanks to QB64, I can essentially port the code to C++ if I do it right,
' but it's now multi-platform (like I said above), so the only problem I
' would have, is compiling Cricket under any version of Mac OS X. Now, if
' someone would like to do that, so that Cricket can be ported to Mac OS,
' then by all means, feel free. This is open-source, after all.
'
'
' LIST OF GAME FIXES I'VE DONE SO FAR (FOR REFERENCE):
' alpha 0:  Ran out of space and memory using DATA and READ statements, and
'           would even get a "Module level code too large" message.
'     FIX:  Moved the hardware detection screen, FlameWare logo drawing
'           commands and the Cricket Kamodon logo (and probably menu, soon)
'           into SUBs, and moved the DATA commands into two files
'           (WLDXL1BG.KMD for background, and WLDXL1FG.KMD for foreground;
'           I'll follow the same method for the actual game (WLDAL1FG.KMD,
'           WLDBL4FG.KMD, etc.)
' alpha 1:  Background and foreground data file reader would get stuck at the
'           end of the first 22 lines in the data files (after 1024
'           characters), and would start redrawing the level from the
'           beginning, one row of sprites higher, about 3/4ths into the level.
'     FIX:  Reprogrammed the reader to grab every single line from the data
'           files, one after the other, and combine each line (1st and 23rd,
'           2nd and 24th, etc.), before reading each set of numbers between
'           the commas, and filling the foreground and background data arrays.
'           This also makes level design a bit more modder-friendly, since
'           this game will be open-source, after all. The current code
'           supports 110 rows of sprites.
' alpha 3:  Iminent problem of not having enough room to put the actual engine
'           code into the main module, because of the DATA statements toward
'           the end that draw each sprite.
'     FIX:  Will soon have to add another subroutine for the sprite reader,
'           which will theoretically let anyone add a lot more sprites, and
'           give each sprite up to 16 different frames of animation, through
'           files WLD?L?S0.KMD to WLD?L?SF.KMD. I figure this can make for
'           some incredibly realistic animation, if done right. This will move
'           the DATA statements that put the sprites into memory out of the
'           source code, and make room for the actual engine code.
' alpha 4:  Problem was averted with code conversion to QB64. This gets rid of
'           QuickBASIC's old "module level code too large" error, because now
'           I can put as much code in the main module as I want to, and I can
'           even use new commands, and background music/sound effects without
'           having to use a library. The only problem is, I feel like I'm
'           quickly running out of sprites I can use on one background plane.
'     FIX:  Increased the maximum number of different possible sprites one can
'           use per sprite plane to 1,000, from 100. If this isn't enough for
'           any potential modders, the level data files can be switched to
'           hexadecimal format instead, theoretically allowing up to 65,536
'           different possible sprites anyone can use on a map (no matter
'           which sprite plane they're put onto). I also added a second
'           background plane, so I can work on adding parallax scrolling.
' alpha 6:  Going to add in vertical scrolling, but I have to figure out a way
'           to implement it properly. The pixel drawing routine can't really
'           handle vertical and horizontal scrolling in its current state.
'     FIX:  Upgraded the drawing routine to be able to store multiple screens
'           (more than 22 rows), and made the background/foreground resource
'           files bigger, to accommodate this.
' alpha 7:  Having three sprite layers (BG1, BG2 and FG1) might make it hard
'           to do parallax scrolling in the future, especially since I'm using
'           BG1 for some of the background effects in Subcon 1.
'     FIX:  Created a fourth sprite layer, which will become the new BG1. The
'           original BG1 became BG2, the original BG2 became FG1, and the
'           original FG1 became FG2. This should make it easier to at least
'           do some sort of parallax scrolling with BG1.
' alpha 8:  The playfield drawing routine seems to be pretty fast... on both
'           a dual-core processor, and a first-generation high-end Core i7.
'           Testing the game both on a 2.2GHz Athlon 64, and a 1.6GHz(?)
'           Athlon XP make the drawing speed slow down to a crawl.
'     FIX:  Wrote a new routine, inspired by a new tutorial written by Garry
'           Ricketson of the QB64 forums (not copying code this time; I wrote
'           my own code, based on what he had wrote). It gives a huge speed
'           increase on the Athlon 64 and Athlon XP, even though the XP isn't
'           as fast as the 64. Still, that's an improvement.
' alpha 9:  The new side-scrolling playfield routine was added, and further
'           refined, taking out a few bugs that messed up the vertical
'           scrolling if called incorrectly, as well as adding in doors, the
'           ability to change the music (only for entering doors, for right
'           now), or just turn it off (from the Options menu), sped up the
'           fade-in/out transitions, added a placeholder transition for
'           entering and exiting doors that I plan to use in the full game,
'           the ability to know what gamepads are plugged into the computer,
'           and moved "Weapon of Choice" (or "Select Controller", if you
'           prefer) from the Options menu to the Reconfigure Controls submenu.
'    NEXT:  Starting to add in full gamepad support, as well as presets for
'           standard gamepads, PlayStation 3 gamepads connected via USB (can
'           it use Bluetooth ones as well, if the computer supports it?), and
'           the ability to change the action keys, both for the keyboard, and
'           up to 5 different gamepads (just in case). Then I'll also stop
'           the ESC key from just directly exiting the game, by giving it its
'           own "Really Quit?" menu, and giving the Pause function a menu, as
'           well.
' alpha 10: Full pause menu added (well, mostly; just "Save" and "Load" don't
'           work, but that's intentional, for the tech demo), as well as the
'           ability to change key mappings with the scheme preset, or using
'           user-defined custom keys. Gamepad button/stick mappings can be
'           changed, too, and the three scheme presets do, as well. After
'           fixing a few bugs, you can even exit back to the main menu from
'           within the game, but the ESC key still exits the game completely,
'           as I've yet to put a quick menu in for it. And speaking of gamepads,
'           I seriously need to rewrite the routines for playing the game with
'           a gamepad, instead of a keyboard.
'     FIX:  Almost completely rewrote the gamepad routines, and loosened up the
'           thumbstick controls (so you can be about 4 or 5 degrees off the
'           absolute top or bottom X or Y positions, and still have the game
'           register that as a move/action), so now the game can be played with
'           a gamepad! You can even change button and keymaps from within the
'           pause menu, and use them instantly, even though I think there might
'           be a bug, either with gamepad or keyboard mode, when doing that...
'           And speaking of gamepad mode bugs, the routines are set up so that
'           a player can set a movement or action to either a thumbstick, D-pad
'           or a button, and have it still work. It works with movements (as the
'           "PlayStation Controller" gamepad scheme preset sets the movement
'           buttons to the D-pad, which respond as "buttons" to a computer), yet
'           a few kinks appear, when trying to set an action, such as jumping, to
'           a thumbstick, or the D-pad.

DEFINT A-Z
'$DYNAMIC

DECLARE SUB Font (Sentence$)                    'From SUB90FNT.BAS
DECLARE SUB CenterFont (Sentence$, StartRow%)   'A little font centering trick
DECLARE SUB TileFont (Sentence$, StartX%, StartY%) 'Leftover from routine test
DECLARE SUB FadeIn                              'These two SUBs replace alpha
DECLARE SUB FadeOut                             '8's fade in/out SUBs.
DECLARE FUNCTION ButtonDef$ (btncode AS LONG)
DECLARE FUNCTION GamepadDef$ (action AS INTEGER)

' Extra SUBs, to make as much room for the level data as possible.
DECLARE SUB HardwareCheck ()
DECLARE SUB FlameWareLogo ()
DECLARE SUB CricketMenu ()

'** GLOBAL VARIABLES
COMMON SHARED zone$, score&, lives, amp%, tick, scrcnt%, vert%, actbgm%, bgm&, prevbgm%, dh
COMMON SHARED ckHurt&, respath$, sprfldr$, chrfldr$, bgmfldr$, sndfldr$, ext$, monfldr$
COMMON SHARED CKL%, CKR%, CKT%, CKB%, CKX%, cont%, gp%, pn, lvname$, msn, ct, res, atk, adm, atm
COMMON SHARED LevelData$, Background1$, Background2$, Foreground1$, Foreground2$, ActionDef$, ActionTable$
'** MOVEMENT KEYS
COMMON SHARED MoveUp AS LONG, ActionUp AS INTEGER
COMMON SHARED MoveDown AS LONG, ActionDown AS INTEGER
COMMON SHARED MoveLeft AS LONG, ActionLeft AS INTEGER
COMMON SHARED MoveRight AS LONG, ActionRight AS INTEGER
COMMON SHARED MoveRun AS LONG, ActionRun AS INTEGER
COMMON SHARED MoveJump AS LONG, ActionJump AS INTEGER
COMMON SHARED MoveAttack AS LONG, ActionAttack AS INTEGER
COMMON SHARED MovePause AS LONG, ActionPause AS INTEGER
COMMON SHARED MoveExit AS LONG, ActionExit AS INTEGER
'** DEBUG KEYS
COMMON SHARED MoveDReset AS LONG, ActionDReset AS INTEGER
COMMON SHARED MoveDLife AS LONG, ActionDLife AS INTEGER
COMMON SHARED MoveDReload AS LONG, ActionDReload AS INTEGER
COMMON SHARED MoveDScrlDn AS LONG, ActionDScrlDn AS INTEGER
COMMON SHARED MoveDScrlUp AS LONG, ActionDScrlUp AS INTEGER

' For the engine tech demo, this is the initial loading screen.
_TITLE "Cricket Kamodon - Development Build 12"
SCREEN 12: COLOR 7
PRINT "ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป"
PRINT "บ CRICKET KAMODON!         ** GAME ENGINE PROTOTYPE **          12th Dev Build บ"
PRINT "บ Copyright 2011-2013 Kamion Shanoriko.                        Coda ??.??.2013 บ"
PRINT "ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
PRINT
PRINT "Sound, music and basic stuff? ";

tilecnt% = 0
respath$ = "/media/Lexar/PROJECTS/CRICKET/" '<-- Remember to EDIT anything using this, before "leaking" dev builds.
sprfldr$ = "SPRITES/"
chrfldr$ = "SPRITES/PLAYER/"
monfldr$ = "SPRITES/ENEMY/"
bgmfldr$ = "MUSIC/"
sndfldr$ = "SOUND/"
ext$ = ".png"

' TODO: Implement a config file for each map, which has the max number of
'       sprites and tiles for each level. It should also specify at least what
'       background music should be available, and maybe even sound effects?
' ^^ This is a work-in-progress. Each level's data file (WLD?L?LD.KMD) has
'    the level's collision and object types defined, and is now starting to
'    implement the available background music. Soon it will also define where
'    enemies spawn, as well as what sound effects each level will need.

'Set up an array for holding up to 10 songs, even though only one can be
'loaded or played at a time (if they're all MP3s; FLACs and OGGs, OTOH...)
DIM SHARED BGM(0 TO 9) AS STRING
'The lines loading Presenterator (or Kamodanthem, in the full game) were moved
'to right before CricketMenu is called.

'Loading sound effects into memory; should start by setting up basic sounds
'for the menus themselves, and when the level is loading, read from the level
'data file, and REDIM the array without losing anything, and load the sounds
DIM SHARED SEF(0 TO 18) AS LONG 'Handles for sound effects loaded into memory
DIM SHARED SEN(0 TO 18) AS STRING 'The names of each sound effect

SEF(0) = _SNDOPEN(respath$ + sndfldr$ + "FlameWareLogo.ogg", "VOL,SYNC")
SEN(0) = "FlameWare Zeronexe Logo -- Xmuzik"
SEF(1) = _SNDOPEN(respath$ + sndfldr$ + "CKJump.ogg", "VOL,SYNC")
SEN(1) = "Cricket Jumps -- Mike Koenig"
'Maybe I should have two hammer pound sounds: one for if there's nothing under
'Cricket's hammer, and another if there is (like a monster, for example).
SEF(2) = _SNDOPEN(respath$ + sndfldr$ + "StereoEcho1UP.ogg", "VOL,SYNC")
SEN(2) = "Cricket Gets a 1UP -- www.freesfx.co.uk"
SEF(3) = _SNDOPEN(respath$ + sndfldr$ + "CKDeathSymphony.ogg", "VOL,SYNC")
SEN(3) = "Cricket Dies -- L. von Beethoven, Musopen"
SEF(4) = _SNDOPEN(respath$ + sndfldr$ + "CKVertMalletSwing.ogg", "VOL,SYNC")
SEN(4) = "Vertical Mallet Smack with no Amp -- Vladimir"
SEF(5) = 0
SEN(5) = "Vertical Mallet Smack with One-Fourth Amp -- Not Set"
SEF(6) = 0
SEN(6) = "Vertical Mallet Smack with One-Half Amp -- Not Set"
SEF(7) = 0
SEN(7) = "Vertical Mallet Smack with Full Amp -- Not Set"
SEF(8) = _SNDOPEN(respath$ + sndfldr$ + "CKHorizMalletSwing.ogg", "VOL,SYNC")
SEN(8) = "Horizontal Mallet Smack with no Amp"
SEF(9) = 0
SEN(9) = "Horizontal Mallet Smack with One-Fourth Amp -- Not Set"
SEF(10) = 0
SEN(10) = "Horizontal Mallet Smack with Half Amp -- Not Set"
SEF(11) = 0
SEN(11) = "Horizontal Mallet Smack with Full Amp -- Not Set"
SEF(12) = _SNDOPEN(respath$ + sndfldr$ + "AmpBarUp.ogg", "VOL,SYNC")
SEN(12) = "Cricket's Amp Bar Increases -- www.freesfx.co.uk"
SEF(13) = _SNDOPEN(respath$ + sndfldr$ + "Alert3Pause.ogg", "VOL,SYNC")
SEN(13) = "Pause or Unpause the Game -- www.freesfx.co.uk"
SEF(14) = _SNDOPEN(respath$ + sndfldr$ + "ClockTick.ogg", "VOL,SYNC")
SEN(14) = "Convert Remaining Time to Points"
SEF(15) = _SNDOPEN(respath$ + sndfldr$ + "TimeRunningOut.ogg", "VOL,SYNC")
SEN(15) = "Time is Running Out"
SEF(16) = _SNDOPEN(respath$ + sndfldr$ + "TimeUpBuzzer.ogg", "VOL,SYNC")
SEN(16) = "Out of Time"
SEF(17) = 0
SEN(17) = "Vertical Mallet Smack That Hits Nothing -- Not Set"
SEF(18) = _SNDOPEN(respath$ + sndfldr$ + "CKHorizMalletMiss.ogg", "VOL,SYNC")
SEN(18) = "Horizontal Mallet Smack That Hits Nothing -- www.freesfx.co.uk"
ckHurt& = _SNDOPEN(respath$ + sndfldr$ + "SQPauseSound.ogg", "VOL,SYNC")

COLOR 15, 0: PRINT "CHECK!"

COLOR 7, 0: PRINT "Find any joysticks or gamepads? ";
cont% = 0 'The active controller number. 0 means keyboard.
DIM SHARED JoyMoveOp(0 TO 13) AS INTEGER 'Gamepad keymap: 0=button, 1 or higher=stick
dev = _DEVICES 'Get the number of input devices this computer has.
IF dev > 2 THEN 'The first two are the keyboard and mouse.
    gp% = dev - 2 'Subtract those.
    COLOR 15, 0: PRINT "Found" + STR$(gp%) + "!"
    DIM SHARED ContType(0 TO gp%) AS STRING 'One keyboard, plus controllers.
    DIM SHARED ContPrst(0 TO gp%, 0 TO 2) AS STRING 'Set up the preset names.
    DIM SHARED DefKeys(0 TO gp%, 0 TO 2, 0 TO 2, 0 TO 13) AS LONG 'Defaults.
    FOR j = 3 TO dev 'Now we need to find out the name of each gamepad.
        mp% = 21 '21 characters in skips "[CONTROLLER]" and "[[NAME]".
        DO 'Basically, we read each character in the name, until we hit the ].
            gpltr$ = MID$(_DEVICE$(j), mp%, 1)
            IF gpltr$ = "]" THEN EXIT DO ELSE gpname$ = gpname$ + gpltr$
            mp% = mp% + 1
        LOOP
        ContType(j - 2) = gpname$
        ContPrst(j - 2, 0) = "Standard Controller" 'Most old or new gamepads.
        ContPrst(j - 2, 1) = "PlayStation Controller" 'PS3 controller support.
        ContPrst(j - 2, 2) = "Custom" 'Whatever gamepad button-maps you like.
        'I bet an Xbox 360 controller probably has "unique" button maps, too.
        IF (j - 2) MOD 2 = 0 THEN 'Even-numbered gamepads (2 or 4)
            DefKeys(j - 2, 0, 0, 0) = 5: DefKeys(j - 2, 0, 1, 0) = 3: DefKeys(j - 2, 0, 2, 0) = 1
            DefKeys(j - 2, 0, 0, 1) = 250: DefKeys(j - 2, 0, 1, 1) = 3: DefKeys(j - 2, 0, 2, 1) = 1
            DefKeys(j - 2, 0, 0, 2) = 5: DefKeys(j - 2, 0, 1, 2) = 2: DefKeys(j - 2, 0, 2, 2) = 1
            DefKeys(j - 2, 0, 0, 3) = 250: DefKeys(j - 2, 0, 1, 3) = 2: DefKeys(j - 2, 0, 2, 3) = 1
            DefKeys(j - 2, 0, 0, 4) = 11: DefKeys(j - 2, 0, 1, 4) = (j - 2): DefKeys(j - 2, 0, 2, 4) = 0
            DefKeys(j - 2, 0, 0, 5) = 3: DefKeys(j - 2, 0, 1, 5) = (j - 2): DefKeys(j - 2, 0, 2, 5) = 0
            DefKeys(j - 2, 0, 0, 6) = 7: DefKeys(j - 2, 0, 1, 6) = (j - 2): DefKeys(j - 2, 0, 2, 6) = 0
            DefKeys(j - 2, 0, 0, 7) = 39: DefKeys(j - 2, 0, 1, 7) = (j - 2): DefKeys(j - 2, 0, 2, 7) = 0
            '"PlayStation Controller" preset (mainly to make it easier to use the D-pad on a PS3 controller)
            DefKeys(j - 2, 1, 0, 0) = 19: DefKeys(j - 2, 1, 1, 0) = (j - 2): DefKeys(j - 2, 1, 2, 0) = 0
            DefKeys(j - 2, 1, 0, 1) = 27: DefKeys(j - 2, 1, 1, 1) = (j - 2): DefKeys(j - 2, 1, 2, 1) = 0
            DefKeys(j - 2, 1, 0, 2) = 31: DefKeys(j - 2, 1, 1, 2) = (j - 2): DefKeys(j - 2, 1, 2, 2) = 0
            DefKeys(j - 2, 1, 0, 3) = 23: DefKeys(j - 2, 1, 1, 3) = (j - 2): DefKeys(j - 2, 1, 2, 3) = 0
            DefKeys(j - 2, 1, 0, 4) = 63: DefKeys(j - 2, 1, 1, 4) = (j - 2): DefKeys(j - 2, 1, 2, 4) = 0
            DefKeys(j - 2, 1, 0, 5) = 59: DefKeys(j - 2, 1, 1, 5) = (j - 2): DefKeys(j - 2, 1, 2, 5) = 0
            DefKeys(j - 2, 1, 0, 6) = 55: DefKeys(j - 2, 1, 1, 6) = (j - 2): DefKeys(j - 2, 1, 2, 6) = 0
            DefKeys(j - 2, 1, 0, 7) = 15: DefKeys(j - 2, 1, 1, 7) = (j - 2): DefKeys(j - 2, 1, 2, 7) = 0
        ELSE 'Odd-numbered gamepads (1, 3, or 5)
            DefKeys(j - 2, 0, 0, 0) = 5: DefKeys(j - 2, 0, 1, 0) = 1: DefKeys(j - 2, 0, 2, 0) = 1
            DefKeys(j - 2, 0, 0, 1) = 250: DefKeys(j - 2, 0, 1, 1) = 1: DefKeys(j - 2, 0, 2, 1) = 1
            DefKeys(j - 2, 0, 0, 2) = 5: DefKeys(j - 2, 0, 1, 2) = 0: DefKeys(j - 2, 0, 2, 2) = 1
            DefKeys(j - 2, 0, 0, 3) = 250: DefKeys(j - 2, 0, 1, 3) = 0: DefKeys(j - 2, 0, 2, 3) = 1
            DefKeys(j - 2, 0, 0, 4) = 9: DefKeys(j - 2, 0, 1, 4) = (j - 2): DefKeys(j - 2, 0, 2, 4) = 0
            DefKeys(j - 2, 0, 0, 5) = 1: DefKeys(j - 2, 0, 1, 5) = (j - 2): DefKeys(j - 2, 0, 2, 5) = 0
            DefKeys(j - 2, 0, 0, 6) = 5: DefKeys(j - 2, 0, 1, 6) = (j - 2): DefKeys(j - 2, 0, 2, 6) = 0
            DefKeys(j - 2, 0, 0, 7) = 37: DefKeys(j - 2, 0, 1, 7) = (j - 2): DefKeys(j - 2, 0, 2, 7) = 0
            '"PlayStation Controller" preset
            DefKeys(j - 2, 1, 0, 0) = 19: DefKeys(j - 2, 1, 1, 0) = (j - 2): DefKeys(j - 2, 1, 2, 0) = 0
            DefKeys(j - 2, 1, 0, 1) = 27: DefKeys(j - 2, 1, 1, 1) = (j - 2): DefKeys(j - 2, 1, 2, 1) = 0
            DefKeys(j - 2, 1, 0, 2) = 31: DefKeys(j - 2, 1, 1, 2) = (j - 2): DefKeys(j - 2, 1, 2, 2) = 0
            DefKeys(j - 2, 1, 0, 3) = 23: DefKeys(j - 2, 1, 1, 3) = (j - 2): DefKeys(j - 2, 1, 2, 3) = 0
            DefKeys(j - 2, 1, 0, 4) = 63: DefKeys(j - 2, 1, 1, 4) = (j - 2): DefKeys(j - 2, 1, 2, 4) = 0
            DefKeys(j - 2, 1, 0, 5) = 59: DefKeys(j - 2, 1, 1, 5) = (j - 2): DefKeys(j - 2, 1, 2, 5) = 0
            DefKeys(j - 2, 1, 0, 6) = 55: DefKeys(j - 2, 1, 1, 6) = (j - 2): DefKeys(j - 2, 1, 2, 6) = 0
            DefKeys(j - 2, 1, 0, 7) = 15: DefKeys(j - 2, 1, 1, 7) = (j - 2): DefKeys(j - 2, 1, 2, 7) = 0
        END IF
        gpname$ = ""
    NEXT j
ELSEIF dev <= 2 THEN
    COLOR 15, 0: PRINT "Nothin' but the keyboard!"
    DIM SHARED ContType(0) AS STRING 'Set up for only one controller.
    DIM SHARED ContPrst(0, 0 TO 2) AS STRING 'Set only the keyboard presets.
    DIM SHARED DefKeys(0, 0 TO 2, 0, 0 TO 13) AS LONG 'Keyboard's defaults.
    gp% = 0
END IF
ContType(0) = "Keyboard" 'The default setting, so the game knows what it is.
ContPrst(0, 0) = "Right-Handed" 'Arrow keys for movement, Z, X, C, V actions.
ContPrst(0, 1) = "Left-Handed" ' W, A, S, D for movement, M, ,, ., / actions.
ContPrst(0, 2) = "Custom" '      Any key mapping other than the above two.
'Now we'll define the default keys for the keyboard. The config file, once
'I implement it, will override these, if it's set up correctly.
MoveUp = 18432 '       Up arrow -> UP
MoveDown = 20480 '     Down arrow -> DOWN
MoveLeft = 19200 '     Left arrow -> LEFT
MoveRight = 19712 '    Right arrow -> RIGHT
MoveRun = 122 '        Z key -> RUN
MoveJump = 120 '       X key -> JUMP
MoveAttack = 99 '      C key -> ATTACK
MovePause = 112 '      P key -> PAUSE
MoveExit = 27 '        ESC key -> EXIT
MoveDLife = 42 '       Asterisk key -> INSTANT 1UP (DEBUG)
MoveDReset = 114 '     R key -> RESPAWN/REPOSITION CHARACTER (DEBUG)
MoveDReload = 100306 ' Control + RESET key -> RELOAD LEVEL (DEBUG)
MoveDScrlDn = 43 '     Plus key -> FORCED DOWNWARD SCROLL (DEBUG)
MoveDScrlUp = 45 '     Minus key -> FORCED UPWARD SCROLL (DEBUG)
'Right-handed preset default keys
DefKeys(0, 0, 0, 0) = MoveUp: DefKeys(0, 0, 0, 1) = MoveDown
DefKeys(0, 0, 0, 2) = MoveLeft: DefKeys(0, 0, 0, 3) = MoveRight
DefKeys(0, 0, 0, 4) = MoveRun: DefKeys(0, 0, 0, 5) = MoveJump
DefKeys(0, 0, 0, 6) = MoveAttack: DefKeys(0, 0, 0, 7) = MovePause
DefKeys(0, 0, 0, 8) = MoveExit: DefKeys(0, 0, 0, 9) = MoveDLife
DefKeys(0, 0, 0, 10) = MoveDReset: DefKeys(0, 0, 0, 11) = MoveDReload
DefKeys(0, 0, 0, 12) = MoveDScrlDn: DefKeys(0, 0, 0, 13) = MoveDScrlUp
'Left-handed preset default keys
DefKeys(0, 1, 0, 0) = 119: DefKeys(0, 1, 0, 1) = 115 'W for UP, S for DOWN
DefKeys(0, 1, 0, 2) = 97: DefKeys(0, 1, 0, 3) = 100 'A for LEFT, D for RIGHT
DefKeys(0, 1, 0, 4) = 44: DefKeys(0, 1, 0, 5) = 46 'COMMA for RUN, PERIOD for JUMP
DefKeys(0, 1, 0, 6) = 47: DefKeys(0, 1, 0, 7) = MovePause 'FORWARD SLASH for ATTACK, P for PAUSE
DefKeys(0, 1, 0, 8) = MoveExit: DefKeys(0, 1, 0, 9) = MoveDLife 'DEBUG keys stay the same, regardless.
DefKeys(0, 1, 0, 10) = MoveDReset: DefKeys(0, 1, 0, 11) = MoveDReload
DefKeys(0, 1, 0, 12) = MoveDScrlDn: DefKeys(0, 1, 0, 13) = MoveDScrlUp

DefKeys(0, 2, 0, 8) = MoveExit: DefKeys(0, 2, 0, 9) = MoveDLife
DefKeys(0, 2, 0, 10) = MoveDReset: DefKeys(0, 2, 0, 11) = MoveDReload
DefKeys(0, 2, 0, 12) = MoveDScrlDn: DefKeys(0, 2, 0, 13) = MoveDScrlUp
'TODO: Set up a configuration file that the engine will read, and use to set the "Custom" preset's keys.
ON ERROR GOTO 0

'256 per row with a one thousand sprite palette.

COLOR 7, 0: PRINT "Level info all set up (1/3)? ";
IF _FILEEXISTS(respath$ + "WLDXL1LD.NRT") THEN
    OPEN respath$ + "WLDXL1LD.NRT" FOR INPUT AS #5
    INPUT #5, lvlnm$ 'First, we get the name of the level.
    INPUT #5, clock$ 'Second, we set the clock. (will be converted to integer)
    INPUT #5, scrnum$ 'Third, we find out how many sets of 22 rows there are.
    INPUT #5, stpos$ 'Fourth, we find out what screen row we should start on.
    INPUT #5, startx$ 'Fifth, we find out what column to put Cricket on.
    INPUT #5, starty$ 'Sixth, we find out what row to put Cricket on.
    'Then, we set up what we need to read the other resource files.
    scrcnt% = VAL(scrnum$) 'Set the max number of screens with scrnum$
    'We'll read the actual level data in during the level loading phase.
    'But right here, let's find out how many music tracks to load, later on.
    msn = 0
    DO
        INPUT #5, song$
        IF LEN(song$) > 3 THEN msn = msn + 1 ELSE EXIT DO
    LOOP
    COLOR 15, 0: PRINT "CHECK! " + STR$(msn)
ELSE
    COLOR 12, 0: PRINT "FAIL! I can't find WLDXL1LD.KMD!"
    END
END IF

COLOR 7, 0: PRINT "FG1 layer (1/3)? "; 'Using the NRT files for right now.
IF _FILEEXISTS(respath$ + "WLDXL1F1.NXT") THEN
    OPEN respath$ + "WLDXL1F1.NXT" FOR INPUT AS #1
    DO UNTIL EOF(1)
        INPUT #1, num$ 'Grab a set of numbers
        grab& = grab& + 1 'Increase total number by one
    LOOP
    IF scrcnt% > 1 THEN
        COLOR 15, 0: PRINT LTRIM$(STR$(grab& / (27 * scrcnt%))) + " sprites across 27 rows, over" + STR$(scrcnt%) + " screens."
    ELSE
        COLOR 15, 0: PRINT LTRIM$(STR$(grab& / 27)) + " sprites across 27 rows."
    END IF
ELSE
    COLOR 12, 0: PRINT "FAIL! I can't find WLDXL1F1.KMD!"
    END
END IF

COLOR 7, 0: PRINT "FG2 layer (1/3)? "; 'Using the NRT files for right now.
IF _FILEEXISTS(respath$ + "WLDXL1F2.NXT") THEN
    OPEN respath$ + "WLDXL1F2.NXT" FOR INPUT AS #2
    DO UNTIL EOF(2)
        INPUT #2, num$ 'Grab a set of numbers
        keep& = keep& + 1 'Increase total number by one
    LOOP
    IF scrcnt% > 1 THEN
        COLOR 15, 0: PRINT LTRIM$(STR$(keep& / (27 * scrcnt%))) + " sprites across 27 rows, over" + STR$(scrcnt%) + " screens."
    ELSE
        COLOR 15, 0: PRINT LTRIM$(STR$(keep& / 27)) + " sprites across 27 rows."
    END IF
ELSE
    COLOR 12, 0: PRINT "FAIL! I can't find WLDXL1F2.KMD!"
    END
END IF

COLOR 7, 0: PRINT "BG1 layer (1/3)? "; 'Using the NRT files for right now.
IF _FILEEXISTS(respath$ + "WLDXL1B1.NXT") THEN
    OPEN respath$ + "WLDXL1B1.NXT" FOR INPUT AS #3
    DO UNTIL EOF(3)
        INPUT #3, num$ 'Grab a set of numbers
        hold& = hold& + 1 'Increase total number by one
    LOOP
    IF scrcnt% > 1 THEN
        COLOR 15, 0: PRINT LTRIM$(STR$(hold& / (27 * scrcnt%))) + " sprites across 27 rows, over" + STR$(scrcnt%) + " screens."
    ELSE
        COLOR 15, 0: PRINT LTRIM$(STR$(hold& / 27)) + " sprites across 27 rows."
    END IF
ELSE
    COLOR 12, 0: PRINT "FAIL! I can't find WLDXL1B1.KMD!"
    END
END IF

COLOR 7, 0: PRINT "BG2 layer (1/3)? "; 'Using the NRT files for right now.
IF _FILEEXISTS(respath$ + "WLDXL1B2.NXT") THEN
    OPEN respath$ + "WLDXL1B2.NXT" FOR INPUT AS #4
    DO UNTIL EOF(4)
        INPUT #4, num$ 'Grab a set of numbers
        take& = take& + 1 'Increase total number by one
    LOOP
    IF scrcnt% > 1 THEN
        COLOR 15, 0: PRINT LTRIM$(STR$(take& / (27 * scrcnt%))) + " sprites across 27 rows, over" + STR$(scrcnt%) + " screens."
    ELSE
        COLOR 15, 0: PRINT LTRIM$(STR$(take& / 27)) + " sprites across 27 rows."
    END IF
ELSE
    COLOR 12, 0: PRINT "FAIL! I can't find WLDXL1B2.KMD!"
    END
END IF

COLOR 7, 0: PRINT "Level info all set up (2/3)? ";
IF _FILEEXISTS(respath$ + "WLDXL2LD.KMD") THEN
    OPEN respath$ + "WLDXL2LD.KMD" FOR INPUT AS #10
    INPUT #10, lvlnm$ 'First, we get the name of the level.
    INPUT #10, clock$ 'Second, we set the clock. (will be converted to integer)
    INPUT #10, scrnum$ 'Third, we find out how many sets of 22 rows there are.
    INPUT #10, stpos$ 'Fourth, we find out what screen row we should start on.
    INPUT #10, startx$ 'Fifth, we find out what column to put Cricket on.
    INPUT #10, starty$ 'Sixth, we find out what row to put Cricket on.
    'Then, we set up what we need to read the other resource files.
    scrcnt2% = VAL(scrnum$) 'Set the max number of screens with scrnum$
    'We'll read the actual level data in during the level loading phase.
    'But right here, let's find out how many music tracks to load, later on.
    msn = 0
    DO
        INPUT #10, song$
        IF LEN(song$) > 3 THEN msn = msn + 1 ELSE EXIT DO
    LOOP
    COLOR 15, 0: PRINT "CHECK! " + STR$(msn)
ELSE
    COLOR 12, 0: PRINT "FAIL! I can't find WLDXL2LD.KMD!"
    END
END IF

COLOR 7, 0: PRINT "FG1 layer (2/3)? "; 'Using the NRT files for right now.
IF _FILEEXISTS(respath$ + "WLDXL2F1.KMD") THEN
    OPEN respath$ + "WLDXL2F1.KMD" FOR INPUT AS #6
    DO UNTIL EOF(6)
        INPUT #6, num$ 'Grab a set of numbers
        grab2& = grab2& + 1 'Increase total number by one
    LOOP
    IF scrcnt2% > 1 THEN
        COLOR 15, 0: PRINT LTRIM$(STR$(grab2& / (27 * scrcnt2%))) + " sprites across 27 rows, over" + STR$(scrcnt2%) + " screens."
    ELSE
        COLOR 15, 0: PRINT LTRIM$(STR$(grab2& / 27)) + " sprites across 27 rows."
    END IF
ELSE
    COLOR 12, 0: PRINT "FAIL! I can't find WLDXL2F1.KMD!"
    END
END IF

COLOR 7, 0: PRINT "FG2 layer (2/3)? "; 'Using the NRT files for right now.
IF _FILEEXISTS(respath$ + "WLDXL2F2.KMD") THEN
    OPEN respath$ + "WLDXL2F2.KMD" FOR INPUT AS #7
    DO UNTIL EOF(7)
        INPUT #7, num$ 'Grab a set of numbers
        keep2& = keep2& + 1 'Increase total number by one
    LOOP
    IF scrcnt2% > 1 THEN
        COLOR 15, 0: PRINT LTRIM$(STR$(keep2& / (27 * scrcnt2%))) + " sprites across 27 rows, over" + STR$(scrcnt2%) + " screens."
    ELSE
        COLOR 15, 0: PRINT LTRIM$(STR$(keep2& / 27)) + " sprites across 27 rows."
    END IF
ELSE
    COLOR 12, 0: PRINT "FAIL! I can't find WLDXL2F2.KMD!"
    END
END IF

COLOR 7, 0: PRINT "BG1 layer (2/3)? "; 'Using the NRT files for right now.
IF _FILEEXISTS(respath$ + "WLDXL2B1.KMD") THEN
    OPEN respath$ + "WLDXL2B1.KMD" FOR INPUT AS #8
    DO UNTIL EOF(8)
        INPUT #8, num$ 'Grab a set of numbers
        hold2& = hold2& + 1 'Increase total number by one
    LOOP
    IF scrcnt2% > 1 THEN
        COLOR 15, 0: PRINT LTRIM$(STR$(hold2& / (27 * scrcnt2%))) + " sprites across 27 rows, over" + STR$(scrcnt2%) + " screens."
    ELSE
        COLOR 15, 0: PRINT LTRIM$(STR$(hold2& / 27)) + " sprites across 27 rows."
    END IF
ELSE
    COLOR 12, 0: PRINT "FAIL! I can't find WLDXL2B1.KMD!"
    END
END IF

COLOR 7, 0: PRINT "BG2 layer (2/3)? "; 'Using the NRT files for right now.
IF _FILEEXISTS(respath$ + "WLDXL2B2.KMD") THEN
    OPEN respath$ + "WLDXL2B2.KMD" FOR INPUT AS #9
    DO UNTIL EOF(9)
        INPUT #9, num$ 'Grab a set of numbers
        take2& = take2& + 1 'Increase total number by one
    LOOP
    IF scrcnt2% > 1 THEN
        COLOR 15, 0: PRINT LTRIM$(STR$(take2& / (27 * scrcnt2%))) + " sprites across 27 rows, over" + STR$(scrcnt2%) + " screens."
    ELSE
        COLOR 15, 0: PRINT LTRIM$(STR$(take2& / 27)) + " sprites across 27 rows."
    END IF
ELSE
    COLOR 12, 0: PRINT "FAIL! I can't find WLDXL2B2.KMD!"
    END
END IF

COLOR 7, 0: PRINT "Level info all set up (3/3)? ";
IF _FILEEXISTS(respath$ + "WLDXL3LD.KMD") THEN
    OPEN respath$ + "WLDXL3LD.KMD" FOR INPUT AS #15
    INPUT #15, lvlnm$ 'First, we get the name of the level.
    INPUT #15, clock$ 'Second, we set the clock. (will be converted to integer)
    INPUT #15, scrnum$ 'Third, we find out how many sets of 22 rows there are.
    INPUT #15, stpos$ 'Fourth, we find out what screen row we should start on.
    INPUT #15, startx$ 'Fifth, we find out what column to put Cricket on.
    INPUT #15, starty$ 'Sixth, we find out what row to put Cricket on.
    'Then, we set up what we need to read the other resource files.
    scrcnt3% = VAL(scrnum$) 'Set the max number of screens with scrnum$
    'We'll read the actual level data in during the level loading phase.
    'But right here, let's find out how many music tracks to load, later on.
    msn = 0
    DO
        INPUT #15, song$
        IF LEN(song$) > 3 THEN msn = msn + 1 ELSE EXIT DO
    LOOP
    COLOR 15, 0: PRINT "CHECK! " + STR$(msn)
ELSE
    COLOR 12, 0: PRINT "FAIL! I can't find WLDXL3LD.KMD!"
    END
END IF

COLOR 7, 0: PRINT "FG1 layer (3/3)? "; 'Using the NRT files for right now.
IF _FILEEXISTS(respath$ + "WLDXL3F1.KMD") THEN
    OPEN respath$ + "WLDXL3F1.KMD" FOR INPUT AS #11
    DO UNTIL EOF(11)
        INPUT #11, num$ 'Grab a set of numbers
        grab3& = grab3& + 1 'Increase total number by one
    LOOP
    IF scrcnt3% > 1 THEN
        COLOR 15, 0: PRINT LTRIM$(STR$(grab3& / (27 * scrcnt3%))) + " sprites across 27 rows, over" + STR$(scrcnt3%) + " screens."
    ELSE
        COLOR 15, 0: PRINT LTRIM$(STR$(grab3& / 27)) + " sprites across 27 rows."
    END IF
ELSE
    COLOR 12, 0: PRINT "FAIL! I can't find WLDXL3F1.KMD!"
    END
END IF

COLOR 7, 0: PRINT "FG2 layer (3/3)? "; 'Using the NRT files for right now.
IF _FILEEXISTS(respath$ + "WLDXL3F2.KMD") THEN
    OPEN respath$ + "WLDXL3F2.KMD" FOR INPUT AS #12
    DO UNTIL EOF(12)
        INPUT #12, num$ 'Grab a set of numbers
        keep3& = keep3& + 1 'Increase total number by one
    LOOP
    IF scrcnt3% > 1 THEN
        COLOR 15, 0: PRINT LTRIM$(STR$(keep3& / (27 * scrcnt3%))) + " sprites across 27 rows, over" + STR$(scrcnt3%) + " screens."
    ELSE
        COLOR 15, 0: PRINT LTRIM$(STR$(keep3& / 27)) + " sprites across 27 rows."
    END IF
ELSE
    COLOR 12, 0: PRINT "FAIL! I can't find WLDXL3F2.KMD!"
    END
END IF

COLOR 7, 0: PRINT "BG1 layer (3/3)? "; 'Using the NRT files for right now.
IF _FILEEXISTS(respath$ + "WLDXL3B1.KMD") THEN
    OPEN respath$ + "WLDXL3B1.KMD" FOR INPUT AS #13
    DO UNTIL EOF(13)
        INPUT #13, num$ 'Grab a set of numbers
        hold3& = hold3& + 1 'Increase total number by one
    LOOP
    IF scrcnt3% > 1 THEN
        COLOR 15, 0: PRINT LTRIM$(STR$(hold3& / (27 * scrcnt3%))) + " sprites across 27 rows, over" + STR$(scrcnt3%) + " screens."
    ELSE
        COLOR 15, 0: PRINT LTRIM$(STR$(hold3& / 27)) + " sprites across 27 rows."
    END IF
ELSE
    COLOR 12, 0: PRINT "FAIL! I can't find WLDXL3B1.KMD!"
    END
END IF

COLOR 7, 0: PRINT "BG2 layer (3/3)? "; 'Using the NRT files for right now.
IF _FILEEXISTS(respath$ + "WLDXL3B2.KMD") THEN
    OPEN respath$ + "WLDXL3B2.KMD" FOR INPUT AS #14
    DO UNTIL EOF(14)
        INPUT #14, num$ 'Grab a set of numbers
        take3& = take3& + 1 'Increase total number by one
    LOOP
    IF scrcnt3% > 1 THEN
        COLOR 15, 0: PRINT LTRIM$(STR$(take3& / (27 * scrcnt3%))) + " sprites across 27 rows, over" + STR$(scrcnt3%) + " screens."
    ELSE
        COLOR 15, 0: PRINT LTRIM$(STR$(take3& / 27)) + " sprites across 27 rows."
    END IF
ELSE
    COLOR 12, 0: PRINT "FAIL! I can't find WLDXL3B2.KMD!"
    END
END IF

COLOR 7, 0: PRINT "Sprites? ";
DO
    destpath$ = respath$ + sprfldr$ + LTRIM$(STR$(tilecnt%)) + ext$
    IF _FILEEXISTS(destpath$) THEN
        tilecnt% = tilecnt% + 1
    ELSE
        tilecnt% = tilecnt% - 1
        EXIT DO
    END IF
LOOP
COLOR 15, 0: PRINT LTRIM$(STR$(tilecnt% + 1)) + " were found."

CLOSE
IF grab& = hold& AND grab& = take& AND grab& = keep& AND hold& = take& AND hold& = keep& AND take& = keep& THEN 'This really works?!
    COLOR 15, 0: PRINT "(1) All four sprite layers are the same length! Awesome!"
ELSE
    COLOR 12, 0 'Surprisingly, QB64/G++ CAN handle all this... good coding!
    IF hold& < grab& OR hold& < take& OR hold& < keep& THEN PRINT "BG1 is smaller than BG2, FG1 and FG2! They should all be the same length!": END
    IF take& < grab& OR take& < hold& OR take& < keep& THEN PRINT "BG2 is smaller than BG1, FG1 and FG2! They should all be the same length!": END
    IF grab& < hold& OR grab& < take& OR grab& < keep& THEN PRINT "FG1 is smaller than BG1, BG2 and FG2! They should all be the same length!": END
    IF keep& < grab& OR keep& < hold& OR keep& < take& THEN PRINT "FG2 is smaller than BG1, BG2 and FG1! They should all be the same length!": END
END IF

IF grab2& = hold2& AND grab2& = take2& AND grab2& = keep2& AND hold2& = take2& AND hold2& = keep2& AND take2& = keep2& THEN 'This really works?!
    COLOR 15, 0: PRINT "(2) All four sprite layers are the same length! Awesome!"
ELSE
    COLOR 12, 0 'Surprisingly, QB64/G++ CAN handle all this... good coding!
    IF hold2& < grab2& OR hold2& < take2& OR hold2& < keep2& THEN PRINT "BG1 is smaller than BG2, FG1 and FG2! They should all be the same length!": END
    IF take2& < grab2& OR take2& < hold2& OR take2& < keep2& THEN PRINT "BG2 is smaller than BG1, FG1 and FG2! They should all be the same length!": END
    IF grab2& < hold2& OR grab2& < take2& OR grab2& < keep2& THEN PRINT "FG1 is smaller than BG1, BG2 and FG2! They should all be the same length!": END
    IF keep2& < grab2& OR keep2& < hold2& OR keep2& < take2& THEN PRINT "FG2 is smaller than BG1, BG2 and FG1! They should all be the same length!": END
END IF

IF grab3& = hold3& AND grab3& = take3& AND grab3& = keep3& AND hold3& = take3& AND hold3& = keep3& AND take3& = keep3& THEN 'This really works?!
    COLOR 15, 0: PRINT "(3) All four sprite layers are the same length! Awesome!"
ELSE
    COLOR 12, 0 'Surprisingly, QB64/G++ CAN handle-- yeah, you get the idea.
    IF hold3& < grab3& OR hold3& < take3& OR hold3& < keep3& THEN PRINT "BG1 is smaller than BG2, FG1 and FG2! They should all be the same length!": END
    IF take3& < grab3& OR take3& < hold3& OR take3& < keep3& THEN PRINT "BG2 is smaller than BG1, FG1 and FG2! They should all be the same length!": END
    IF grab3& < hold3& OR grab3& < take3& OR grab3& < keep3& THEN PRINT "FG1 is smaller than BG1, BG2 and FG2! They should all be the same length!": END
    IF keep3& < grab3& OR keep3& < hold3& OR keep3& < take3& THEN PRINT "FG2 is smaller than BG1, BG2 and FG1! They should all be the same length!": END
END IF

'**** Change for alpha 10: map data arrays aren't DIM'd until the level loading routine.
'**** Change for alpha 12: sprite array is extended to fit sixteen frames for each background/foreground sprite
DIM SHARED spritedata(0 TO 1280, 0 TO tilecnt%) AS LONG 'The sprite array stays here, though!

'Older routine comments were removed; they're in older source files, anyway.

PRINT
PRINT "** PRESS ANY KEY TO START."
DO: SLEEP: LOOP WHILE INKEY$ = ""

DO: what& = _KEYHIT: LOOP UNTIL NOT what& ' Clear the keyboard buffer?

SCREEN _NEWIMAGE(320, 240, 32)
'_FULLSCREEN _SQUAREPIXELS
'IF NOT _FULLSCREEN THEN _FULLSCREEN _OFF 'Windowed mode if fullscreen fails.

ON ERROR GOTO 0 ' Kill the error-trapping subroutine (should we keep this?)
CALL HardwareCheck ' The "tech demo intro message" only. The rest is skipped.
CALL FlameWareLogo ' FLAMEWARE PRESENTS screen

'** Change for alpha 11: I put a line marker here, as I'm going to put in ESC key and pause menus.
LoopBack: 'To jump back to the main menu, should the player hit ESC while in-game.
BGM(0) = respath$ + bgmfldr$ + "TechDemoTheme.mp3"
'If there's other songs loaded (like, if we come here from exiting the game), unload them
IF LEN(BGM(1)) > 0 THEN 'A fun little trick; if there was more than one BGM, that number will be higher than zero.
    FOR dx = 1 TO 9
        BGM(dx) = ""
    NEXT dx
END IF
IF actbgm% >= 0 THEN
    bgm& = _SNDOPEN(BGM(0), "VOL,PAUSE")
    actbgm% = 0
    IF bgm& THEN _SNDLOOP bgm& 'Only plays background music if it's actually defined and loaded
END IF
CricketMenu ' For the demo, just the demo logo (which I should remake later), and the main menu.
'The SUB will set up everything the engine needs to run, then it'll hand off to it,
'when the main DO..LOOP is exited (with EXIT DO), which happens when a game is started (or in the future, loaded).

' This triggers a fade-out, and starts drawing the scoreboard at the top of the screen, along with
' the name of the level that's being loaded, how many lives you have, and my version of "MARIO START!"
IF lives = 0 THEN lives = 5
FadeOut
CLS
LINE (0, 0)-(319, 22), _RGBA32(0, 255, 0, 255), BF
LINE (0, 0)-(40, 22), _RGBA32(0, 0, 127, 255), BF
LINE (160, 0)-(212, 11), _RGBA32(0, 0, 127, 255), BF
LINE (245, 0)-(285, 11), _RGBA32(0, 0, 127, 255), BF
LINE (120, 12)-(150, 22), _RGBA32(0, 0, 127, 255), BF
LINE (245, 12)-(319, 22), _RGBA32(0, 127, 0, 255), BF
DRAW "C" + STR$(&HFFFFFFFF) + " BM7,8"
Font "ZONE"
DRAW "C" + STR$(&HFF000000) + " BM44,8"
Font zone$ 'Interchangeable string variable, shows area name and level number
DRAW "C" + STR$(&HFFFFFFFF) + " BM164,8"
Font "CRICKET"
IF LEN(LTRIM$(STR$(lives))) = 3 THEN 'Over 100 lives! Somebody's GOOD!
    IF LEFT$(LTRIM$(STR$(lives)), 1) = "1" THEN DRAW "C" + STR$(&HFF000000) + " BM220,8" ELSE DRAW "C" + STR$(&HFF000000) + " BM219,8"
    Font LTRIM$(STR$(lives))
ELSEIF LEN(LTRIM$(STR$(lives))) = 2 THEN 'Anywhere from 10 to 99 lives
    IF LEFT$(LTRIM$(STR$(lives)), 1) = "1" THEN DRAW "C" + STR$(&HFF000000) + " BM224,8" ELSE DRAW "C" + STR$(&HFF000000) + " BM223,8"
    Font LTRIM$(STR$(lives))
ELSE 'Anywhere from 0 to 9 lives, either he's a cat-cricket, or not. xD
    IF LTRIM$(STR$(lives)) = "1" THEN DRAW "C" + STR$(&HFF000000) + " BM227,8" ELSE DRAW "C" + STR$(&HFF000000) + " BM226,8"
    Font LTRIM$(STR$(lives))
END IF
DRAW "C" + STR$(&HFFFFFFFF) + " BM252,8"
Font "TIME"
DRAW "C" + STR$(&HFF000000) + " BM293,8"
Font LTRIM$(STR$(tick)) 'Should display the starting time on the clock
DRAW "C" + STR$(&HFFFFFFFF) + " BM3,20"
Font "SCORE"
DRAW "C" + STR$(&HFF000000) + " BM" + STR$(119 - (8 * LEN(LTRIM$(STR$(score&))))) + ",20"
Font LTRIM$(STR$(score&))
DRAW "C" + STR$(&HFFFFFFFF) + " BM125,20"
Font "AMP"
IF amp% > 0 THEN LINE (158, 14)-((158 + amp%), 20), _RGBA32(amp% * 3, amp% * 1.5, (240 - (amp% * 3)), 255), BF
DRAW "C" + STR$(&HFF000000) + " BM261,20"
Font "NORMAL"
DRAW "C" + STR$(&HFFFFFFFF)
CenterFont zone$ + ": " + UCASE$(lvname$), 100 'There's no lowercase letters
CenterFont LTRIM$(STR$(lives)) + " LIVES LEFT", 120
CenterFont "GO, CRICKET!", 140
FadeIn

'Read the foreground and background maps from a file (hopefully save memory)
'Moved these here, since the actual game will load level data on this screen.

LOCATE 15, 1: PRINT "DEBUG: Quick check: ";
'Foreground 1 quick sanity check
grab& = 0
DO UNTIL EOF(1)
    INPUT #1, num$ 'Grab a set of numbers
    grab& = grab& + 1 'Increase total number by one
LOOP
CLOSE #1: OPEN Foreground1$ FOR INPUT AS #1 'Close and reopen it for loading
PRINT "FG1 ";

'Foreground 2 quick sanity check
keep& = 0
DO UNTIL EOF(2)
    INPUT #2, num$ 'Grab a set of numbers
    keep& = keep& + 1 'Increase total number by one
LOOP
CLOSE #2: OPEN Foreground2$ FOR INPUT AS #2 'Close and reopen it for loading
PRINT "FG2 ";

'Background 1 quick sanity check
hold& = 0
DO UNTIL EOF(3)
    INPUT #3, num$ 'Grab a set of numbers
    hold& = hold& + 1 'Increase total number by one
LOOP
CLOSE #3: OPEN Background1$ FOR INPUT AS #3 'Close and reopen it for loading
PRINT "BG1 ";

'Background 2 quick sanity check
take& = 0
DO UNTIL EOF(4)
    INPUT #4, num$ 'Grab a set of numbers
    take& = take& + 1 'Increase total number by one
LOOP
CLOSE #4: OPEN Background2$ FOR INPUT AS #4 'Close and reopen it for loading
PRINT "BG2";

_DISPLAY 'So the player doesn't see the sprites being drawn.
FOR tc = 0 TO tilecnt%
    'LOCATE 5, 1: PRINT "DEBUG: Loading sprite " + LTRIM$(STR$(tc)) + "... ";
    destpath$ = respath$ + sprfldr$ + LTRIM$(STR$(tc)) + ext$
    tx& = _LOADIMAGE(destpath$)
    _PUTIMAGE (0, 50), tx&
    IF _WIDTH(tx&) = 8 THEN
        FOR i = 0 TO 15
            GET (0, 50)-(7, 57), spritedata(i * 80, tc)
        NEXT i
    ELSEIF _WIDTH(tx&) = 158 THEN
        FOR i = 0 TO 15
            GET (i * 10, 50)-((i * 10) + 7, 57), spritedata(i * 80, tc)
        NEXT i
    END IF
    _FREEIMAGE tx&
    LINE (0, 50)-(170, 57), _RGBA32(0, 0, 0, 255), BF
    'spritedata(tc, 0) = _LOADIMAGE(destpath$)
NEXT tc

'Now we set up the map data arrays, sized according to the level.
IF res THEN 'Check: is this the first time, this run, that we've DIM'd these arrays?
    REDIM SHARED Background1(0 TO ((27 * scrcnt%) - 1), 0 TO ((hold& / 27) - 1)) AS _UNSIGNED INTEGER
    REDIM SHARED Background2(0 TO ((27 * scrcnt%) - 1), 0 TO ((take& / 27) - 1)) AS _UNSIGNED INTEGER
    REDIM SHARED Foreground1(0 TO ((27 * scrcnt%) - 1), 0 TO ((grab& / 27) - 1)) AS _UNSIGNED INTEGER
    REDIM SHARED Foreground2(0 TO ((27 * scrcnt%) - 1), 0 TO ((keep& / 27) - 1)) AS _UNSIGNED INTEGER
    REDIM SHARED LevelData(0 TO ((27 * scrcnt%) - 1), 0 TO ((grab& / 27) - 1)) AS _UNSIGNED INTEGER
    REDIM SHARED MonsterAnim(adm - 1, 0 TO 3, 0) AS LONG 'Animation type: 0=walk, 1=attack, 2=hurt, 3=death (? WIP)
    REDIM SHARED MnS(atm - 1, 0 TO 5) AS INTEGER 'Spawn number: 0=hasn't appeared, 1=has appeared, 2=was defeated
    REDIM SHARED MnP(atm - 1) AS INTEGER 'Temporary array for remembering the next command in each monster's AI
    REDIM SHARED ActionType(adm - 1) AS STRING 'Which of the read action types are "ENEMY", "BOSS" or "ACTION"
    REDIM SHARED TrigPos(atm - 1, 0 TO 3) AS STRING 'Since only one value is a string, they all have to be string vars
ELSE 'If it is, then let's DIM them properly
    DIM SHARED Background1(0 TO ((27 * scrcnt%) - 1), 0 TO ((hold& / 27) - 1)) AS _UNSIGNED INTEGER
    DIM SHARED Background2(0 TO ((27 * scrcnt%) - 1), 0 TO ((take& / 27) - 1)) AS _UNSIGNED INTEGER
    DIM SHARED Foreground1(0 TO ((27 * scrcnt%) - 1), 0 TO ((grab& / 27) - 1)) AS _UNSIGNED INTEGER
    DIM SHARED Foreground2(0 TO ((27 * scrcnt%) - 1), 0 TO ((keep& / 27) - 1)) AS _UNSIGNED INTEGER
    'Should I keep it as 2 background layers, and 2 foreground layers?
    DIM SHARED LevelData(0 TO ((27 * scrcnt%) - 1), 0 TO ((grab& / 27) - 1)) AS _UNSIGNED INTEGER
    DIM SHARED MonsterAnim(adm - 1, 0 TO 3, 0) AS LONG 'Number order: monster number, animation type, animation frame.
    DIM SHARED MnS(atm - 1, 0 TO 5) AS INTEGER 'Monster number, left/right/top/bottom/global X coords/spawn status
    DIM SHARED MnP(atm - 1) AS INTEGER 'For right now, it just keeps track of what direction they're facing
    DIM SHARED ActionType(adm - 1) AS STRING 'Keeps track of the different bosses/enemies/action triggers.
    DIM SHARED TrigPos(atm - 1, 0 TO 3) AS STRING 'Corresponding number, direction facing, and tile coordinates
END IF

FOR d = 0 TO (adm - 1)
    INPUT #6, a$, b$, c$ 'For testing purposes, these should read "000", "ENEMY" and "ShyGuy.png".
    a% = VAL(a$) 'This is a number, so we'll convert it to an integer
    'TODO: We'll need to put additional lines in for each type, like "ENEMY", "BOSS" and "ACTION".
    ActionType(a%) = b$
    IF UCASE$(b$) = "ENEMY" THEN
        MonsterAnim(a%, 0, 0) = _LOADIMAGE(respath$ + monfldr$ + c$)
        'TODO: Once monster coordinates have been refined, have the game set them up from this routine.
    ELSEIF UCASE$(b$) = "BOSS" THEN
        MonsterAnim(a%, 0, 0) = _LOADIMAGE(respath$ + monfldr$ + c$)
        'Work-in-progress...
    ELSEIF UCASE$(b$) = "ACTION" THEN
        'Work-in-progress...
    ELSE 'Throw an error?
    END IF
NEXT d

FOR t = 0 TO (atm - 1) 'Set up each enemy (work-in-progress)
    INPUT #7, j$, k$, l$, m$ 'For testing purposes, the first line should read "000", "R", "130" and "18".
    TrigPos(t, 0) = j$: TrigPos(t, 1) = k$: TrigPos(t, 2) = l$: TrigPos(t, 3) = m$
    j% = VAL(j$) 'Since this is a number, let's turn it into an integer, as that'll help us better.
    'TODO: Check the first number against MonsterAnim and ActionType, and read more options if it's an ACTION type.
    'Right now, the spawning routine in the main game loop overwrites these. Should I keep this part in, anyway?
    'MnS(t, 0) = (VAL(TrigPos(t, 3)) * 8) - 8
    'MnS(t, 1) = MnS(t, 0) + (_WIDTH(MonsterAnim(j%, 0, 0)) - 1)
    'dvs = VAL(TrigPos(t, 2)) / 27
    'rm = VAL(TrigPos(t, 2)) MOD 27
    'IF rm > 13 THEN dvs = dvs - 1 'Currently, the main game routine overwrites MnS(?, 2) and MnS(?, 3) when spawning.
    'MnS(t, 2) = (VAL(TrigPos(t, 2)) - (dvs * 27)) * 8 '(VAL(TrigPos(t, 2)) - vert%) * 8
    'MnS(t, 3) = MnS(t, 2) + _HEIGHT(MonsterAnim(j%, 0, 0))
    MnS(t, 4) = VAL(TrigPos(t, 3)) * 8 'Except the global X coordinate. The spawning routine doesn't touch this, yet.
    IF TrigPos(t, 1) = "L" THEN MnP(t) = -48 'Little quick bugfix for monsters that spawn facing left.
NEXT t

'First, we see how many times we'll have to run through a loop, and grab the
'background and foreground layer data, as well as the level data itself. I
'figured I'd throw the level data reading commands in with the sprite reading
'routine, since the level data file works the same way, past the ID stuff at
'the top of the file.
lc% = ((grab& / 27) / scrcnt%) / 256 'This stuff is... kinda complicated. xD
'Start by figuring out how many sprites there are, total, on one row of each
'level (a level is 27 rows, or one whole screenful of sprites), then divide
'that by the total number of three-digit number sets that can be on one line
'in the level data files (background1, background2, etc.), which is 256, times
'the number of levels on the map. Then we see if it all divides evenly.
rc% = ((grab& / 27) / scrcnt%) MOD 256 'Loop once more if there's a remainder.
IF rc% >= 128 THEN lc% = lc% - 1 'Bugfix. Don't round up when it's 128 and up!
'LOCATE 3, 1: PRINT "Divisor is" + STR$(lc%) + ", remainder is" + STR$(rc%) + "."

'Loop this entire routine as many times as there are screen levels on the map
FOR sl% = 0 TO (scrcnt% - 1)

    IF lc% < 1 THEN 'If the total is less than 256... loop once.
        FOR r% = 0 TO 26
            FOR s% = 0 TO (((grab& / 27) / scrcnt%) - 1)
                'LOCATE 4, 1: PRINT "DEBUG: Grabbing row" + STR$((r% + (27 * sl%)) + 1) + ", tile" + STR$(s% + 1) + " A "
                INPUT #1, ns$ 'Grab one of foreground layer one's sprites
                Foreground1((r% + (27 * sl%)), s%) = VAL(ns$) 'Convert & save
                ns$ = "" 'Erase what it's set to, just in case
                INPUT #2, ns$ 'Go for foreground layer two, now
                Foreground2((r% + (27 * sl%)), s%) = VAL(ns$) 'Convert & save
                ns$ = "" 'Let's erase it again, as quickly as we can
                INPUT #3, ns$ 'Go for background layer one, next
                Background1((r% + (27 * sl%)), s%) = VAL(ns$) 'Convert & save
                ns$ = "" 'One more quick erasing of this variable
                INPUT #4, ns$ 'Grab from background layer two, this time
                Background2((r% + (27 * sl%)), s%) = VAL(ns$) 'Convert & save
                ns$ = "" 'Okay, I lied... I gotta erase it, again!
                INPUT #5, ns$ 'And finally, grab some level data!
                LevelData((r% + (27 * sl%)), s%) = VAL(ns$) 'Convert and save
            NEXT s%
        NEXT r%

    ELSEIF lc% >= 1 THEN '          If the total is at least 256 or more,
        FOR l% = 0 TO (lc% - 1) '   loop as many times as the total number
            FOR r% = 0 TO 26 '      of tiles per row goes into 256.
                FOR s% = 0 TO 255
                    'LOCATE 4, 1: PRINT "DEBUG: Grabbing row" + STR$((r% + (27 * sl%)) + 1) + ", tile" + STR$(256 * l% + (s% + 1)) + " B "
                    INPUT #1, ns$ 'Grab one from the first foreground layer
                    Foreground1((r% + (27 * sl%)), s% + (256 * l%)) = VAL(ns$)
                    ns$ = "" 'Erase what it's set to, just in case
                    INPUT #2, ns$ 'Go for the second foreground layer, now
                    Foreground2((r% + (27 * sl%)), s% + (256 * l%)) = VAL(ns$)
                    ns$ = "" 'Erase it again, Tony
                    INPUT #3, ns$ 'Make a mad dash for background layer one!
                    Background1((r% + (27 * sl%)), s% + (256 * l%)) = VAL(ns$)
                    ns$ = "" 'Blankety-blank-blank it out
                    INPUT #4, ns$ 'Take something from background layer two!
                    Background2((r% + (27 * sl%)), s% + (256 * l%)) = VAL(ns$)
                    ns$ = "" 'I sense a serious need for some blankage.
                    INPUT #5, ns$ 'And a-reach-in and-a grab-a-level data!
                    LevelData((r% + (27 * sl%)), s% + (256 * l%)) = VAL(ns$)
                NEXT s%
            NEXT r%
        NEXT l%

        'Now, if there was at least 256 or more, but still some left afterward,
        'let's run through one more loop, to get the remaining sprites.

        IF rc% > 0 THEN 'Any sprites left after reading each multiple of 256?
            FOR r% = 0 TO 26
                FOR s% = 0 TO ((((grab& / 27) / scrcnt%) - (256 * lc%)) - 1)
                    'LOCATE 4, 1: PRINT "DEBUG: Grabbing row" + STR$((r% + (27 * sl%)) + 1) + ", tile" + STR$((256 * lc%) + s% + 1) + " BC "
                    INPUT #1, ns$ 'Grab a sprite
                    Foreground1((r% + (27 * sl%)), ((256 * lc%) + s%)) = VAL(ns$)
                    ns$ = "" 'Clear the variable, because I'm paranoid
                    INPUT #2, ns$ 'Grab another sprite
                    Foreground2((r% + (27 * sl%)), ((256 * lc%) + s%)) = VAL(ns$)
                    ns$ = "" 'Clear that thing! Get it far away from me! lol
                    INPUT #3, ns$ 'Grab yet another sprite
                    Background1((r% + (27 * sl%)), ((256 * lc%) + s%)) = VAL(ns$)
                    ns$ = "" 'Erase it! Erase it, I say!
                    INPUT #4, ns$ 'Grab one more sprite!
                    Background2((r% + (27 * sl%)), ((256 * lc%) + s%)) = VAL(ns$)
                    ns$ = "" 'It casts magic! Therefore, it must be erased!
                    INPUT #5, ns$ 'Enough with the sprites! Level data, now!
                    LevelData((r% + (27 * sl%)), ((256 * lc%) + s%)) = VAL(ns$)
                NEXT s%
            NEXT r%
        END IF

    ELSE PRINT "Double-check what LC% and RC% are set to!": END
    END IF

NEXT sl%

CLOSE 1, 2, 3, 4, 5, 6, 7

'Pre drawing routine loop routines -- setting up variables and arrays
IF res = 0 THEN
    DIM SHARED sprseg(0 TO 41) AS INTEGER 'How much of each column is on-screen
    DIM SHARED sprnum(0 TO 41) AS INTEGER 'Which sprites are on the screen
    DIM SHARED sprpos(0 TO 41) AS INTEGER 'The X coordinate of each sprite column
    DIM SHARED verrow(0 TO 28) AS INTEGER 'The Y coordinate of each row of sprites
END IF
FOR n = 0 TO 41
    sprseg(n) = 8
    sprnum(n) = (n - 1)
    sprpos(n) = ((n - 1) * 8)
NEXT n
FOR n = 0 TO 28: verrow(n) = ((n - 1) * 8): NEXT n

'Load the test character's basic sprites into memory (change for alpha 11: this was a suggestion...)
IF res = 0 THEN DIM SHARED CrickMov(0 TO 8) AS LONG 'Standing, and walking sprites
IF res = 0 THEN DIM SHARED CrickJmp(0 TO 7) AS LONG 'Jumping sprites (just for the thrill of it)
IF res = 0 THEN DIM SHARED CrickAtk(0 TO 5) AS LONG 'Attacking sprites (so I can get that mechanic working)
destpath$ = respath$ + chrfldr$ + "JC-Stand0.png"
CrickMov(0) = _LOADIMAGE(destpath$)
FOR ws = 0 TO 6
    destpath$ = respath$ + chrfldr$ + "JC-Walk" + LTRIM$(STR$(ws)) + ".png"
    CrickMov(ws + 1) = _LOADIMAGE(destpath$)
NEXT ws
FOR ws = 0 TO 7
    destpath$ = respath$ + chrfldr$ + "JC-Jump" + LTRIM$(STR$(ws)) + ".png"
    CrickJmp(ws) = _LOADIMAGE(destpath$)
NEXT ws
'jump& = _LOADIMAGE(destpath$)
FOR ws = 0 TO 5
    destpath$ = respath$ + chrfldr$ + "JC-Attack" + LTRIM$(STR$(ws)) + ".png"
    CrickAtk(ws) = _LOADIMAGE(destpath$)
NEXT ws
destpath$ = respath$ + chrfldr$ + "KTE-Climb0.png" 'Everyone's favorite Disney-inspired interjection doesn't climb?
climb& = _LOADIMAGE(destpath$)
plyr& = CrickMov(0)
mf = 0

' With the "GO, CRICKET!" message on the screen, using the
' TimeIsNow!/ClockCheck!/WhatIsTime! method mentioned earlier, the game either
' waits for the player to press ENTER, or keeps going, after 3 seconds.
LET TimeIsNow! = TIMER
DO WHILE WhatIsTime! < 3! AND NOT _KEYDOWN(13)
    LET ClockCheck! = TIMER
    LET WhatIsTime! = ClockCheck! - TimeIsNow!
    IF _KEYDOWN(13) THEN EXIT DO
LOOP

FadeOut 'Then it just fades out the entire screen.
LevelStart = 1 'This is so it only fades in once, and doesn't cause a glitch.

'Define Cricket's coordinates, for detecting sprite collision later on.
'TODO: Redefine these for collision detection, after I finally get the sprite
'      plane (and any other extra foreground/background planes) implemented.
'CKL% = 0 '                           Coordinate of Cricket's left border
CKR% = CKL% + _WIDTH(plyr&) - 1 '  Coordinate of Cricket's right border
'CKT% = 24 '                 Coordinate of the very top of Cricket's head
CKB% = CKT% + _HEIGHT(plyr&) - 1 ' Coordinate of the very bottom of Cricket's feet
CKX% = CKL% '       Global X coordinate, for absolute positioning on the map

'**** Change for alpha 12: Bye-bye ON TIMER-style "ClockChange", hello tenth-of-a-second-to-the-clock timing

IF res = 0 THEN
    DIM UnderFoot(0 TO 3) AS INTEGER
    DIM AboveHead(0 TO 3) AS INTEGER
    DIM InFrontOf(0 TO 4) AS INTEGER
    DIM BehindMoi(0 TO 4) AS INTEGER
ELSE
    res = 0
END IF

LET TimeIsNow# = TIMER: fc = 0: atk = -1: aa = 0
DO

    _LIMIT 180 'This is good for a Core i7, but what about something slower?
    LET ClockCheck# = TIMER

    'STEP 1: Draw the playfield, and the buffer columns and rows
    'TODO: Prevent the game from crashing if one of the background or foreground layers uses a tile that isn't loaded.
    FOR y = 0 TO 28
        FOR x = 0 TO 41 '(tsc - 1)
            'LOCATE 1, 1: PRINT "COLUMN: " + LTRIM$(STR$(w)) + "  ROW: " + LTRIM$(STR$(y)) + "  LEFTMOST: " + STR$(sprpos(0))
            IF sprnum(x) >= 0 AND verrow(y) < 216 THEN
                'IF y = 0 AND (vert% - 1) = -1 THEN ELSE _PUTIMAGE (sprpos(x), 24 + verrow(y)), spritedata(Background1((vert% - 1) + y, sprnum(x)), 0)
                IF y = 0 AND (vert% - 1) = -1 THEN ELSE PUT (sprpos(x), 24 + verrow(y)), spritedata(f * 80, Background1((vert% - 1) + y, sprnum(x))), _CLIP PSET
                'IF y = 0 AND (vert% - 1) = -1 THEN ELSE _PUTIMAGE (sprpos(x), 24 + verrow(y)), spritedata(Background2((vert% - 1) + y, sprnum(x)), 0)
                IF y = 0 AND (vert% - 1) = -1 THEN ELSE PUT (sprpos(x), 24 + verrow(y)), spritedata(f * 80, Background2((vert% - 1) + y, sprnum(x))), _CLIP PSET, _RGB32(1, 1, 1)
                'IF y = 0 AND (vert% - 1) = -1 THEN ELSE _PUTIMAGE (sprpos(x), 24 + verrow(y)), spritedata(Foreground1((vert% - 1) + y, sprnum(x)), 0)
                IF y = 0 AND (vert% - 1) = -1 THEN ELSE PUT (sprpos(x), 24 + verrow(y)), spritedata(f * 80, Foreground1((vert% - 1) + y, sprnum(x))), _CLIP PSET, _RGB32(1, 1, 1)
                'IF y = 0 AND (vert% - 1) = -1 THEN ELSE _PUTIMAGE (sprpos(x), 24 + verrow(y)), spritedata(Foreground2((vert% - 1) + y, sprnum(x)), 0)
                IF y = 0 AND (vert% - 1) = -1 THEN ELSE PUT (sprpos(x), 24 + verrow(y)), spritedata(f * 80, Foreground2((vert% - 1) + y, sprnum(x))), _CLIP PSET, _RGB32(1, 1, 1)
            END IF
        NEXT x
    NEXT y

    'STEP 2: Check for a sprite collision, then draw the player and enemies
    FOR clr = 0 TO 3 'Reset all the arrays and variables, to prevent a bug
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
        CASE IS < 32
            PRow = 0
        CASE IS < 40
            PRow = 1
        CASE IS < 48
            PRow = 2
        CASE IS < 56
            PRow = 3
        CASE IS < 64
            PRow = 4
        CASE IS < 72
            PRow = 5
        CASE IS < 80
            PRow = 6
        CASE IS < 88
            PRow = 7
        CASE IS < 96
            PRow = 8
        CASE IS < 104
            PRow = 9
        CASE IS < 112
            PRow = 10
        CASE IS < 120
            PRow = 11
        CASE IS < 128
            PRow = 12
        CASE IS < 136
            PRow = 13
        CASE IS < 144
            PRow = 14
        CASE IS < 152
            PRow = 15
        CASE IS < 160
            PRow = 16
        CASE IS < 168
            PRow = 17
        CASE IS < 176
            PRow = 18
        CASE IS < 184
            PRow = 19
        CASE IS < 192
            PRow = 20
        CASE IS < 200
            PRow = 21
        CASE IS < 208
            PRow = 22
        CASE IS < 216
            PRow = 23
        CASE IS < 224
            PRow = 24
        CASE IS < 232
            PRow = 25
        CASE IS < 240
            PRow = 26
        CASE IS < 248
            PRow = 27
        CASE IS < 256
            PRow = 28
        CASE IS < 264
            PRow = 29
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
    IF oco THEN oco = 0
    IF jump% = 0 THEN jdrop = 1
    IF jdrop THEN 'Only check what's under Cricket's feet if he's not jumping
        FOR Foot = 0 TO (sn - 1) 'The number of sprites below Cricket's feet
            IF PRow < 27 THEN 'Bugfix: don't check what's below Cricket's feet, if he fell off the lowest screen.
                IF LevelData(vert% + PRow, UnderFoot(Foot)) = 1 THEN 'Platform
                    'IF jump% = -2 THEN oco = oco + 1 ELSE
                    IF CKB% MOD 8 = 7 THEN jdrop = 0: jump% = 0 ' mf = 0
                    IF cont% = 0 AND jpush = 1 THEN
                        IF NOT _KEYDOWN(MoveJump) THEN jpush = 0
                        plyr& = CrickMov(mf): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1: IF CKB% MOD 8 = 0 THEN CKT% = CKT% - 1: CKB% = CKB% - 1
                    ELSEIF cont% > 0 AND jpush = 1 THEN
                        IF JoyMoveOp(5) > 0 AND MoveJump < 128 THEN
                            IF STICK(ActionJump, JoyMoveOp(5)) > MoveJump THEN jpush = 0
                            plyr& = CrickMov(mf): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1: IF CKB% MOD 8 = 0 THEN CKT% = CKT% - 1: CKB% = CKB% - 1
                        ELSEIF JoyMoveOp(5) > 0 AND MoveJump > 128 THEN
                            IF STICK(ActionJump, JoyMoveOp(5)) < MoveJump THEN jpush = 0
                            plyr& = CrickMov(mf): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1: IF CKB% MOD 8 = 0 THEN CKT% = CKT% - 1: CKB% = CKB% - 1
                        ELSE
                            IF JoyMoveOp(5) = 0 AND STRIG(MoveJump, ActionJump) = 0 THEN jpush = 0
                            plyr& = CrickMov(mf): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1: IF CKB% MOD 8 = 0 THEN CKT% = CKT% - 1: CKB% = CKB% - 1
                        END IF
                    END IF
                ELSEIF LevelData(vert% + PRow, UnderFoot(Foot)) = 2 THEN 'Wall
                    'IF jump% = -2 THEN oco = oco + 1 ELSE
                    IF CKB% MOD 8 = 7 THEN jdrop = 0: jump% = 0 ' mf = 0
                    IF cont% = 0 AND jpush = 1 THEN
                        IF NOT _KEYDOWN(MoveJump) THEN jpush = 0
                        plyr& = CrickMov(mf): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1: IF CKB% MOD 8 = 0 THEN CKT% = CKT% - 1: CKB% = CKB% - 1
                    ELSEIF cont% > 0 AND jpush = 1 THEN
                        IF JoyMoveOp(5) > 0 AND MoveJump < 128 THEN
                            IF STICK(ActionJump, JoyMoveOp(5)) > MoveJump THEN jpush = 0
                            plyr& = CrickMov(mf): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1: IF CKB% MOD 8 = 0 THEN CKT% = CKT% - 1: CKB% = CKB% - 1
                        ELSEIF JoyMoveOp(5) > 0 AND MoveJump > 128 THEN
                            IF STICK(ActionJump, JoyMoveOp(5)) < MoveJump THEN jpush = 0
                            plyr& = CrickMov(mf): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1: IF CKB% MOD 8 = 0 THEN CKT% = CKT% - 1: CKB% = CKB% - 1
                        ELSE
                            IF JoyMoveOp(5) = 0 AND STRIG(MoveJump, ActionJump) = 0 THEN jpush = 0
                            plyr& = CrickMov(mf): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1: IF CKB% MOD 8 = 0 THEN CKT% = CKT% - 1: CKB% = CKB% - 1
                        END IF
                    END IF
                END IF
                IF jump% = -2 AND LevelData(vert% + PRow, UnderFoot(Foot)) <> 3 THEN
                    oco = oco + 1 'Also check to see if Cricket's climbing something
                END IF
            END IF
            IF oco > 2 THEN jump% = 0: jdrop = 1 'If he isn't, make him fall
        NEXT Foot
    END IF

    'Quick check: did Cricket jump or fall off the screen?
    IF CKT% > 240 AND slidecount% = 0 AND (vert% + 27) / 27 = scrcnt% THEN
        'If he fell off the bottom screen, then that's a death.
        jdrop = 0 'Stop him from falling
        jump% = -1 'Reset the height counter
        IF actbgm% >= 0 AND bgm& THEN _SNDSTOP bgm& 'Stop the background music (bugfix: works even when BGM is off)
        _SNDPLAY SEF(3) 'Play the death sound
        CKT% = 240: CKB% = (CKT% + _HEIGHT(plyr&) - 1) 'Move Cricket, so the sound only plays once
        IF lives > 0 THEN lives = lives - 1 'Take away a life
        'At some point, the game will jump to the "try again?" screen.
        'For now, all you have to do is press F12+R to revive Cricket.
    ELSEIF CKT% >= 236 AND slidecount% = 0 AND (vert% + 27) / 27 <> scrcnt% THEN
        'But if he didn't, we'll scroll the screen up!
        slidecount% = 216
        'jdrop = 0
        CKT% = CKT% - 1: CKB% = (CKT% + _HEIGHT(plyr&) - 1)
    ELSEIF CKB% <= 40 AND slidecount% = 0 AND (vert% + 27) / 27 > 1 THEN
        'And if he hits the top of the screen, let's scroll it down!
        slidecount% = -216
        'jdrop = 0
        CKT% = CKT% + 1: CKB% = (CKT% + _HEIGHT(plyr&) - 1)
        'TODO: Make sure that, if he jumps, he won't fall after it scrolls.
    END IF

    'Sprite positioning routine -- draw the player, and each enemy in sight
    '** MONSTERS/BOSSES/ACTION TRIGGERS (BUG: Enemies on leftmost column disappear when player's "back is turned")
    FOR q = 0 TO 27 'What screen row are we on? Are there monsters on that screen row, too?
        FOR o = 0 TO (atm - 1) 'Run through each monster in memory, and check its coordinates
            IF (vert% + q) = VAL(TrigPos(o, 2)) THEN 'Are we on the same screen row as this monster?
                FOR p = 0 TO 41: IF sprnum(p) = VAL(TrigPos(o, 3)) THEN 'If so, what screen column is it on?
                        IF MnS(o, 5) = 0 THEN 'If it hasn't been spawned, or just scrolled off the screen...
                            MnS(o, 5) = 1 'Spawn or respawn it (moving off-screen "unspawns" a monster)
                            MnS(o, 0) = p * 8
                            MnS(o, 1) = MnS(o, 0) + (_WIDTH(MonsterAnim(VAL(TrigPos(o, 0)), 0, 0)) - 1)
                            MnS(o, 3) = 24 + (q * 8)
                            MnS(o, 2) = MnS(o, 3) - (_HEIGHT(MonsterAnim(VAL(TrigPos(o, 0)), 0, 0)) - 1)
                        END IF
                        IF TrigPos(o, 1) = "R" THEN _PUTIMAGE (MnS(o, 0), MnS(o, 2) - 1), MonsterAnim(VAL(TrigPos(o, 0)), 0, 0) ELSE _PUTIMAGE (MnS(o, 1), MnS(o, 2) - 1)-(MnS(o, 0), MnS(o, 3) - 1), MonsterAnim(VAL(TrigPos(o, 0)), 0, 0)
                        DRAW "C" + STR$(&HFFFFFFFF) + " BM" + STR$(MnS(o, 1) + 6) + "," + STR$(MnS(o, 2)): Font "L" + LTRIM$(STR$(MnS(o, 0))) + " R" + LTRIM$(STR$(MnS(o, 1))) + " T" + LTRIM$(STR$(MnS(o, 2))) + " B" + LTRIM$(STR$(MnS(o, 3))) + " X" + LTRIM$(STR$(MnS(o, 4)))
                    END IF
                NEXT p
            END IF
        NEXT o
    NEXT q
    '_SNDPLAYFILE "/media/Lexar/PROJECTS/CRICKET/SOUND/MESSAGE-B_Accept.wav", 1, 1: jk = 1

    '** CRICKET KAMODON
    IF pf THEN 'I WAS using Knuckles the Echidna to test this, but my brother suggested someone different...
        _PUTIMAGE (CKR%, CKT%)-(CKL%, CKB%), plyr& 'Let your conscience face left!
    ELSEIF NOT pf THEN
        _PUTIMAGE (CKL%, CKT% + 1), plyr& 'Let your conscience face right!
    END IF

    'STEP 3: Check to see if the game has just finished loading this level
    'Debug and state information, such as character positioning (toggle on/off with F12+I in-game)
    IF dh THEN
        DRAW "C" + STR$(&HFFFFFFFF) + " BM30,33": Font STR$(CKT%)
        DRAW "C" + STR$(&HFFFFFFFF) + " BM2,43": Font STR$(CKL%)
        DRAW "C" + STR$(&HFFFFFFFF) + " BM60,43": Font STR$(CKR%)
        DRAW "C" + STR$(&HFFFFFFFF) + " BM30,53": Font STR$(CKB%)
        DRAW "C" + STR$(&HFFFFFF77) + " BM30,43": Font STR$(CKX%)
        DRAW "C" + STR$(&HFFFFFFFF) + " BM90,33": Font "HEIGHT" + STR$(jump%) + STR$(jdrop) + STR$(jpush)
        DRAW "C" + STR$(&HFFFFFFFF) + " BM180,33": Font STR$(sn) + " TILES UNDER FEET"
        DRAW "C" + STR$(&HFFFFFFFF) + " BM90,43": Font "ABOVE AND BELOW" + STR$(UnderFoot(0)) + "," + STR$(UnderFoot(1)) + "," + STR$(UnderFoot(2)) + "," + STR$(UnderFoot(3))
        DRAW "C" + STR$(&HFFFFFFFF) + " BM90,53": Font "IN-BETWEEN" + STR$(InFrontOf(0)) + "," + STR$(InFrontOf(1)) + "," + STR$(InFrontOf(2)) + "," + STR$(InFrontOf(3)) + "," + STR$(InFrontOf(4))
        DRAW "C" + STR$(&HFFFFFFFF) + " BM1,63": Font "DOOR TILES - LEFT" + STR$(Ldoor) + "," + STR$(LevelData(Vpos, Ldoor)) + " RIGHT" + STR$(Rdoor) + "," + STR$(LevelData(Vpos, Rdoor))
        DRAW "C" + STR$(&HFFFFFFFF) + " BM1,238": Font "SCREEN UPDATES LAST SECOND" + STR$(fps)
    END IF

    'And now, to redraw the HUD! (That bar at the top of the game screen)
    LINE (0, 0)-(319, 22), _RGBA32(0, 255, 0, 255), BF
    LINE (0, 0)-(40, 22), _RGBA32(0, 0, 127, 255), BF
    LINE (160, 0)-(212, 11), _RGBA32(0, 0, 127, 255), BF
    LINE (245, 0)-(285, 11), _RGBA32(0, 0, 127, 255), BF
    LINE (120, 12)-(150, 22), _RGBA32(0, 0, 127, 255), BF
    LINE (245, 12)-(319, 22), _RGBA32(0, 127, 0, 255), BF
    LINE (0, 23)-(319, 23), 0
    DRAW "C" + STR$(&HFFFFFFFF) + " BM7,8"
    Font "ZONE"
    DRAW "C" + STR$(&HFF000000) + " BM44,8"
    Font zone$
    DRAW "C" + STR$(&HFFFFFFFF) + " BM164,8"
    Font "CRICKET"
    IF LEN(LTRIM$(STR$(lives))) = 3 THEN 'Over 100 lives! Somebody's GOOD!
        IF LEFT$(LTRIM$(STR$(lives)), 1) = "1" THEN DRAW "C" + STR$(&HFF000000) + " BM220,8" ELSE DRAW "C" + STR$(&HFF000000) + " BM219,8"
        Font LTRIM$(STR$(lives))
    ELSEIF LEN(LTRIM$(STR$(lives))) = 2 THEN 'Anywhere from 10 to 99 lives
        IF LEFT$(LTRIM$(STR$(lives)), 1) = "1" THEN DRAW "C" + STR$(&HFF000000) + " BM223,8" ELSE DRAW "C" + STR$(&HFF000000) + " BM222,8"
        Font LTRIM$(STR$(lives))
    ELSE 'Anywhere from 0 to 9 lives, either he's a cat-cricket, or not. xD
        IF LTRIM$(STR$(lives)) = "1" THEN DRAW "C" + STR$(&HFF000000) + " BM227,8" ELSE DRAW "C" + STR$(&HFF000000) + " BM226,8"
        Font LTRIM$(STR$(lives))
    END IF
    DRAW "C" + STR$(&HFFFFFFFF) + " BM252,8"
    Font "TIME"
    IF tick < 60 THEN LINE (286, 0)-(320, 11), _RGBA32(175, 0, 0, 255), BF ELSE LINE (286, 0)-(320, 11), _RGBA32(0, 255, 0, 255), BF
    DRAW "B M293,8"
    IF tick < 10 THEN
        DRAW "C" + STR$(&HFFFFFFFF): Font "00" + LTRIM$(STR$(tick))
    ELSEIF tick < 100 THEN
        IF tick < 60 THEN DRAW "C" + STR$(&HFFFFFFFF) ELSE DRAW "C" + STR$(&HFF000000)
        Font "0" + LTRIM$(STR$(tick))
    ELSE
        DRAW "C" + STR$(&HFF000000): Font LTRIM$(STR$(tick))
    END IF
    DRAW "C" + STR$(&HFFFFFFFF) + " BM3,20"
    Font "SCORE" 'Long integer. Being very prepared. xD
    DRAW "C" + STR$(&HFF000000) + " BM" + STR$(119 - (8 * LEN(LTRIM$(STR$(score&))))) + ",20"
    Font LTRIM$(STR$(score&))
    DRAW "C" + STR$(&HFFFFFFFF) + " BM125,20"
    Font "AMP"
    IF amp% > 0 THEN LINE (158, 14)-((158 + amp%), 20), _RGBA32(amp% * 3, amp% * 1.5, (240 - (amp% * 3)), 255), BF
    DRAW "C" + STR$(&HFF000000) + " BM261,20"
    Font "NORMAL"
    LINE (0, 23)-(319, 23), _RGBA32(0, 0, 0, 255)

    IF LevelStart = 1 THEN ' ONLY do this if the screen is just fading in.
        IF actbgm% >= 0 AND bgm& THEN _SNDLOOP bgm& 'Start the BGM if we can!
        FadeIn ' Make the screen fade in from black.
        LevelStart = 0 ' Make it so it doesn't keep fading in with each loop.
        DO WHILE _KEYHIT ' And this clears the keyboard buffer.
        LOOP
    ELSEIF LevelStart = 2 THEN
        cur& = _COPYIMAGE(0) 'Take a picture of the new playfield area.
        FOR za = 0 TO 160
            '_PUTIMAGE (0, 0), cur&
            '_SETALPHA za, 0 TO _RGBA(255, 255, 255, 255), pic&
            '_PUTIMAGE (lft, top)-(rgt, bot), pic&
            LINE (0, 0)-(319, 239), _RGB32(0, 0, 0), BF
            _PUTIMAGE (lft - za, top)-(rgt + za, bot), cur&
            _DISPLAY
        NEXT za
        LevelStart = 0
    END IF

    'LOCATE 1, 1: PRINT STR$(vert%) + " ... " + STR$(MnS(0, 0)) + " ... " + STR$(MnS(0, 2))
    _DISPLAY 'Only update the screen for the map/players/monsters, or the HUD

    'STEP 4: Check to see if we have to scroll the screen upward or downward

    'This routine makes the screen scroll (and stops everything else from
    'moving, player or creature) one row of pixels for each number in the
    '"slidecount%" integer. Once it hits zero, scrolling stops.
    IF slidecount% < 0 THEN 'Are we scrolling the screen downward?
        IF jdrop THEN jdrop = 0: jdset = 1
        FOR v = 0 TO 28: verrow(v) = verrow(v) + 1: NEXT v
        slidecount% = slidecount% + 1
        CKT% = CKT% + 1
        CKB% = CKB% + 1
        FOR n = 0 TO (atm - 1)
            IF MnS(n, 5) = 1 THEN
                MnS(n, 2) = MnS(n, 2) + 1
                MnS(n, 3) = MnS(n, 3) + 1
                IF MnS(n, 2) >= 240 THEN MnS(n, 5) = 0
            END IF
        NEXT n
        IF verrow(0) >= 0 THEN
            FOR v = 0 TO 28: verrow(v) = verrow(v) - 8: NEXT v
            vert% = vert% - 1
            'IF MnS(0, 5) = 1 THEN swp = VAL(TrigPos(0, 2)): swp = swp - 1: TrigPos(0, 2) = STR$(swp)
        END IF
        IF slidecount% = 0 AND jdset THEN jdrop = 1: jdset = 0
    ELSEIF slidecount% > 0 THEN 'Or are we scrolling the screen upward?
        IF jdrop THEN jdrop = 0: jdset = 1
        FOR v = 0 TO 28: verrow(v) = verrow(v) - 1: NEXT v
        slidecount% = slidecount% - 1
        CKT% = CKT% - 1
        CKB% = CKB% - 1
        FOR n = 0 TO (atm - 1)
            IF MnS(n, 5) = 1 THEN
                MnS(n, 2) = MnS(n, 2) - 1
                MnS(n, 3) = MnS(n, 3) - 1
                IF MnS(n, 3) <= 23 THEN MnS(n, 5) = 0
            END IF
        NEXT n
        IF verrow(0) <= -16 THEN
            FOR v = 0 TO 28: verrow(v) = verrow(v) + 8: NEXT v
            vert% = vert% + 1
            'IF MnS(0, 5) = 1 THEN swp = VAL(TrigPos(0, 2)): swp = swp + 1: TrigPos(0, 2) = STR$(swp)
        END IF
        IF slidecount% = 0 AND jdset THEN jdrop = 1: jdset = 0
    END IF

    'STEP 5: Act out the next step in each monster's AI routine (this is a work-in-progress)
    fs = fs + 1: qp = 0: pq = 0
    IF fs = 5 THEN 'Bugfix: all other enemies keep moving, even after one has stopped, for whatever reason
        FOR ai = 0 TO (atm - 1)
            IF slidecount% = 0 AND MnS(ai, 5) = 1 THEN
                'LOCATE 1, 1: PRINT "FYRE!!!": _DISPLAY
                'IF MnS(ai, 4) > 0 THEN '<-- See if moving where this is checked breaks anything.
                IF TrigPos(ai, 1) = "L" THEN
                    'LOCATE 2, 1: PRINT "PUT IT OUT!!!": _DISPLAY
                    'qp = VAL(TrigPos(ai, 2)): pq = VAL(TrigPos(ai, 3))
                    qp = (MnS(ai, 3) - 32) / 8: pq = MnS(ai, 0) / 8
                    LOCATE 1, 1: PRINT STR$(qp): LOCATE 2, 1: PRINT STR$(pq): LOCATE 3, 1: PRINT STR$(LevelData(vert% + qp, pq - 1)): LOCATE 4, 1: PRINT STR$(LevelData(vert% + (qp + 1), pq - 1)): _DISPLAY
                    IF MnS(ai, 3) MOD 8 > 3 THEN qp = qp - 1
                    IF MnS(ai, 0) MOD 8 > 3 THEN pq = pq - 1
                    IF MnS(ai, 4) = 0 OR pq = 0 THEN 'Found the leftmost side of the map? Turn around.
                        TrigPos(ai, 1) = "R"
                    ELSEIF LevelData(vert% + (qp + 1), pq) < 1 OR LevelData(vert% + (qp + 1), pq) > 2 THEN
                        'Found the edge of the platform/wall? Turn around.
                        'MnS(ai, 0) = MnS(ai, 0) + 1: MnS(ai, 1) = MnS(ai, 1) + 1: MnS(ai, 4) = MnS(ai, 4) + 1
                        TrigPos(ai, 1) = "R"
                    ELSEIF LevelData(vert% + qp, pq - 1) = 2 THEN 'A wall in front of you? Turn around.
                        'MnS(ai, 0) = MnS(ai, 0) + 1: MnS(ai, 1) = MnS(ai, 1) + 1: MnS(ai, 4) = MnS(ai, 4) + 1
                        TrigPos(ai, 1) = "R"
                    ELSE 'Nothing stopping you from moving? Keep going.
                        MnS(ai, 0) = MnS(ai, 0) - 1
                        MnS(ai, 1) = MnS(ai, 1) - 1
                        MnS(ai, 4) = MnS(ai, 4) - 1
                    END IF
                    IF MnS(ai, 4) MOD 8 = 0 THEN g = VAL(TrigPos(ai, 3)): g = g - 1: TrigPos(ai, 3) = STR$(g)
                    'MnP(ai) = MnP(ai) + 1: IF MnP(ai) = 0 THEN TrigPos(ai, 1) = "R"
                ELSEIF TrigPos(ai, 1) = "R" THEN
                    'LOCATE 2, 1: PRINT "IT'S STILL BURNING!!!": _DISPLAY
                    'qp = VAL(TrigPos(ai, 2)): pq = VAL(TrigPos(ai, 3))
                    qp = (MnS(ai, 3) - 32) / 8: pq = MnS(ai, 1) / 8
                    LOCATE 1, 1: PRINT STR$(qp): LOCATE 2, 1: PRINT STR$(pq): LOCATE 3, 1: PRINT STR$(LevelData(vert% + qp, pq + 1)): LOCATE 4, 1: PRINT STR$(LevelData(vert% + (qp + 1), pq + 1)): _DISPLAY
                    IF MnS(ai, 3) MOD 8 > 3 THEN qp = qp - 1
                    IF MnS(ai, 1) MOD 8 > 3 THEN pq = pq - 1
                    IF LevelData(vert% + (qp + 1), pq) < 1 OR LevelData(vert% + (qp + 1), pq) > 2 THEN
                        'Found the edge of the platform/wall? Turn around.
                        'MnS(ai, 0) = MnS(ai, 0) - 1: MnS(ai, 1) = MnS(ai, 1) - 1: MnS(ai, 4) = MnS(ai, 4) - 1
                        TrigPos(ai, 1) = "L"
                    ELSEIF LevelData(vert% + qp, pq) = 2 THEN 'A wall in front of you? Turn around.
                        'MnS(ai, 0) = MnS(ai, 0) - 1: MnS(ai, 1) = MnS(ai, 1) - 1: MnS(ai, 4) = MnS(ai, 4) - 1
                        TrigPos(ai, 1) = "L"
                    ELSE 'Nothing stopping you from moving? Keep going.
                        MnS(ai, 0) = MnS(ai, 0) + 1
                        MnS(ai, 1) = MnS(ai, 1) + 1
                        MnS(ai, 4) = MnS(ai, 4) + 1
                    END IF
                    IF MnS(ai, 4) MOD 8 = 0 THEN g = VAL(TrigPos(ai, 3)): g = g + 1: TrigPos(ai, 3) = STR$(g)
                    'MnP(ai) = MnP(ai) + 1: IF MnP(ai) = 48 THEN MnP(ai) = -48: TrigPos(ai, 1) = "L"
                END IF
            END IF
            'END IF
        NEXT ai
        fs = 0
    END IF

    'STEP 6: Check for player input, to decide what to do, next

    'TODO: Figure out how to reset the animation to the standing sprite, if the player stops moving.
    kp& = _KEYHIT 'For any extra keys that don't need to be held down to work.
    IF cont% = 0 THEN GOSUB KeyCtrlCheck ELSE GOSUB JoyCtrlCheck

    '** Change for alpha 11: DEBUG keys are permanently mapped to the keyboard, and moved here.

    IF kp& = MoveExit THEN 'ESC ends our fancy little simulation.
        Sector = 3: Selector = 17: GOSUB PauseGame

    ELSEIF _KEYDOWN(34304) AND kp& = MoveDScrlDn AND slidecount% = 0 THEN 'F12+PLUS scrolls the screen down 27 lines (DEBUG)
        IF vert% < ((27 * scrcnt%) - 27) THEN
            slidecount% = 216
        END IF

    ELSEIF _KEYDOWN(34304) AND kp& = MoveDScrlUp AND slidecount% = 0 THEN 'F12+MINUS scrolls the screen up 27 lines (DEBUG)
        IF vert% > 0 THEN
            slidecount% = -216
        END IF

    ELSEIF _KEYDOWN(34304) AND kp& = 114 THEN 'Pressing F12+R resets Cricket's position (DEBUG key)
        CKT% = 61
        CKB% = CKT% + _HEIGHT(plyr&) - 1
        jdrop = 1
        jump% = 0
        'Should pressing the RESET key also reset jpush?
        IF actbgm% >= 0 THEN IF NOT _SNDPLAYING(bgm&) THEN _SNDLOOP bgm&

    ELSEIF _KEYDOWN(34304) AND kp& = MoveDLife THEN 'Pressing F12+* gives Cricket a 1UP (DEBUG)
        lives = lives + 1
        score& = score& + 100 'Just to test the score box; remove this when monsters are implemented more
        _SNDPLAY SEF(2) 'That's bound to get annoying if I do it a lot

        'F12+E reloads all level data (DEBUG)
    ELSEIF _KEYDOWN(34304) AND _KEYDOWN(101) THEN GOSUB LevelReload

        'F12+T stops/starts the timer (DEBUG)
    ELSEIF _KEYDOWN(34304) AND kp& = 116 THEN
        _SNDPLAY ckHurt&
        LINE (0, 0)-(320, 240), _RGBA32(0, 0, 0, 140), BF
        DRAW "C" + STR$(&HFFFFFFFF)
        IF cstop = 1 THEN CenterFont "THE TIMER HAS BEEN STARTED", 120 ELSE CenterFont "THE TIMER HAS BEEN STOPPED", 120
        IF cstop = 1 THEN cstop = 0 ELSE cstop = 1
        _DISPLAY
        DO: LOOP WHILE _KEYDOWN(34304) OR _KEYDOWN(116)

        'F12+I toggles the debug and state information (DEBUG)
    ELSEIF _KEYDOWN(34304) AND kp& = 105 THEN
        IF dh = 1 THEN dh = 0 ELSE dh = 1

        'F12+PageUp increases the AMP bar (DEBUG)
    ELSEIF _KEYDOWN(34304) AND kp& = 18688 THEN
        _SNDPLAY SEF(12)
        IF amp% < 80 THEN amp% = amp% + 2
        score& = score& + 10 'Just to test the score box; remove this when monsters are implemented more

        'F12+PageDown decreases the AMP bar (DEBUG)
    ELSEIF _KEYDOWN(34304) AND kp& = 20736 THEN
        IF amp% > 0 THEN amp% = amp% - 2

    END IF

    fc = fc + 1
    LET WhatIsTime# = ClockCheck# - TimeIsNow#
    IF WhatIsTime# >= .01 THEN
        csc = csc + 1
        IF csc MOD 2 = 0 THEN f = f + 1: IF f = 16 THEN f = 0
        IF csc = 18 THEN
            fps = fc: fc = 0
            IF cstop = 0 AND tick > 0 THEN GOSUB ClockChange
            csc = 0
        END IF
        TimeIsNow# = TIMER: ClockCheck# = TIMER: WhatIsTime# = 0
    END IF
LOOP

SYSTEM ' To stop it from running farther than it should.

'** INPUT CHECKING SUBROUTINE -> KEYBOARD
KeyCtrlCheck:
'********************
' Move Left
'********************
IF atk = -1 AND _KEYDOWN(MoveLeft) AND slidecount% = 0 THEN 'I should probably re-implement running, sometime...
    'IF _KEYDOWN(122) THEN 'Hold down Z
    '    FOR q = 0 TO (tsc - 1)
    '        sprpos(q) = sprpos(q) + 2
    '    NEXT q
    'ELSE
    IF _KEYDOWN(MoveJump) AND jdrop = 0 AND jpush = 0 THEN 'Holding down JUMP
        IF jump% = -2 THEN jump% = 0 'Jump off from a ladder/beanstalk.
        IF jump% = 0 THEN _SNDPLAY SEF(1): plyr& = CrickJmp(0): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1
        IF jump% = 4 THEN plyr& = CrickJmp(1): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1
        IF jump% = 9 THEN plyr& = CrickJmp(2): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1
        IF jump% = 19 THEN plyr& = CrickJmp(3): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1
        IF jump% = 30 THEN plyr& = CrickJmp(4): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1
        IF _KEYDOWN(MoveRun) THEN 'The player jumps higher if it's running!
            IF jump% < 60 THEN
                notop = 0
                IF CKT% <= 23 THEN 'Bugfix. If nothing's above him, he just jumps.
                    CKT% = CKT% - 1: CKB% = CKB% - 1: jump% = jump% + 1
                ELSEIF CKT% > 23 THEN
                    FOR Head = 0 TO sn
                        IF LevelData(vert% + (PRow - 5), AboveHead(Head)) = 2 THEN
                            jdrop = 1 'Stop jumping if something's above his head
                            'Maybe add the sound of him hitting his head?
                        ELSE
                            notop = notop + 1 'One of the tiles isn't a wall
                        END IF
                    NEXT Head 'Below: jump if the 3 or 4 tiles above aren't walls
                    IF notop > 2 THEN CKT% = CKT% - 1: CKB% = CKB% - 1: jump% = jump% + 1
                END IF
            ELSE
                jump% = 0
                jdrop = 1
            END IF
        ELSE
            IF jump% < 42 THEN
                notop = 0
                IF CKT% <= 23 THEN 'Does jumping off the screen at the highest screen level still crash the game?
                    CKT% = CKT% - 1: CKB% = CKB% - 1: jump% = jump% + 1
                ELSEIF CKT% > 23 THEN
                    FOR Head = 0 TO sn
                        IF LevelData(vert% + (PRow - 5), AboveHead(Head)) = 2 THEN
                            jdrop = 1 'Stop jumping if something's above his head
                            'Maybe add the sound of him hitting his head?
                        ELSE
                            notop = notop + 1 'One of the tiles isn't a wall
                        END IF
                    NEXT Head 'Below: jump if the 3 or 4 tiles above aren't walls
                    IF notop > 2 THEN CKT% = CKT% - 1: CKB% = CKB% - 1: jump% = jump% + 1
                END IF
            ELSE
                jump% = 0
                jdrop = 1
            END IF
        END IF
    END IF
    IF kp& = (MoveJump - (MoveJump * 2)) THEN jdrop = 1: jpush = 1 'Let go of the JUMP key early
    IF jdrop THEN 'When you let go of JUMP
        IF jpush = 0 THEN jpush = 1
        IF plyr& <> CrickJmp(6) THEN plyr& = CrickJmp(6): CKB% = CKT% + _HEIGHT(plyr&) - 1: CKR% = CKL% + _WIDTH(plyr&) - 1: mf = 1
        CKT% = CKT% + 1
        CKB% = CKB% + 1
    END IF
    IF kp& = MoveAttack THEN _SNDPLAY SEF(18) 'Just to show that it works.
    'Check for collisions, before we decide if we can move, or not
    IF (vert% + PRow) > 0 THEN
        IF PRow < 27 AND CKT% > 23 THEN
            noblock = 0
            FOR cr = 0 TO so 'I need to make this so if you're halfway off the screen, it doesn't check every wall.
                IF LevelData(vert% + BehindMoi(cr), topleft) <> 2 THEN 'Wall
                    noblock = noblock + 1
                END IF
            NEXT cr
        END IF
    ELSE
        noblock = 4
    END IF
    IF noblock > 3 AND CKX% > 0 THEN 'If nothing's in our way, let's move!
        IF jump% = -2 THEN jump% = 0: plyr& = CrickMov(0): CKB% = CKT% + _HEIGHT(plyr&) - 1: CKR% = CKL% + _WIDTH(plyr&) - 1: mf = 1
        pf = 1 'pf is TRUE, meaning the player is facing LEFT.
        stl = 0 'This checks to see if we can scroll the screen.
        IF sprnum(0) > -1 THEN 'Bugfix: Can't check the type of a tile that doesn't exist...
            FOR wc = 0 TO 26 'If the left-most (off-screen) tile is a STOP tile...
                IF LevelData(vert% + wc, sprnum(0)) = 99 THEN stl = stl + 1
            NEXT wc '...then the screen will stop scrolling to the left.
        END IF
        IF CKL% = 148 AND sprnum(0) > -1 AND stl < 27 THEN 'Scroll the background
            FOR q = 0 TO 41 '(tsc - 1)
                sprpos(q) = sprpos(q) + 1
            NEXT q
            CKX% = CKX% - 1
            IF plyr& = CrickMov(mf) THEN mt = mt + 1
            IF mt = 20 THEN 'Changing this number speeds up, or slows down, the walking animation.
                mt = 0: mf = mf + 1: IF mf = 7 THEN mf = 2
                plyr& = CrickMov(mf): CKT% = CKB% - _HEIGHT(CrickMov(mf)): CKR% = CKL% + _WIDTH(CrickMov(mf)) - 1
            END IF
            FOR n = 0 TO (atm - 1)
                IF MnS(n, 5) = 1 THEN
                    MnS(n, 0) = MnS(n, 0) + 1
                    MnS(n, 1) = MnS(n, 1) + 1
                    IF MnS(n, 0) = MnS(n, 4) OR MnS(n, 4) < 148 THEN MnS(n, 4) = MnS(n, 4) + 1
                    IF MnS(n, 0) >= 320 THEN MnS(n, 5) = 0
                END IF
            NEXT n
            'ELSEIF CKL% < 148 THEN
        ELSEIF CKL% > 0 AND CKL% < 148 OR sprnum(0) = -1 AND sprpos(0) = -8 OR stl = 27 OR str = 27 THEN 'Move Cricket
            CKL% = CKL% - 1: CKR% = CKR% - 1: CKX% = CKX% - 1
            IF plyr& = CrickMov(mf) THEN mt = mt + 1
            IF mt = 20 THEN
                mt = 0: mf = mf + 1: IF mf = 7 THEN mf = 2
                plyr& = CrickMov(mf): CKT% = CKB% - _HEIGHT(CrickMov(mf)): CKR% = CKL% + _WIDTH(CrickMov(mf)) - 1
            END IF
        END IF
    END IF
    'END IF
    IF sprpos(0) >= 0 THEN 'Do we need to reset the column positions?
        FOR q = 0 TO 41
            sprpos(q) = sprpos(q) - 8 'Reset the column positions
            sprnum(q) = sprnum(q) - 1 'Change the displayed sprite
        NEXT q
    END IF
    '*********************
    ' Move Right
    '*********************
ELSEIF atk = -1 AND _KEYDOWN(MoveRight) AND slidecount% = 0 THEN
    'IF _KEYDOWN(122) THEN 'Hold down Z
    '    FOR q = 0 TO (tsc - 1)
    '        sprpos(q) = sprpos(q) - 2
    '    NEXT q
    'ELSE
    IF _KEYDOWN(MoveJump) AND jdrop = 0 AND jpush = 0 THEN 'Holding down JUMP
        IF jump% = -2 THEN jump% = 0 'Jump off from a ladder/beanstalk.
        IF jump% = 0 THEN _SNDPLAY SEF(1): plyr& = CrickJmp(0): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1
        IF jump% = 4 THEN plyr& = CrickJmp(1): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1
        IF jump% = 9 THEN plyr& = CrickJmp(2): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1
        IF jump% = 19 THEN plyr& = CrickJmp(3): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1
        IF jump% = 30 THEN plyr& = CrickJmp(4): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1
        IF _KEYDOWN(MoveRun) THEN 'The player jumps higher if it's running!
            'Maybe copy this routine to the UP and DOWN arrow routines?
            IF jump% < 60 THEN
                notop = 0
                IF CKT% <= 23 THEN 'BUG: Jumping off the screen at the topmost screen level still crashes the game.
                    CKT% = CKT% - 1: CKB% = CKB% - 1: jump% = jump% + 1
                ELSEIF CKT% > 23 THEN
                    FOR Head = 0 TO sn
                        IF LevelData(vert% + (PRow - 5), AboveHead(Head)) = 2 THEN
                            jdrop = 1 'Stop jumping if something's above his head
                            'Maybe add the sound of him hitting his head?
                        ELSE
                            notop = notop + 1 'One of the tiles isn't a wall
                        END IF
                    NEXT Head 'Below: jump if the 3 or 4 tiles above aren't walls
                    IF notop > 2 THEN CKT% = CKT% - 1: CKB% = CKB% - 1: jump% = jump% + 1
                END IF
            ELSE
                jump% = 0
                jdrop = 1
            END IF
        ELSE
            IF jump% < 42 THEN
                notop = 0
                IF CKT% <= 23 THEN 'BUG: Jumping off the screen at the topmost screen level still crashes the game.
                    CKT% = CKT% - 1: CKB% = CKB% - 1: jump% = jump% + 1
                ELSEIF CKT% > 23 THEN
                    FOR Head = 0 TO sn
                        IF LevelData(vert% + (PRow - 5), AboveHead(Head)) = 2 THEN
                            jdrop = 1 'Stop jumping if something's above his head
                            'Maybe add the sound of him hitting his head?
                        ELSE
                            notop = notop + 1 'One of the tiles isn't a wall
                        END IF
                    NEXT Head 'Below: jump if the 3 or 4 tiles above aren't walls
                    IF notop > 2 THEN CKT% = CKT% - 1: CKB% = CKB% - 1: jump% = jump% + 1
                END IF
            ELSE
                jump% = 0
                jdrop = 1
            END IF
        END IF
    END IF
    IF kp& = (MoveJump - (MoveJump * 2)) THEN jdrop = 1: jpush = 1 'Let go of the JUMP key early
    IF jdrop THEN 'When you let go of JUMP
        IF jpush = 0 THEN jpush = 1
        IF plyr& <> CrickJmp(6) THEN plyr& = CrickJmp(6): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1: mf = 1
        CKT% = CKT% + 1
        CKB% = CKB% + 1
    END IF
    IF kp& = MoveAttack THEN _SNDPLAY SEF(18) 'Just to show that it works.
    'Check for collisions, before we decide if we can move, or not
    IF (vert% + PRow) > 0 THEN
        IF PRow < 27 AND CKT% > 23 THEN
            noblock = 0
            FOR cr = 0 TO so
                IF LevelData(vert% + InFrontOf(cr), topright) <> 2 THEN 'Wall
                    noblock = noblock + 1
                END IF
            NEXT cr
        END IF
    ELSE 'Don't check above the player, if they're on the top-most screen.
        noblock = 4
    END IF
    IF noblock > 3 THEN 'If nothing's in our way, where do we move?
        IF jump% = -2 THEN jump% = 0: plyr& = CrickMov(1): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1: mf = 1
        pf = 0 'pf is FALSE, meaning the player is facing RIGHT.
        str = 0 'This checks to see if we can scroll the screen.
        FOR wc = 0 TO 26 'If the right-most (off-screen) tile is a STOP tile...
            IF LevelData(vert% + wc, sprnum(41)) = 99 THEN str = str + 1
        NEXT wc '...then the screen will stop scrolling to the right.
        IF CKL% = 148 AND sprnum(0) >= -1 AND str < 27 THEN
            FOR q = 0 TO 41 '(tsc - 1)
                sprpos(q) = sprpos(q) - 1
            NEXT q
            CKX% = CKX% + 1
            IF plyr& = CrickMov(mf) THEN mt = mt + 1
            IF mt = 20 THEN
                mt = 0: mf = mf + 1: IF mf = 7 THEN mf = 2
                plyr& = CrickMov(mf): CKT% = CKB% - _HEIGHT(CrickMov(mf)): CKR% = CKL% + _WIDTH(CrickMov(mf)) - 1
            END IF
            FOR n = 0 TO (atm - 1)
                IF MnS(n, 5) = 1 THEN
                    MnS(n, 0) = MnS(n, 0) - 1
                    MnS(n, 1) = MnS(n, 1) - 1
                    IF MnS(n, 0) = MnS(n, 4) OR MnS(n, 4) < 148 THEN MnS(n, 4) = MnS(n, 4) - 1
                    IF MnS(n, 1) <= 0 THEN MnS(n, 5) = 0
                END IF
            NEXT n
        ELSEIF CKL% < 148 OR str = 27 AND CKR% < 319 THEN
            CKL% = CKL% + 1: CKR% = CKR% + 1: CKX% = CKX% + 1
            IF plyr& = CrickMov(mf) THEN mt = mt + 1
            IF mt = 20 THEN
                mt = 0: mf = mf + 1: IF mf = 7 THEN mf = 2
                plyr& = CrickMov(mf): CKT% = CKB% - _HEIGHT(CrickMov(mf)): CKR% = CKL% + _WIDTH(CrickMov(mf)) - 1
            END IF
        END IF
    END IF
    'END IF
    IF sprpos(0) <= -8 THEN 'Do we need to reset the column positions?
        FOR q = 0 TO 41
            sprpos(q) = sprpos(q) + 8
            sprnum(q) = sprnum(q) + 1
        NEXT q
    END IF
    '*********************
    ' Move Up
    '*********************
ELSEIF atk = -1 AND _KEYDOWN(MoveUp) AND slidecount% = 0 THEN
    climb = 0
    FOR cc = 0 TO sn
        IF LevelData(vert% + (PRow - 3), AboveHead(cc)) = 3 AND LevelData(vert% + (PRow - 2), AboveHead(cc)) = 3 THEN
            climb = climb + 1 'If Cricket can climb what's above him
        END IF
    NEXT cc 'If he's in front of two climbable tiles, let's climb up!
    IF climb = 2 THEN
        IF plyr& <> climb& THEN plyr& = climb&: CKR% = CKL% + (_WIDTH(plyr&) - 1): CKT% = CKB% - _HEIGHT(plyr&)
        CKT% = CKT% - 1: CKB% = CKB% - 1: jump% = -2: jdrop = 0
    END IF
    Ldoor = 0: Rdoor = 0: Vpos = 0 'Reset the variables
    FOR dc = 0 TO sn 'Check to see if we're standing in front of a door
        IF LevelData(vert% + (PRow - 2), UnderFoot(dc)) >= 100 AND LevelData(vert% + (PRow - 2), UnderFoot(dc)) <= 599 THEN
            IF Ldoor > 0 AND ldset THEN ldset = 0: Rdoor = UnderFoot(dc)
            IF Ldoor = 0 AND Rdoor = 0 THEN Ldoor = UnderFoot(dc): Vpos = (vert% + (PRow - 2)): ldset = 1
        END IF
    NEXT dc
    IF Ldoor > 0 AND Rdoor > 0 THEN 'If you're actually standing on a door
        FOR hc = 0 TO (grab& / (27 * scrcnt%)) 'Start looking for the door
            FOR vc = 0 TO ((27 * scrcnt%) - 1) 'that this door leads to.
                IF vc <> Vpos AND hc <> Ldoor THEN 'Skip the same door.
                    'BUG: The check below this sometimes goes out of range, on smaller levels.
                    IF LevelData(vc, hc) = LevelData(Vpos, Ldoor) AND LevelData(vc, hc + 1) = LevelData(Vpos, Rdoor) THEN
                        'LOCATE 10, 1: PRINT "YOU ARE HERE: " + STR$(Vpos) + "," + STR$(Ldoor)
                        'LOCATE 11, 1: PRINT "DOOR GOES HERE: " + STR$(vc) + "," + STR$(hc): _DISPLAY
                        mc = VAL(RIGHT$(STR$(LevelData(Vpos, Ldoor)), 1))
                        'TIMER OFF
                        lft = 0
                        top = 0
                        rgt = 319
                        bot = 239
                        mv = 11
                        IF pic& THEN _FREEIMAGE pic&
                        pic& = _COPYIMAGE(0)
                        DO
                            LINE (0, 0)-(319, 239), _RGB32(0, 0, 0), BF
                            _PUTIMAGE (lft, top)-(rgt, bot), pic&
                            _DISPLAY
                            IF actbgm% >= 0 AND mc <> actbgm% THEN
                                IF lft MOD 20 = 0 THEN mv = mv - 1
                                _SNDVOL bgm&, (mv / 10)
                                IF mv = 0 THEN _SNDSTOP bgm&
                            END IF
                            IF lft = 160 THEN EXIT DO
                            lft = lft + 1
                            'top = top - 1
                            rgt = rgt - 1
                            'bot = bot + 1
                        LOOP 'COPY THESE CHANGES TO THE GAMEPAD "ENTER DOOR" ROUTINE, WHEN THEY'RE FINISHED!
                        vr = vc MOD 27 'Set the screen level appropriately
                        IF vr > 0 THEN vert% = (vc - vr) ELSE vert% = vc
                        'Reset Cricket's coordinates for the new area
                        CKT% = (vr * 8) - 8: CKB% = CKT% + (_HEIGHT(plyr&) - 1)
                        sll = 0 'Look for any columns full of STOPSCROLL (99) tiles to the LEFT
                        FOR st = (hc - 20) TO hc
                            FOR jj = 0 TO 26
                                IF LevelData(vert% + jj, st) = 99 THEN cfs = cfs + 1
                            NEXT jj
                            IF cfs = 27 THEN sa = sll 'We found one! Make a note of its offset
                            cfs = 0: sll = sll + 1
                        NEXT st
                        hc = hc + sa 'Slide the screen to the right however many rows to lock the left side
                        sa = 0: slr = 21 'Now, look for any columns full of STOPSCROLL (99) tiles to the RIGHT
                        FOR st = (hc + 1) TO (hc + 21)
                            FOR jj = 0 TO 26
                                IF LevelData(vert% + jj, st) = 99 THEN cfs = cfs + 1
                            NEXT jj
                            IF cfs = 27 THEN sa = slr 'We found one! Make a note of its offset
                            cfs = 0: slr = slr - 1
                        NEXT st
                        hc = hc - sa 'Slide the screen to the left however many rows to lock the right side
                        CKX% = hc * 8 'Set Cricket's global X coordinate
                        IF CKX% >= 148 THEN CKL% = 148 ELSE CKL% = CKX% 'Should change if one or both sides are locked
                        CKR% = CKL% + _WIDTH(plyr&) - 1 'And L/R coords
                        FOR s = 0 TO 41: sprpos(s) = ((s - 1) * 8): NEXT s
                        FOR lh = (hc - 20) TO hc 'Shift the background to
                            sprnum(c) = lh '      the correct position
                            c = c + 1
                        NEXT lh
                        FOR rh = (hc + 1) TO (hc + 21) 'Same as above
                            sprnum(c) = rh
                            c = c + 1
                        NEXT rh
                        c = 0
                        DO WHILE _KEYDOWN(MoveUp): LOOP 'Let go of the key!
                        LevelStart = 2
                        IF actbgm% >= 0 AND mc <> actbgm% THEN
                            _SNDCLOSE bgm&
                            bgm& = _SNDOPEN(BGM(mc), "VOL,PAUSE")
                            _SNDLOOP bgm&
                            actbgm% = mc
                        ELSEIF actbgm% = -1 THEN prevbgm% = mc
                        END IF
                        IF plyr& <> CrickMov(0) THEN plyr& = CrickMov(0): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1: mf = 1
                        EXIT FOR
                    END IF
                END IF
            NEXT vc
            IF LevelStart THEN EXIT FOR
        NEXT hc
    END IF
    IF _KEYDOWN(MoveJump) AND jump% > -2 AND jdrop = 0 AND jpush = 0 THEN
        'IF jump% = -2 THEN jump% = 0 'Jump off from a ladder/beanstalk.
        IF jump% = 0 THEN _SNDPLAY SEF(1): plyr& = CrickJmp(0): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1
        IF jump% = 4 THEN plyr& = CrickJmp(1): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1
        IF jump% = 9 THEN plyr& = CrickJmp(2): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1
        IF jump% = 19 THEN plyr& = CrickJmp(3): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1
        IF jump% = 30 THEN plyr& = CrickJmp(4): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1
        IF jump% < 42 THEN 'Height test, instead of setting the coordinate.
            notop = 0
            IF CKT% <= 23 THEN 'BUG: Jumping off the screen at the topmost screen level still crashes the game.
                CKT% = CKT% - 1: CKB% = CKB% - 1: jump% = jump% + 1
            ELSEIF CKT% > 23 THEN
                FOR Head = 0 TO sn
                    IF LevelData(vert% + (PRow - 5), AboveHead(Head)) = 2 THEN
                        jdrop = 1 'Stop jumping if something's above his head
                        'Maybe add the sound of him hitting his head?
                    ELSE
                        notop = notop + 1 'One of the tiles isn't a wall
                    END IF
                NEXT Head 'Below: jump if the 3 or 4 tiles above aren't walls
                IF notop > 2 THEN CKT% = CKT% - 1: CKB% = CKB% - 1: jump% = jump + 1
            END IF
        ELSE
            jump% = 0
            jdrop = 1
        END IF
    ELSEIF kp& = (MoveJump - (MoveJump * 2)) THEN jdrop = 1: jpush = 1 'Let off the JUMP key early
    ELSEIF jdrop THEN 'If you let off the JUMP key
        IF jpush = 0 THEN jpush = 1
        IF plyr& <> CrickJmp(6) THEN plyr& = CrickJmp(6): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1
        CKT% = CKT% + 1 '   This brings the player sprite back down to the
        CKB% = CKB% + 1 '   platform it was on, one pixel at a time.
    END IF
    '******************
    ' Move Down
    '******************
ELSEIF atk = -1 AND _KEYDOWN(MoveDown) AND slidecount% = 0 THEN
    climb = 0
    FOR cc = 0 TO sn
        IF LevelData(vert% + (PRow - 2), AboveHead(cc)) = 3 AND LevelData(vert% + (PRow - 1), AboveHead(cc)) = 3 THEN
            climb = climb + 1 'If Cricket can climb what's below him
        ELSEIF LevelData(vert% + PRow, AboveHead(cc)) = 1 OR LevelData(vert% + PRow, AboveHead(cc)) = 2 THEN
            climb = climb - 1 'He won't climb below a platform
        END IF
    NEXT cc 'If he's in front of two climbable tiles, let's slide!
    IF climb = 2 THEN
        IF plyr& <> climb& THEN plyr& = climb&: CKR% = CKL% + (_WIDTH(plyr&) - 1): CKT% = CKB% - _HEIGHT(plyr&)
        CKT% = CKT% + 1: CKB% = CKB% + 1: jump% = -2: jdrop = 0
    END IF
    IF _KEYDOWN(MoveJump) AND jump% > -2 AND jdrop = 0 AND jpush = 0 THEN
        'IF jump% = -2 THEN jump% = 0 'Jump off from a ladder/beanstalk.
        IF jump% = 0 THEN _SNDPLAY SEF(1): plyr& = CrickJmp(0): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1
        IF jump% = 4 THEN plyr& = CrickJmp(1): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1
        IF jump% = 9 THEN plyr& = CrickJmp(2): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1
        IF jump% = 19 THEN plyr& = CrickJmp(3): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1
        IF jump% = 30 THEN plyr& = CrickJmp(4): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1
        IF jump% < 42 THEN 'Height test, instead of setting the coordinate.
            notop = 0
            IF CKT% <= 23 THEN 'BUG: Jumping off the screen at the topmost screen level still crashes the game.
                CKT% = CKT% - 1: CKB% = CKB% - 1: jump% = jump% + 1
            ELSEIF CKT% > 23 THEN
                FOR Head = 0 TO sn
                    IF LevelData(vert% + (PRow - 5), AboveHead(Head)) = 2 THEN
                        jdrop = 1 'Stop jumping if something's above his head
                        'Maybe add the sound of him hitting his head?
                    ELSE
                        notop = notop + 1 'One of the tiles isn't a wall
                    END IF
                NEXT Head 'Below: jump if the 3 or 4 tiles above aren't walls
                IF notop > 2 THEN CKT% = CKT% - 1: CKB% = CKB% - 1: jump% = jump + 1
            END IF
        ELSE
            jump% = 0
            jdrop = 1
        END IF
    ELSEIF kp& = (MoveJump - (MoveJump * 2)) THEN jdrop = 1: jpush = 1 'Let go of the JUMP key early
    ELSEIF jdrop THEN 'If you let off the JUMP key
        IF jpush = 0 THEN jpush = 1
        IF plyr& <> CrickJmp(6) THEN plyr& = CrickJmp(6): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1
        CKT% = CKT% + 1 '   This brings the player sprite back down to the
        CKB% = CKB% + 1 '   platform it was on, one pixel at a time.
    END IF

ELSEIF kp& = MovePause THEN GOSUB PauseGame 'Pressing the PAUSE key by itself

ELSEIF kp& = MoveRun THEN 'Pressing RUN by itself
    _SNDPLAY ckHurt& 'Just for testing purposes.
    'I wanna get the movement speed down, first. Then I'll make this work.

ELSEIF atk = -1 AND _KEYDOWN(MoveJump) AND jdrop = 0 AND jpush = 0 AND slidecount% = 0 THEN 'Holding down the JUMP key
    IF jump% = -2 THEN jump% = 0 'Jump off from a ladder/beanstalk.
    IF jump% = 0 THEN _SNDPLAY SEF(1): plyr& = CrickJmp(0): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1
    IF jump% = 4 THEN plyr& = CrickJmp(1): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1
    IF jump% = 9 THEN plyr& = CrickJmp(2): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1
    IF jump% = 19 THEN plyr& = CrickJmp(3): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1
    IF jump% = 30 THEN plyr& = CrickJmp(4): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1
    IF jump% < 42 THEN 'Height test, instead of setting the coordinate.
        notop = 0
        IF CKT% <= 23 THEN 'The wall checking routine should only check what walls the on-screen parts are near.
            CKT% = CKT% - 1: CKB% = CKB% - 1: jump% = jump% + 1
        ELSEIF CKT% > 23 THEN
            FOR Head = 0 TO sn
                IF LevelData(vert% + (PRow - 5), AboveHead(Head)) = 2 THEN
                    jdrop = 1 'Stop jumping if something's above his head
                    'Maybe add the sound of him hitting his head?
                ELSE
                    notop = notop + 1 'One of the tiles isn't a wall
                END IF
            NEXT Head 'Below: jump if the 3 or 4 tiles above aren't walls
            IF notop > 2 THEN CKT% = CKT% - 1: CKB% = CKB% - 1: jump% = jump + 1
        END IF
    ELSE
        jump% = 0
        jdrop = 1
    END IF
ELSEIF kp& = (MoveJump - (MoveJump * 2)) THEN jdrop = 1: jpush = 1 'Let go of the JUMP key early
ELSEIF jdrop THEN 'If you let off the JUMP key
    IF jpush = 0 THEN jpush = 1
    IF plyr& <> CrickJmp(6) THEN plyr& = CrickJmp(6): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1
    CKT% = CKT% + 1 '   This brings the player sprite back down to the
    CKB% = CKB% + 1 '   platform it was on, one pixel at a time.

ELSEIF kp& = MoveAttack THEN 'Pressing the ATTACK key by itself
    IF atk = -1 THEN atk = 0: last& = plyr& 'This starts the attack animation sequence, and saves the pose we were in
    'I wonder if I should make it so you can hold down the attack key?
ELSEIF atk > -1 THEN
    IF atk < 6 THEN plyr& = CrickAtk(atk): CKT% = CKB% - _HEIGHT(CrickAtk(atk)): CKR% = CKL% + _WIDTH(CrickAtk(atk)) - 1
    aa = aa + 1
    IF aa = 10 THEN aa = 0: atk = atk + 1 'Changing "aa = 10" speeds up, or slows down, the attacking animation.
    IF atk = 4 AND aa = 0 THEN _SNDPLAY SEF(4) 'The "AND aa = 0" stops it from playing that sound continuously.
    IF atk = 6 THEN atk = -1: plyr& = last&: CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1
    'END IF

END IF
RETURN

'** INPUT CHECKING SUBROUTINE -> GAMEPAD (This needs a few tweaks, but actually works great, so far!)
JoyCtrlCheck:
'********************
' Move Left
'********************
IF atk = -1 AND slidecount% = 0 THEN
    IF JoyMoveOp(2) > 0 AND MoveLeft < 128 THEN
        IF STICK(ActionLeft, JoyMoveOp(2)) < MoveLeft THEN GOSUB LeftMove
    ELSEIF JoyMoveOp(2) > 0 AND MoveLeft > 128 THEN
        IF STICK(ActionLeft, JoyMoveOp(2)) > MoveLeft THEN GOSUB LeftMove
    ELSEIF JoyMoveOp(2) = 0 AND STRIG(MoveLeft, ActionLeft) THEN GOSUB LeftMove
    END IF
END IF

'********************
' Move Right
'********************
IF atk = -1 AND slidecount% = 0 THEN
    IF JoyMoveOp(3) > 0 AND MoveRight < 128 THEN
        IF STICK(ActionRight, JoyMoveOp(3)) < MoveRight THEN GOSUB RightMove
    ELSEIF JoyMoveOp(3) > 0 AND MoveRight > 128 THEN
        IF STICK(ActionRight, JoyMoveOp(3)) > MoveRight THEN GOSUB RightMove
    ELSEIF JoyMoveOp(3) = 0 AND STRIG(MoveRight, ActionRight) THEN GOSUB RightMove
    END IF
END IF

'********************
' Move Up
'********************
IF atk = -1 AND slidecount% = 0 THEN
    IF JoyMoveOp(0) > 0 AND MoveUp < 128 THEN
        IF STICK(ActionUp, JoyMoveOp(0)) < MoveUp THEN GOSUB UpMove
    ELSEIF JoyMoveOp(0) > 0 AND MoveUp > 128 THEN
        IF STICK(ActionUp, JoyMoveOp(0)) > MoveUp THEN GOSUB UpMove
    ELSEIF JoyMoveOp(0) = 0 AND STRIG(MoveUp, ActionUp) THEN GOSUB UpMove
    END IF
END IF

'********************
' Move Down
'********************
IF atk = -1 AND slidecount% = 0 THEN
    IF JoyMoveOp(1) > 0 AND MoveDown < 128 THEN
        IF STICK(ActionDown, JoyMoveOp(1)) < MoveDown THEN GOSUB DownMove
    ELSEIF JoyMoveOp(1) > 0 AND MoveDown > 128 THEN
        IF STICK(ActionDown, JoyMoveOp(1)) > MoveDown THEN GOSUB DownMove
    ELSEIF JoyMoveOp(1) = 0 AND STRIG(MoveDown, ActionDown) THEN GOSUB DownMove
    END IF
END IF

'********************
' Pause
'********************
IF JoyMoveOp(7) > 0 AND MovePause < 128 THEN
    IF STICK(ActionPause, JoyMoveOp(7)) < MovePause THEN GOSUB PauseGame
ELSEIF JoyMoveOp(7) > 0 AND MovePause > 128 THEN
    IF STICK(ActionPause, JoyMoveOp(7)) > MovePause THEN GOSUB PauseGame
ELSEIF JoyMoveOp(7) = 0 AND STRIG(MovePause, ActionPause) THEN GOSUB PauseGame
END IF

'********************
' Run (by itself)
'********************
IF JoyMoveOp(4) > 0 AND MoveRun < 128 THEN
    IF STICK(ActionRun, JoyMoveOp(4)) < MoveRun THEN s = 0 'Just for testing purposes.
ELSEIF JoyMoveOp(4) > 0 AND MoveRun > 128 THEN
    IF STICK(ActionRun, JoyMoveOp(4)) > MoveRun THEN s = 0 'Just for testing purposes.
ELSEIF JoyMoveOp(4) = 0 AND STRIG(MoveRun, ActionRun) THEN s = 0 'Just for testing purposes.
END IF

'********************
' Jump (by itself)
'********************
IF atk = -1 AND slidecount% = 0 AND jdrop = 0 AND jpush = 0 THEN
    IF JoyMoveOp(5) > 0 AND MoveJump < 128 THEN
        IF STICK(ActionJump, JoyMoveOp(5)) < MoveJump THEN
            IF JoyMoveOp(4) > 0 AND MoveRun < 128 THEN
                IF STICK(ActionRun, JoyMoveOp(4)) < MoveRun THEN rj = 1
            ELSEIF JoyMoveOp(4) > 0 AND MoveRun > 128 THEN
                IF STICK(ActionRun, JoyMoveOp(4)) > MoveRun THEN rj = 1
            ELSEIF JoyMoveOp(4) = 0 AND STRIG(MoveRun, ActionRun) THEN rj = 1
            END IF
            GOSUB JumpAction
        END IF
    ELSEIF JoyMoveOp(5) > 0 AND MoveJump > 128 THEN
        IF STICK(ActionJump, JoyMoveOp(5)) > MoveJump THEN
            IF JoyMoveOp(4) > 0 AND MoveRun < 128 THEN
                IF STICK(ActionRun, JoyMoveOp(4)) < MoveRun THEN rj = 1
            ELSEIF JoyMoveOp(4) > 0 AND MoveRun > 128 THEN
                IF STICK(ActionRun, JoyMoveOp(4)) > MoveRun THEN rj = 1
            ELSEIF JoyMoveOp(4) = 0 AND STRIG(MoveRun, ActionRun) THEN rj = 1
            END IF
            GOSUB JumpAction
        END IF
    ELSEIF JoyMoveOp(5) = 0 AND STRIG(MoveJump, ActionJump) THEN
        IF JoyMoveOp(4) > 0 AND MoveRun < 128 THEN
            IF STICK(ActionRun, JoyMoveOp(4)) < MoveRun THEN rj = 1
        ELSEIF JoyMoveOp(4) > 0 AND MoveRun > 128 THEN
            IF STICK(ActionRun, JoyMoveOp(4)) > MoveRun THEN rj = 1
        ELSEIF JoyMoveOp(4) = 0 AND STRIG(MoveRun, ActionRun) THEN rj = 1
        END IF
        GOSUB JumpAction
    END IF
END IF

'********************
' Attack (by itself)
'********************
IF JoyMoveOp(6) > 0 AND MoveAttack < 128 THEN
    IF STICK(ActionAttack, JoyMoveOp(6)) < MoveAttack THEN
        IF JoyMoveOp(2) > 0 AND MoveLeft < 128 THEN
            IF STICK(ActionLeft, JoyMoveOp(2)) < MoveLeft THEN ma = 1
        ELSEIF JoyMoveOp(2) > 0 AND MoveLeft > 128 THEN
            IF STICK(ActionLeft, JoyMoveOp(2)) > MoveLeft THEN ma = 1
        ELSEIF JoyMoveOp(2) = 0 AND STRIG(MoveLeft, ActionLeft) THEN ma = 1
        END IF
        IF JoyMoveOp(3) > 0 AND MoveRight < 128 THEN
            IF STICK(ActionRight, JoyMoveOp(3)) < MoveRight THEN ma = 1
        ELSEIF JoyMoveOp(3) > 0 AND MoveRight > 128 THEN
            IF STICK(ActionRight, JoyMoveOp(3)) > MoveRight THEN ma = 1
        ELSEIF JoyMoveOp(3) = 0 AND STRIG(MoveRight, ActionRight) THEN ma = 1
        END IF
        IF ma THEN _SNDPLAY SEF(18) ELSE IF atk = -1 THEN atk = 0: last& = plyr&
        ma = 0
    END IF
ELSEIF JoyMoveOp(6) > 0 AND MoveAttack > 128 THEN
    IF STICK(ActionAttack, JoyMoveOp(6)) > MoveAttack THEN
        IF JoyMoveOp(2) > 0 AND MoveLeft < 128 THEN
            IF STICK(ActionLeft, JoyMoveOp(2)) < MoveLeft THEN ma = 1
        ELSEIF JoyMoveOp(2) > 0 AND MoveLeft > 128 THEN
            IF STICK(ActionLeft, JoyMoveOp(2)) > MoveLeft THEN ma = 1
        ELSEIF JoyMoveOp(2) = 0 AND STRIG(MoveLeft, ActionLeft) THEN ma = 1
        END IF
        IF JoyMoveOp(3) > 0 AND MoveRight < 128 THEN
            IF STICK(ActionRight, JoyMoveOp(3)) < MoveRight THEN ma = 1
        ELSEIF JoyMoveOp(3) > 0 AND MoveRight > 128 THEN
            IF STICK(ActionRight, JoyMoveOp(3)) > MoveRight THEN ma = 1
        ELSEIF JoyMoveOp(3) = 0 AND STRIG(MoveRight, ActionRight) THEN ma = 1
        END IF
        IF ma THEN _SNDPLAY SEF(18) ELSE IF atk = -1 THEN atk = 0: last& = plyr&
        ma = 0
    END IF
ELSEIF JoyMoveOp(6) = 0 AND STRIG(MoveAttack, ActionAttack) THEN
    IF JoyMoveOp(2) > 0 AND MoveLeft < 128 THEN
        IF STICK(ActionLeft, JoyMoveOp(2)) < MoveLeft THEN ma = 1
    ELSEIF JoyMoveOp(2) > 0 AND MoveLeft > 128 THEN
        IF STICK(ActionLeft, JoyMoveOp(2)) > MoveLeft THEN ma = 1
    ELSEIF JoyMoveOp(2) = 0 AND STRIG(MoveLeft, ActionLeft) THEN ma = 1
    END IF
    IF JoyMoveOp(3) > 0 AND MoveRight < 128 THEN
        IF STICK(ActionRight, JoyMoveOp(3)) < MoveRight THEN ma = 1
    ELSEIF JoyMoveOp(3) > 0 AND MoveRight > 128 THEN
        IF STICK(ActionRight, JoyMoveOp(3)) > MoveRight THEN ma = 1
    ELSEIF JoyMoveOp(3) = 0 AND STRIG(MoveRight, ActionRight) THEN ma = 1
    END IF
    IF ma THEN _SNDPLAY SEF(18) ELSE IF atk = -1 THEN atk = 0: last& = plyr&
    ma = 0
END IF

IF JoyMoveOp(5) > 0 AND MoveJump < 128 AND slidecount% = 0 THEN
    IF STICK(ActionJump, JoyMoveOp(5)) > MoveJump AND jump% > 0 THEN jdrop = 1: jpush = 1
ELSEIF JoyMoveOp(5) > 0 AND MoveJump > 128 AND slidecount% = 0 THEN
    IF STICK(ActionJump, JoyMoveOp(5)) < MoveJump AND jump% > 0 THEN jdrop = 1: jpush = 1
ELSEIF JoyMoveOp(5) = 0 AND slidecount% = 0 THEN 'I hope this fixes that bug...
    IF STRIG(MoveJump, ActionJump) = 0 AND jump% > 0 THEN jdrop = 1: jpush = 1
END IF

IF jdrop THEN 'If you let off the JUMP button/thumbstick
    IF jpush = 0 THEN jpush = 1
    IF rj = 1 THEN rj = 0
    IF plyr& <> CrickJmp(6) THEN plyr& = CrickJmp(6): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1
    CKT% = CKT% + 1 '   This brings the player sprite back down to the
    CKB% = CKB% + 1 '   platform it was on, one pixel at a time.
END IF

IF atk > -1 THEN 'If you pressed the ATTACK button/thumbstick while standing still
    IF atk < 6 THEN plyr& = CrickAtk(atk): CKT% = CKB% - _HEIGHT(CrickAtk(atk)): CKR% = CKL% + _WIDTH(CrickAtk(atk)) - 1
    aa = aa + 1
    IF aa = 10 THEN aa = 0: atk = atk + 1 'Changing "aa = 10" speeds up, or slows down, the attacking animation.
    IF atk = 4 AND aa = 0 THEN _SNDPLAY SEF(4)
    IF atk = 6 THEN atk = -1: plyr& = last&: CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1
END IF

RETURN

LevelReload: 'A quick way to reload the level while playing it, to make testing easier.
IF actbgm% >= 0 AND bgm& THEN _SNDPAUSE bgm&
_SNDPLAY SEF(13)
_AUTODISPLAY
OPEN LevelData$ FOR INPUT AS #5
OPEN Foreground1$ FOR INPUT AS #1
OPEN Foreground2$ FOR INPUT AS #2
OPEN Background1$ FOR INPUT AS #3
OPEN Background2$ FOR INPUT AS #4
'Placeholder for the monster/trigger data files

FOR q = 0 TO (5 + (msn - 1))
    INPUT #5, sumvar$
NEXT q

LINE (0, 50)-(170, 57), _RGBA32(0, 0, 0, 255), BF
FOR tc = 0 TO tilecnt% 'It should probably show the sprites being drawn, since it's a DEBUG feature, after all.
    LOCATE 11, 1: PRINT "Reloading sprite " + LTRIM$(STR$(tc)) + "... ";
    destpath$ = respath$ + sprfldr$ + LTRIM$(STR$(tc)) + ext$
    tx& = _LOADIMAGE(destpath$)
    _PUTIMAGE (0, 50), tx&
    IF _WIDTH(tx&) = 8 THEN
        FOR i = 0 TO 15
            GET (0, 50)-(7, 57), spritedata(i * 80, tc)
        NEXT i
    ELSEIF _WIDTH(tx&) = 158 THEN
        FOR i = 0 TO 15
            GET (i * 10, 50)-((i * 10) + 7, 57), spritedata(i * 80, tc)
        NEXT i
    END IF
    _FREEIMAGE tx&
    LINE (0, 50)-(170, 57), _RGBA32(0, 0, 0, 255), BF
NEXT tc

lc% = ((grab& / 27) / scrcnt%) / 256
rc% = ((grab& / 27) / scrcnt%) MOD 256
IF rc% >= 128 THEN lc% = lc% - 1
LOCATE 12, 1: PRINT "Divisor is" + STR$(lc%) + ", remainder is" + STR$(rc%) + "."

FOR sl% = 0 TO (scrcnt% - 1)

    IF lc% < 1 THEN 'If the total is less than 256... loop once.
        FOR r% = 0 TO 26
            FOR s% = 0 TO (((grab& / 27) / scrcnt%) - 1)
                LOCATE 13, 1: PRINT "Re-reading row" + STR$((r% + (27 * sl%)) + 1) + ", tile" + STR$(s% + 1) + " A "
                INPUT #1, ns$ 'Grab one of foreground layer one's sprites
                Foreground1((r% + (27 * sl%)), s%) = VAL(ns$) 'Convert & save
                ns$ = "" 'Erase what it's set to, just in case
                INPUT #2, ns$ 'Go for foreground layer two, now
                Foreground2((r% + (27 * sl%)), s%) = VAL(ns$) 'Convert & save
                ns$ = "" 'Let's erase it again, as quickly as we can
                INPUT #3, ns$ 'Go for background layer one, next
                Background1((r% + (27 * sl%)), s%) = VAL(ns$) 'Convert & save
                ns$ = "" 'One more quick erasing of this variable
                INPUT #4, ns$ 'Grab from background layer two, this time
                Background2((r% + (27 * sl%)), s%) = VAL(ns$) 'Convert & save
                ns$ = "" 'Okay, I lied... I gotta erase it, again!
                INPUT #5, ns$ 'And finally, grab some level data!
                LevelData((r% + (27 * sl%)), s%) = VAL(ns$) 'Convert and save
            NEXT s%
        NEXT r%

    ELSEIF lc% >= 1 THEN
        FOR l% = 0 TO (lc% - 1)
            FOR r% = 0 TO 26
                FOR s% = 0 TO 255
                    LOCATE 13, 1: PRINT "Re-reading row" + STR$((r% + (27 * sl%)) + 1) + ", tile" + STR$(256 * l% + (s% + 1)) + " B "
                    INPUT #1, ns$ 'Grab one from the first foreground layer
                    Foreground1((r% + (27 * sl%)), s% + (256 * l%)) = VAL(ns$)
                    ns$ = "" 'Erase what it's set to, just in case
                    INPUT #2, ns$ 'Go for the second foreground layer, now
                    Foreground2((r% + (27 * sl%)), s% + (256 * l%)) = VAL(ns$)
                    ns$ = "" 'Erase it again, Tony
                    INPUT #3, ns$ 'Make a mad dash for background layer one!
                    Background1((r% + (27 * sl%)), s% + (256 * l%)) = VAL(ns$)
                    ns$ = "" 'Blankety-blank-blank it out
                    INPUT #4, ns$ 'Take something from background layer two!
                    Background2((r% + (27 * sl%)), s% + (256 * l%)) = VAL(ns$)
                    ns$ = "" 'I sense a serious need for some blankage.
                    INPUT #5, ns$ 'And a-reach-in and-a grab-a-level data!
                    LevelData((r% + (27 * sl%)), s% + (256 * l%)) = VAL(ns$)
                NEXT s%
            NEXT r%
        NEXT l%

        IF rc% > 0 THEN 'Any sprites left after reading each multiple of 256?
            FOR r% = 0 TO 26
                FOR s% = 0 TO ((((grab& / 27) / scrcnt%) - (256 * lc%)) - 1)
                    LOCATE 13, 1: PRINT "Re-reading row" + STR$((r% + (27 * sl%)) + 1) + ", tile" + STR$((256 * lc%) + s% + 1) + " BC "
                    INPUT #1, ns$ 'Grab a sprite
                    Foreground1((r% + (27 * sl%)), ((256 * lc%) + s%)) = VAL(ns$)
                    ns$ = "" 'Clear the variable, because I'm paranoid
                    INPUT #2, ns$ 'Grab another sprite
                    Foreground2((r% + (27 * sl%)), ((256 * lc%) + s%)) = VAL(ns$)
                    ns$ = "" 'Clear that thing! Get it far away from me! lol
                    INPUT #3, ns$ 'Grab yet another sprite
                    Background1((r% + (27 * sl%)), ((256 * lc%) + s%)) = VAL(ns$)
                    ns$ = "" 'Erase it! Erase it, I say!
                    INPUT #4, ns$ 'Grab one more sprite!
                    Background2((r% + (27 * sl%)), ((256 * lc%) + s%)) = VAL(ns$)
                    ns$ = "" 'It casts magic! Therefore, it must be erased!
                    INPUT #5, ns$ 'Enough with the sprites! Level data, now!
                    LevelData((r% + (27 * sl%)), ((256 * lc%) + s%)) = VAL(ns$)
                NEXT s%
            NEXT r%
        END IF
    END IF

NEXT sl%
CLOSE 1, 2, 3, 4, 5

_DISPLAY
IF actbgm% >= 0 THEN IF _SNDPAUSED(bgm&) THEN _SNDPLAY bgm&
IF jump% > 0 THEN jdrop = 1
RETURN

PauseGame:
IF actbgm% >= 0 AND bgm& THEN _SNDPAUSE bgm&
_SNDPLAY SEF(13)
LINE (0, 0)-(320, 240), _RGBA32(0, 0, 0, 140), BF
ps& = _COPYIMAGE(0)
DRAW "C" + STR$(&HFFFFFFFF)
_DISPLAY

LET Flicker = 1
IF Selector < 17 THEN LET Selector = 1
IF Sector < 2 THEN LET Sector = 0
LET TimeIsNow! = TIMER
DO ' EDIT: Unconditional loop, instead of waiting for CHR$(13).
    LET ClockCheck! = TIMER
    LET WhatIsTime! = ClockCheck! - TimeIsNow!
    IF WhatIsTime! >= .01 THEN
        IF mp = 0 THEN Flicker = Flicker + 1 ELSE Flicker = Flicker - 1
        IF Flicker = 17 THEN mp = 1
        IF Flicker = 0 THEN mp = 0

        IF Sector = 0 THEN 'The main pause menu.
            DRAW "C" + STR$(&HFFFFFFFF)
            IF cont% = 0 THEN CenterFont "PRESS ''" + ButtonDef$(MovePause) + "'' AGAIN TO UNPAUSE", 85
            IF cont% > 0 AND JoyMoveOp(7) > 0 THEN
                CenterFont "PUSH THAT THUMBSTICK THAT WAY AGAIN TO UNPAUSE", 85
            ELSEIF cont% > 0 AND JoyMoveOp(7) = 0 THEN
                CenterFont "PRESS ''" + GamepadDef$(7) + "'' AGAIN TO UNPAUSE", 85
            END IF
            DRAW "C" + STR$(&HFFFFFFFF): CenterFont "UNPAUSE", 105
            DRAW "C" + STR$(&HFF888888): CenterFont "LOAD", 115
            DRAW "C" + STR$(&HFF888888): CenterFont "SAVE", 125
            DRAW "C" + STR$(&HFFFFFFFF): CenterFont "CONTROLS", 135
            DRAW "C" + STR$(&HFFFFFFFF): IF actbgm% >= 0 THEN CenterFont "MUSIC IS ON", 145 ELSE CenterFont "MUSIC IS OFF", 145
            DRAW "C" + STR$(&HFFFFFFFF): CenterFont "EXIT TO MENU", 160
        ELSEIF Sector = 1 THEN 'The "Controls" menu.
            DRAW "C" + STR$(&HFFFFFFFF): CenterFont "CONTROL OPTIONS", 65
            DRAW "C" + STR$(&HFFFFFFFF) + " BM1,85": Font "WEAPON OF CHOICE"
            DRAW "C" + STR$(&HFFFFFFFF) + " BM1,95": Font "CONTROL SCHEME PRESET"
            DRAW "C" + STR$(&HFFFFFFFF) + " BM1,105": Font "UP"
            DRAW "C" + STR$(&HFFFFFFFF) + " BM1,115": Font "DOWN"
            DRAW "C" + STR$(&HFFFFFFFF) + " BM1,125": Font "LEFT"
            DRAW "C" + STR$(&HFFFFFFFF) + " BM1,135": Font "RIGHT"
            DRAW "C" + STR$(&HFFFFFFFF) + " BM1,145": Font "RUN"
            DRAW "C" + STR$(&HFFFFFFFF) + " BM1,155": Font "JUMP"
            DRAW "C" + STR$(&HFFFFFFFF) + " BM1,165": Font "ATTACK"
            DRAW "C" + STR$(&HFFFFFFFF) + " BM1,175": Font "PAUSE"
            DRAW "B M155,85 C" + STR$(&HFF00FF00): Font UCASE$(ContType(cont%))
            DRAW "B M155,95 C" + STR$(&HFF00FF00): Font UCASE$(ContPrst(cont%, pn))
            DRAW "B M50,105 C" + STR$(&HFF00FF00): IF cont% = 0 THEN Font ButtonDef$(MoveUp) ELSE Font GamepadDef$(0)
            DRAW "B M50,115 C" + STR$(&HFF00FF00): IF cont% = 0 THEN Font ButtonDef$(MoveDown) ELSE Font GamepadDef$(1)
            DRAW "B M50,125 C" + STR$(&HFF00FF00): IF cont% = 0 THEN Font ButtonDef$(MoveLeft) ELSE Font GamepadDef$(2)
            DRAW "B M50,135 C" + STR$(&HFF00FF00): IF cont% = 0 THEN Font ButtonDef$(MoveRight) ELSE Font GamepadDef$(3)
            DRAW "B M50,145 C" + STR$(&HFF00FF00): IF cont% = 0 THEN Font ButtonDef$(MoveRun) ELSE Font GamepadDef$(4)
            DRAW "B M50,155 C" + STR$(&HFF00FF00): IF cont% = 0 THEN Font ButtonDef$(MoveJump) ELSE Font GamepadDef$(5)
            DRAW "B M50,165 C" + STR$(&HFF00FF00): IF cont% = 0 THEN Font ButtonDef$(MoveAttack) ELSE Font GamepadDef$(6)
            DRAW "B M50,175 C" + STR$(&HFF00FF00): IF cont% = 0 THEN Font ButtonDef$(MovePause) ELSE Font GamepadDef$(7)
        ELSEIF Sector = 2 THEN 'The "Exit to Menu" menu.
            DRAW "C" + STR$(&HFFFFFFFF): CenterFont "THIS WILL END THIS GAME! ARE YOU SURE?", 100
            DRAW "C" + STR$(&HFFFFFFFF): CenterFont "YES! I'M POSITIVE!", 120
            DRAW "C" + STR$(&HFFFFFFFF): CenterFont "WAIT! NO, NOT YET!", 130
        ELSEIF Sector = 3 THEN 'The "ESC" menu.
            DRAW "C" + STR$(&HFFFFFFFF): CenterFont "SO... WHERE DO WE GO FROM HERE?", 95
            DRAW "C" + STR$(&HFFFFFFFF): CenterFont "BACK TO THE GAME!", 115
            DRAW "C" + STR$(&HFFFFFFFF): CenterFont "BACK TO THE MAIN MENU!", 125
            DRAW "C" + STR$(&HFFFFFFFF): CenterFont "BACK TO MY DESKTOP!", 135

        END IF

        'First row is the main pause menu.
        IF Selector = 1 THEN DRAW "C" + STR$(_RGBA32(0, 178, 0, (Flicker * 15))): CenterFont "UNPAUSE", 105
        'Initially skipping "LOAD" and "SAVE" for the prototype only.
        IF Selector = 2 THEN DRAW "C" + STR$(_RGBA32(0, 178, 0, (Flicker * 15))): CenterFont "CONTROLS", 135
        IF Selector = 3 THEN DRAW "C" + STR$(_RGBA32(0, 178, 0, (Flicker * 15))): IF actbgm% >= 0 THEN CenterFont "MUSIC IS ON", 145 ELSE CenterFont "MUSIC IS OFF", 145
        IF Selector = 4 THEN DRAW "C" + STR$(_RGBA32(0, 178, 0, (Flicker * 15))): CenterFont "EXIT TO MENU", 160
        'Second row is the "controls" menu.
        IF Selector = 5 THEN DRAW "C" + STR$(_RGBA32(0, 178, 0, (Flicker * 15))) + " BM1,85": Font "WEAPON OF CHOICE"
        IF Selector = 6 THEN DRAW "C" + STR$(_RGBA32(0, 178, 0, (Flicker * 15))) + " BM1,95": Font "CONTROL SCHEME PRESET"
        IF Selector = 7 THEN DRAW "C" + STR$(_RGBA32(0, 178, 0, (Flicker * 15))) + " BM1,105": Font "UP"
        IF Selector = 8 THEN DRAW "C" + STR$(_RGBA32(0, 178, 0, (Flicker * 15))) + " BM1,115": Font "DOWN"
        IF Selector = 9 THEN DRAW "C" + STR$(_RGBA32(0, 178, 0, (Flicker * 15))) + " BM1,125": Font "LEFT"
        IF Selector = 10 THEN DRAW "C" + STR$(_RGBA32(0, 178, 0, (Flicker * 15))) + " BM1,135": Font "RIGHT"
        IF Selector = 11 THEN DRAW "C" + STR$(_RGBA32(0, 178, 0, (Flicker * 15))) + " BM1,145": Font "RUN"
        IF Selector = 12 THEN DRAW "C" + STR$(_RGBA32(0, 178, 0, (Flicker * 15))) + " BM1,155": Font "JUMP"
        IF Selector = 13 THEN DRAW "C" + STR$(_RGBA32(0, 178, 0, (Flicker * 15))) + " BM1,165": Font "ATTACK"
        IF Selector = 14 THEN DRAW "C" + STR$(_RGBA32(0, 178, 0, (Flicker * 15))) + " BM1,175": Font "PAUSE"
        'Third row is the "exit to menu" menu.
        IF Selector = 15 THEN DRAW "C" + STR$(_RGBA32(0, 178, 0, (Flicker * 15))): CenterFont "YES! I'M POSITIVE!", 120
        IF Selector = 16 THEN DRAW "C" + STR$(_RGBA32(0, 178, 0, (Flicker * 15))): CenterFont "WAIT! NO, NOT YET!", 130
        'Fourth row is the "ESC" menu.
        IF Selector = 17 THEN DRAW "C" + STR$(_RGBA32(0, 178, 0, (Flicker * 15))): CenterFont "BACK TO THE GAME!", 115
        IF Selector = 18 THEN DRAW "C" + STR$(_RGBA32(0, 178, 0, (Flicker * 15))): CenterFont "BACK TO THE MAIN MENU!", 125
        IF Selector = 19 THEN DRAW "C" + STR$(_RGBA32(0, 178, 0, (Flicker * 15))): CenterFont "BACK TO MY DESKTOP!", 135

        LET TimeIsNow! = TIMER
        LET ClockCheck! = TIMER
        LET WhatIsTime! = 0
        _DISPLAY
    END IF
    'IF cont% = 0 THEN 'You get this set of CASE statements if you're using the keyboard. (TODO: Gamepad support!)
    SELECT CASE _KEYHIT 'Works with the keyboard repeat rate, unlike _KEYDOWN.
        CASE 18432 ' Up arrow
            Selector = Selector - 1
            IF Sector = 0 THEN IF Selector = 0 THEN Selector = 4
            IF Sector = 1 THEN IF Selector = 4 THEN Selector = 14
            IF Sector = 2 THEN IF Selector = 14 THEN Selector = 16
            IF Sector = 3 THEN IF Selector = 16 THEN Selector = 19
        CASE 20480 ' Down arrow
            Selector = Selector + 1
            IF Sector = 0 THEN IF Selector = 5 THEN Selector = 1
            IF Sector = 1 THEN IF Selector = 15 THEN Selector = 5
            IF Sector = 2 THEN IF Selector = 17 THEN Selector = 15
            IF Sector = 3 THEN IF Selector = 20 THEN Selector = 17
        CASE 13 ' ENTER
            SELECT CASE Selector 'A tiny bit of code optimization, here. (which I totally copied from CricketMenu)
                CASE 1 'Unpause
                    _SNDPLAY SEF(4)
                    EXIT DO
                CASE 2 'Controls
                    _SNDPLAY SEF(4)
                    _PUTIMAGE (0, 0)-(319, 239), ps&
                    Sector = 1
                    Selector = 5
                CASE 3 'Music Toggle (and later ambience toggle, too)
                    _SNDPLAY SEF(8)
                    IF actbgm% >= 0 THEN
                        prevbgm% = actbgm%
                        actbgm% = -1
                        _SNDSTOP bgm&
                        _SNDCLOSE bgm&
                        _PUTIMAGE (0, 0)-(319, 239), ps&
                    ELSEIF actbgm% = -1 THEN
                        actbgm% = prevbgm%
                        bgm& = _SNDOPEN(BGM(actbgm%), "VOL,PAUSE")
                        _PUTIMAGE (0, 0)-(319, 239), ps&
                    END IF
                CASE 4 'Exit to Menu
                    _SNDPLAY SEF(4)
                    _PUTIMAGE (0, 0)-(319, 239), ps&
                    Sector = 2
                    Selector = 15
                CASE 5 'Weapon of Choice
                    _SNDPLAY SEF(8)
                    cont% = cont% + 1
                    IF cont% > gp% THEN cont% = 0
                    'Change the keymaps accordingly.
                    IF cont% = 0 THEN 'Keyboard
                        MoveUp = DefKeys(0, pn, 0, 0): MoveDown = DefKeys(0, pn, 0, 1)
                        MoveLeft = DefKeys(0, pn, 0, 2): MoveRight = DefKeys(0, pn, 0, 3)
                        MoveRun = DefKeys(0, pn, 0, 4): MoveJump = DefKeys(0, pn, 0, 5)
                        MoveAttack = DefKeys(0, pn, 0, 6): MovePause = DefKeys(0, pn, 0, 7)
                    ELSEIF cont% > 0 THEN 'Gamepad or Joystick
                        MoveUp = DefKeys(cont%, pn, 0, 0): ActionUp = DefKeys(cont%, pn, 1, 0)
                        MoveDown = DefKeys(cont%, pn, 0, 1): ActionDown = DefKeys(cont%, pn, 1, 1)
                        MoveLeft = DefKeys(cont%, pn, 0, 2): ActionLeft = DefKeys(cont%, pn, 1, 2)
                        MoveRight = DefKeys(cont%, pn, 0, 3): ActionRight = DefKeys(cont%, pn, 1, 3)
                        MoveRun = DefKeys(cont%, pn, 0, 4): ActionRun = DefKeys(cont%, pn, 1, 4)
                        MoveJump = DefKeys(cont%, pn, 0, 5): ActionJump = DefKeys(cont%, pn, 1, 5)
                        MoveAttack = DefKeys(cont%, pn, 0, 6): ActionAttack = DefKeys(cont%, pn, 1, 6)
                        MovePause = DefKeys(cont%, pn, 0, 7): ActionPause = DefKeys(cont%, pn, 1, 7)
                        FOR kn = 0 TO 7: JoyMoveOp(kn) = DefKeys(cont%, pn, 2, kn): NEXT kn
                    END IF
                    _PUTIMAGE (0, 0)-(319, 239), ps&
                CASE 6 'Control Scheme Preset
                    _SNDPLAY SEF(8)
                    pn = pn + 1
                    IF pn > 2 THEN pn = 0
                    IF cont% = 0 AND pn < 2 THEN 'Keyboard (the keymaps change, too)
                        MoveUp = DefKeys(0, pn, 0, 0): MoveDown = DefKeys(0, pn, 0, 1)
                        MoveLeft = DefKeys(0, pn, 0, 2): MoveRight = DefKeys(0, pn, 0, 3)
                        MoveRun = DefKeys(0, pn, 0, 4): MoveJump = DefKeys(0, pn, 0, 5)
                        MoveAttack = DefKeys(0, pn, 0, 6): MovePause = DefKeys(0, pn, 0, 7)
                    ELSEIF cont% > 0 AND pn < 2 THEN 'Gamepad or Joystick
                        MoveUp = DefKeys(cont%, pn, 0, 0): ActionUp = DefKeys(cont%, pn, 1, 0)
                        MoveDown = DefKeys(cont%, pn, 0, 1): ActionDown = DefKeys(cont%, pn, 1, 1)
                        MoveLeft = DefKeys(cont%, pn, 0, 2): ActionLeft = DefKeys(cont%, pn, 1, 2)
                        MoveRight = DefKeys(cont%, pn, 0, 3): ActionRight = DefKeys(cont%, pn, 1, 3)
                        MoveRun = DefKeys(cont%, pn, 0, 4): ActionRun = DefKeys(cont%, pn, 1, 4)
                        MoveJump = DefKeys(cont%, pn, 0, 5): ActionJump = DefKeys(cont%, pn, 1, 5)
                        MoveAttack = DefKeys(cont%, pn, 0, 6): ActionAttack = DefKeys(cont%, pn, 1, 6)
                        MovePause = DefKeys(cont%, pn, 0, 7): ActionPause = DefKeys(cont%, pn, 1, 7)
                        FOR kn = 0 TO 7: JoyMoveOp(kn) = DefKeys(cont%, pn, 2, kn): NEXT kn
                    END IF
                    _PUTIMAGE (0, 0)-(319, 239), ps&
                CASE 7: GOSUB Remap 'Code OPTIMIZATION YEAAAAH!! Waaaa! Shoo-be-doo-boo-yeah! (know the reference?)
                CASE 8: GOSUB Remap
                CASE 9: GOSUB Remap
                CASE 10: GOSUB Remap
                CASE 11: GOSUB Remap
                CASE 12: GOSUB Remap
                CASE 13: GOSUB Remap
                CASE 14: GOSUB Remap
                CASE 15 'Yes! I'm positive!
                    _SNDPLAY SEF(4)
                    IF RecSpot = 2 THEN 'If this menu was called, after picking "back to my desktop"
                        IF actbgm% >= 0 AND bgm& THEN _SNDSTOP bgm&
                        IF _FULLSCREEN THEN _FULLSCREEN _OFF
                        END
                    ELSE 'If this menu was called, after picking "exit to menu" or "back to the menu"
                        IF actbgm% >= 0 AND bgm& THEN _SNDSTOP bgm&: _SNDCLOSE bgm& 'Kill the background music
                        actbgm% = 0: prevbgm% = 0 'Reset the active song to 0
                        FadeOut 'Fade to black
                        _FREEIMAGE ps& 'Get rid of the saved image of the screen
                        res = 1 'So the game will know that it doesn't have to DIM critical graphics engine arrays
                        GOTO LoopBack 'Return to the main menu (I can, from a subroutine, without messing things up!)
                    END IF
                CASE 16 'No, wait! Not yet!
                    _SNDPLAY SEF(4)
                    _PUTIMAGE (0, 0)-(319, 239), ps&
                    IF RecSpot = 0 THEN Sector = 0: Selector = 4
                    IF RecSpot = 1 THEN Sector = 3: Selector = 18
                    IF RecSpot = 2 THEN Sector = 3: Selector = 19
                    RecSpot = 0
                CASE 17 'Back to the Game!
                    _SNDPLAY SEF(4)
                    EXIT DO
                CASE 18 'Back to the Menu!
                    _SNDPLAY SEF(4)
                    _PUTIMAGE (0, 0)-(319, 239), ps&
                    Sector = 2
                    Selector = 15
                    RecSpot = 1
                CASE 19 'Back to My Desktop!
                    _SNDPLAY SEF(4)
                    _PUTIMAGE (0, 0)-(319, 239), ps&
                    Sector = 2
                    Selector = 15
                    RecSpot = 2
            END SELECT
        CASE 27 'ESC
            IF Sector = 0 THEN 'From the main pause menu
                _SNDPLAY SEF(1)
                EXIT DO
            ELSEIF Sector = 3 THEN 'From the "ESC" menu
                _SNDPLAY SEF(1)
                Sector = 0
                Selector = 1
                EXIT DO
            ELSEIF Sector = 2 THEN 'From the "exit to menu" menu
                _SNDPLAY SEF(1)
                _PUTIMAGE (0, 0)-(319, 239), ps&, small
                IF RecSpot = 0 THEN Sector = 0: Selector = 4
                IF RecSpot = 1 THEN Sector = 3: Selector = 18
                IF RecSpot = 2 THEN Sector = 3: Selector = 19
                RecSpot = 0
            ELSEIF Sector = 1 THEN 'From the "controls" menu
                _SNDPLAY SEF(1)
                _PUTIMAGE (0, 0)-(319, 239), ps&, small
                Sector = 0
                Selector = 2
            END IF
        CASE MovePause 'Press the PAUSE key again
            EXIT DO
    END SELECT
    'ELSEIF cont% > 0 THEN 'Or you get this set if you're using a gamepad. (we should uncomment this, and add it in)
    'END IF
LOOP

'IF cont% = 0 THEN 'If the keyboard is the controller...
'DO: LOOP UNTIL _KEYHIT = MovePause 'Press the key again to unpause
'ELSEIF cont% > 0 THEN '...or if a gamepad is the controller...
'    DO 'Wait for the right button/thumbstick to be pressed, then unpause
'        IF JoyMoveOp(7) > 0 AND STICK(ActionPause, JoyMoveOp(7)) = MovePause THEN EXIT DO
'        IF JoyMoveOp(7) = 0 AND STRIG(MovePause, ActionPause) THEN EXIT DO
'LOOP
'END IF
_FREEIMAGE ps&
IF actbgm% >= 0 THEN IF _SNDPAUSED(bgm&) THEN _SNDPLAY bgm& ELSE _SNDLOOP bgm&
IF jump% > 0 THEN jdrop = 1
RETURN

'*********************************************************
' Momentary change for this build... the new movement subroutines for the gamepad routine code!
LeftMove:
'Check for collisions, before we decide if we can move, or not
IF (vert% + PRow) > 0 THEN
    IF PRow < 27 AND CKT% > 23 THEN
        noblock = 0
        FOR cr = 0 TO so 'Apparently this throws an error, on the top screen.
            IF LevelData(vert% + BehindMoi(cr), topleft) <> 2 THEN 'Wall
                noblock = noblock + 1
            END IF
        NEXT cr
    END IF
ELSE
    noblock = 4
END IF
IF noblock > 3 AND CKX% > 0 THEN 'If nothing's in our way, let's move!
    IF jump% = -2 THEN jump% = 0: plyr& = CrickMov(0): CKB% = CKT% + _HEIGHT(plyr&) - 1: CKR% = CKL% + _WIDTH(plyr&) - 1: mf = 1
    pf = 1 'pf is TRUE, meaning the player is facing LEFT.
    stl = 0 'This checks to see if we can scroll the screen.
    IF sprnum(0) > -1 THEN 'Bugfix: Can't check the type of a tile that doesn't exist...
        FOR wc = 0 TO 26 'If the left-most (off-screen) tile is a STOP tile...
            IF LevelData(vert% + wc, sprnum(0)) = 99 THEN stl = stl + 1
        NEXT wc '...then the screen will stop scrolling to the left.
    END IF
    IF CKL% = 148 AND sprnum(0) > -1 AND stl < 27 THEN 'Scroll the background
        FOR q = 0 TO 41 '(tsc - 1)
            sprpos(q) = sprpos(q) + 1
        NEXT q
        CKX% = CKX% - 1
        IF plyr& = CrickMov(mf) THEN mt = mt + 1
        IF mt = 20 THEN 'Changing this number speeds up, or slows down, the walking animation.
            mt = 0: mf = mf + 1: IF mf = 7 THEN mf = 2
            plyr& = CrickMov(mf): CKT% = CKB% - _HEIGHT(CrickMov(mf)): CKR% = CKL% + _WIDTH(CrickMov(mf)) - 1
        END IF
        FOR n = 0 TO (atm - 1)
            IF MnS(n, 5) = 1 THEN
                MnS(n, 0) = MnS(n, 0) + 1
                MnS(n, 1) = MnS(n, 1) + 1
                IF MnS(n, 0) >= 320 THEN MnS(n, 5) = 0
            END IF
        NEXT n
        'ELSEIF CKL% < 148 THEN
    ELSEIF CKL% > 0 AND CKL% < 148 OR sprnum(0) = -1 AND sprpos(0) = -8 OR stl = 27 OR str = 27 THEN 'Move Cricket
        CKL% = CKL% - 1: CKR% = CKR% - 1: CKX% = CKX% - 1
        IF plyr& = CrickMov(mf) THEN mt = mt + 1
        IF mt = 20 THEN
            mt = 0: mf = mf + 1: IF mf = 7 THEN mf = 2
            plyr& = CrickMov(mf): CKT% = CKB% - _HEIGHT(CrickMov(mf)): CKR% = CKL% + _WIDTH(CrickMov(mf)) - 1
        END IF
    END IF
END IF
'END IF
IF sprpos(0) >= 0 THEN 'Do we need to reset the column positions?
    FOR q = 0 TO 41
        sprpos(q) = sprpos(q) - 8 'Reset the column positions
        sprnum(q) = sprnum(q) - 1 'Change the displayed sprite
    NEXT q
END IF
RETURN

RightMove:
'Check for collisions, before we decide if we can move, or not
IF (vert% + PRow) > 0 THEN
    IF PRow < 27 AND CKT% > 23 THEN
        noblock = 0
        FOR cr = 0 TO so
            IF LevelData(vert% + InFrontOf(cr), topright) <> 2 THEN 'Wall
                noblock = noblock + 1
            END IF
        NEXT cr
    END IF
ELSE 'Don't check above the player, if they're on the top-most screen.
    noblock = 4
END IF
IF noblock > 3 THEN 'If nothing's in our way, where do we move?
    IF jump% = -2 THEN jump% = 0: plyr& = CrickMov(0): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1: mf = 1
    pf = 0 'pf is FALSE, meaning the player is facing RIGHT.
    str = 0 'This checks to see if we can scroll the screen.
    FOR wc = 0 TO 26 'If the right-most (off-screen) tile is a STOP tile...
        IF LevelData(vert% + wc, sprnum(41)) = 99 THEN str = str + 1
    NEXT wc '...then the screen will stop scrolling to the right.
    IF CKL% = 148 AND sprnum(0) >= -1 AND str < 27 THEN
        FOR q = 0 TO 41 '(tsc - 1)
            sprpos(q) = sprpos(q) - 1
        NEXT q
        CKX% = CKX% + 1
        IF plyr& = CrickMov(mf) THEN mt = mt + 1
        IF mt = 20 THEN
            mt = 0: mf = mf + 1: IF mf = 7 THEN mf = 2
            plyr& = CrickMov(mf): CKT% = CKB% - _HEIGHT(CrickMov(mf)): CKR% = CKL% + _WIDTH(CrickMov(mf)) - 1
        END IF
        FOR n = 0 TO (atm - 1)
            IF MnS(n, 5) = 1 THEN
                MnS(n, 0) = MnS(n, 0) - 1
                MnS(n, 1) = MnS(n, 1) - 1
                IF MnS(n, 1) <= 0 THEN MnS(n, 5) = 0
            END IF
        NEXT n
    ELSEIF CKL% < 148 OR str = 27 AND CKR% < 319 THEN
        CKL% = CKL% + 1: CKR% = CKR% + 1: CKX% = CKX% + 1
        IF plyr& = CrickMov(mf) THEN mt = mt + 1
        IF mt = 20 THEN
            mt = 0: mf = mf + 1: IF mf = 7 THEN mf = 2
            plyr& = CrickMov(mf): CKT% = CKB% - _HEIGHT(CrickMov(mf)): CKR% = CKL% + _WIDTH(CrickMov(mf)) - 1
        END IF
    END IF
END IF
'END IF
IF sprpos(0) <= -8 THEN 'Do we need to reset the column positions?
    FOR q = 0 TO 41
        sprpos(q) = sprpos(q) + 8
        sprnum(q) = sprnum(q) + 1
    NEXT q
END IF
RETURN

UpMove:
climb = 0
FOR cc = 0 TO sn
    IF LevelData(vert% + (PRow - 3), AboveHead(cc)) = 3 AND LevelData(vert% + (PRow - 2), AboveHead(cc)) = 3 THEN
        climb = climb + 1 'If Cricket can climb what's above him
    END IF
NEXT cc 'If he's in front of two climbable tiles, let's climb up!
IF climb = 2 THEN
    IF plyr& <> climb& THEN plyr& = climb&: CKR% = CKL% + (_WIDTH(plyr&) - 1): CKT% = CKB% - _HEIGHT(plyr&)
    CKT% = CKT% - 1: CKB% = CKB% - 1: jump% = -2: jdrop = 0
END IF
Ldoor = 0: Rdoor = 0: Vpos = 0 'Reset the variables
FOR dc = 0 TO sn 'Check to see if we're standing in front of a door
    IF LevelData(vert% + (PRow - 2), UnderFoot(dc)) >= 100 AND LevelData(vert% + (PRow - 2), UnderFoot(dc)) <= 599 THEN
        IF Ldoor > 0 AND ldset THEN ldset = 0: Rdoor = UnderFoot(dc)
        IF Ldoor = 0 AND Rdoor = 0 THEN Ldoor = UnderFoot(dc): Vpos = (vert% + (PRow - 2)): ldset = 1
    END IF
NEXT dc
IF Ldoor > 0 AND Rdoor > 0 THEN 'If you're actually standing on a door
    FOR hc = 0 TO (grab& / (27 * scrcnt%)) 'Start looking for the door
        FOR vc = 0 TO ((27 * scrcnt%) - 1) 'that this door leads to.
            IF vc <> Vpos AND hc <> Ldoor THEN 'Skip the same door.
                'BUG: The check below this sometimes goes out of range, on smaller levels.
                IF LevelData(vc, hc) = LevelData(Vpos, Ldoor) AND LevelData(vc, hc + 1) = LevelData(Vpos, Rdoor) THEN
                    'LOCATE 10, 1: PRINT "YOU ARE HERE: " + STR$(Vpos) + "," + STR$(Ldoor)
                    'LOCATE 11, 1: PRINT "DOOR GOES HERE: " + STR$(vc) + "," + STR$(hc): _DISPLAY
                    mc = VAL(RIGHT$(STR$(LevelData(Vpos, Ldoor)), 1))
                    'TIMER OFF
                    lft = 0
                    top = 0
                    rgt = 319
                    bot = 239
                    mv = 11
                    IF pic& THEN _FREEIMAGE pic&
                    pic& = _COPYIMAGE(0)
                    DO
                        LINE (0, 0)-(319, 239), _RGB32(0, 0, 0), BF
                        _PUTIMAGE (lft, top)-(rgt, bot), pic&
                        _DISPLAY
                        IF actbgm% >= 0 AND mc <> actbgm% THEN
                            IF lft MOD 20 = 0 THEN mv = mv - 1
                            _SNDVOL bgm&, (mv / 10)
                            IF mv = 0 THEN _SNDSTOP bgm&
                        END IF
                        IF lft = 160 THEN EXIT DO
                        lft = lft + 1
                        'top = top - 1
                        rgt = rgt - 1
                        'bot = bot + 1
                    LOOP 'Grab the keyboard routine's code from this point when you perfect it
                    vr = vc MOD 27 'Set the screen level appropriately
                    IF vr > 0 THEN vert% = (vc - vr) ELSE vert% = vc
                    'Reset Cricket's coordinates for the new area
                    CKT% = (vr * 8) - 8: CKB% = CKT% + (_HEIGHT(plyr&) - 1)
                    CKX% = hc * 8 'Set Cricket's global X coordinate
                    IF CKX% >= 148 THEN CKL% = 148 ELSE CKL% = CKX%
                    CKR% = CKL% + _WIDTH(plyr&) - 1 'And L/R coords
                    FOR s = 0 TO 41: sprpos(s) = ((s - 1) * 8): NEXT s
                    FOR lh = (hc - 20) TO hc 'Shift the background to
                        sprnum(c) = lh '      the correct position
                        c = c + 1
                    NEXT lh
                    FOR rh = (hc + 1) TO (hc + 21) 'Same as above
                        sprnum(c) = rh
                        c = c + 1
                    NEXT rh
                    c = 0
                    DO WHILE _KEYDOWN(MoveUp): LOOP 'Let go of the key! <-- CHANGE THIS TO THE GAMEPAD EQUIVALENT!
                    LevelStart = 2
                    IF actbgm% >= 0 AND mc <> actbgm% THEN
                        _SNDCLOSE bgm&
                        bgm& = _SNDOPEN(BGM(mc), "VOL,PAUSE")
                        _SNDLOOP bgm&
                        actbgm% = mc
                    ELSEIF actbgm% = -1 THEN prevbgm% = mc
                    END IF
                    IF plyr& <> CrickMov(0) THEN plyr& = CrickMov(0): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1: mf = 1
                    EXIT FOR
                END IF
            END IF
        NEXT vc
        IF LevelStart THEN EXIT FOR
    NEXT hc
END IF
RETURN

DownMove:
climb = 0
FOR cc = 0 TO sn
    IF LevelData(vert% + (PRow - 2), AboveHead(cc)) = 3 AND LevelData(vert% + (PRow - 1), AboveHead(cc)) = 3 THEN
        climb = climb + 1 'If Cricket can climb what's below him
    ELSEIF LevelData(vert% + PRow, AboveHead(cc)) = 1 OR LevelData(vert% + PRow, AboveHead(cc)) = 2 THEN
        climb = climb - 1 'He won't climb below a platform
    END IF
NEXT cc 'If he's in front of two climbable tiles, let's slide!
IF climb = 2 THEN
    IF plyr& <> climb& THEN plyr& = climb&: CKR% = CKL% + (_WIDTH(plyr&) - 1): CKT% = CKB% - _HEIGHT(plyr&)
    CKT% = CKT% + 1: CKB% = CKB% + 1: jump% = -2: jdrop = 0
END IF
RETURN

JumpAction:
IF jump% = -2 THEN jump% = 0 'Jump off from a ladder/beanstalk.
IF jump% = 0 THEN _SNDPLAY SEF(1): plyr& = CrickJmp(0): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1
IF jump% = 4 THEN plyr& = CrickJmp(1): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1
IF jump% = 9 THEN plyr& = CrickJmp(2): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1
IF jump% = 19 THEN plyr& = CrickJmp(3): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1
IF jump% = 30 THEN plyr& = CrickJmp(4): CKT% = CKB% - _HEIGHT(plyr&): CKR% = CKL% + _WIDTH(plyr&) - 1
IF rj = 1 AND jump% < 60 THEN
    notop = 0
    IF CKT% <= 23 THEN 'Does jumping off the screen at the highest screen level still crash the game?
        CKT% = CKT% - 1: CKB% = CKB% - 1: jump% = jump% + 1
    ELSEIF CKT% > 23 THEN
        FOR Head = 0 TO sn
            IF LevelData(vert% + (PRow - 5), AboveHead(Head)) = 2 THEN
                jdrop = 1 'Stop jumping if something's above his head
                'Maybe add the sound of him hitting his head?
            ELSE
                notop = notop + 1 'One of the tiles isn't a wall
            END IF
        NEXT Head 'Below: jump if the 3 or 4 tiles above aren't walls
        IF notop > 2 THEN CKT% = CKT% - 1: CKB% = CKB% - 1: jump% = jump% + 1
    END IF
ELSEIF rj = 0 AND jump% < 42 THEN 'Height test, instead of setting the coordinate.
    notop = 0
    IF CKT% <= 23 THEN 'Bugfix. If nothing's above him, he just jumps.
        CKT% = CKT% - 1: CKB% = CKB% - 1: jump% = jump% + 1
    ELSEIF CKT% > 23 THEN
        FOR Head = 0 TO sn
            IF LevelData(vert% + (PRow - 5), AboveHead(Head)) = 2 THEN
                jdrop = 1 'Stop jumping if something's above his head
                'Maybe add the sound of him hitting his head?
            ELSE
                notop = notop + 1 'One of the tiles isn't a wall
            END IF
        NEXT Head 'Below: jump if the 3 or 4 tiles above aren't walls
        IF notop > 2 THEN CKT% = CKT% - 1: CKB% = CKB% - 1: jump% = jump + 1
    END IF
ELSE
    jump% = 0
    jdrop = 1
END IF
RETURN

RunAction:

RETURN

AttackAction:

RETURN

'*********************************************************
ClockChange: ' Change the time on the clock subroutine (once every second)
'A little change to the main routine, since I'm trying something different.
tick = tick - 1
IF tick = 59 THEN _SNDPLAY SEF(15)
IF tick = 0 THEN _SNDPLAY SEF(16)
RETURN

'*********************************************************
Remap: 'Aside from a few code changes, this is essentially the same as the subroutine within CricketMenu.
_SNDPLAY SEF(8)
IF cont% = 0 THEN kc& = _KEYHIT: IF kc& = 13 THEN DO: kc& = _KEYHIT: LOOP WHILE kc& = 13 'Stop holding ENTER down!
IF cont% = 0 THEN
    DO: kg& = _KEYHIT: LOOP WHILE kg& <= 0 'Only capture a positive keycode.
    IF kg& = 27 THEN 'If the player pressed ESC...
        _SNDPLAY SEF(1)
    ELSE 'Or if the player actually pressed a different key...
        _SNDPLAY SEF(4)
        IF Selector = 7 THEN MoveUp = kg&
        IF Selector = 8 THEN MoveDown = kg&
        IF Selector = 9 THEN MoveLeft = kg&
        IF Selector = 10 THEN MoveRight = kg&
        IF Selector = 11 THEN MoveRun = kg&
        IF Selector = 12 THEN MoveJump = kg&
        IF Selector = 13 THEN MoveAttack = kg&
        IF Selector = 14 THEN MovePause = kg&
    END IF
ELSEIF cont% > 0 THEN
    sax = 0: say = 0: sbx = 0: sby = 0: dpx = 0: dpy = 0
    DO
        km& = _KEYHIT 'For gamepad checking, this just checks to see if the user presses ESC, instead.
        IF km& = 27 THEN _SNDPLAY SEF(1): EXIT DO 'Stop checking the active gamepad if ESC is pressed
        xc = cont% * 2
        sax = STICK(xc - 2, 1)
        IF sax > 249 OR sax < 6 THEN EXIT DO
        say = STICK(xc - 1, 1)
        IF say > 249 OR say < 6 THEN EXIT DO
        sbx = STICK(xc - 2, 2)
        IF sbx > 249 OR sbx < 6 THEN EXIT DO
        sby = STICK(xc - 1, 2)
        IF sby > 249 OR sby < 6 THEN EXIT DO
        dpx = STICK(xc - 2, 3)
        IF dpx > 249 OR dpx < 6 THEN EXIT DO
        dpy = STICK(xc - 1, 3)
        IF dpy > 249 OR dpy < 6 THEN EXIT DO
        IF cont% MOD 2 = 0 THEN bp = STRIG((cb * 4) + 3, cont%) ELSE bp = STRIG((cb * 4) + 1, cont%)
        IF bp THEN
            IF cont% MOD 2 = 0 THEN bn = ((cb * 4) + 3): JoyMoveOp(Selector - 7) = 0: EXIT DO
            IF cont% MOD 2 = 1 THEN bn = ((cb * 4) + 1): JoyMoveOp(Selector - 7) = 0: EXIT DO
        END IF
        IF bn = 0 THEN cb = cb + 1: IF cb = 30 THEN cb = 0
        bp = 0: bn = 0
    LOOP 'Unconditional loop; pressing a gamepad button or thumbstick, or pressing ESC on the keyboard, will end this
    IF km& <> 27 THEN _SNDPLAY SEF(4)
    IF Selector = 7 THEN
        IF sax > 249 OR sax < 6 THEN MoveUp = sax: ActionUp = (xc - 2): JoyMoveOp(0) = 1
        IF say > 249 OR say < 6 THEN MoveUp = say: ActionUp = (xc - 1): JoyMoveOp(0) = 1
        IF sbx > 249 OR sbx < 6 THEN MoveUp = sbx: ActionUp = (xc - 2): JoyMoveOp(0) = 2
        IF sby > 249 OR sby < 6 THEN MoveUp = sby: ActionUp = (xc - 1): JoyMoveOp(0) = 2
        IF dpx > 249 OR dpx < 6 THEN MoveUp = dpx: ActionUp = (xc - 2): JoyMoveOp(0) = 3
        IF dpy > 249 OR dpy < 6 THEN MoveUp = dpy: ActionUp = (xc - 1): JoyMoveOp(0) = 3
        IF bn > 0 THEN MoveUp = bn: ActionUp = cont%
    ELSEIF Selector = 8 THEN
        IF sax > 249 OR sax < 6 THEN MoveDown = sax: ActionDown = (xc - 2): JoyMoveOp(1) = 1
        IF say > 249 OR say < 6 THEN MoveDown = say: ActionDown = (xc - 1): JoyMoveOp(1) = 1
        IF sbx > 249 OR sbx < 6 THEN MoveDown = sbx: ActionDown = (xc - 2): JoyMoveOp(1) = 2
        IF sby > 249 OR sby < 6 THEN MoveDown = sby: ActionDown = (xc - 1): JoyMoveOp(1) = 2
        IF dpx > 249 OR dpx < 6 THEN MoveDown = dpx: ActionDown = (xc - 2): JoyMoveOp(1) = 3
        IF dpy > 249 OR dpy < 6 THEN MoveDown = dpy: ActionDown = (xc - 1): JoyMoveOp(1) = 3
        IF bn > 0 THEN MoveDown = bn: ActionDown = cont%
    ELSEIF Selector = 9 THEN
        IF sax > 249 OR sax < 6 THEN MoveLeft = sax: ActionLeft = (xc - 2): JoyMoveOp(2) = 1
        IF say > 249 OR say < 6 THEN MoveLeft = say: ActionLeft = (xc - 1): JoyMoveOp(2) = 1
        IF sbx > 249 OR sbx < 6 THEN MoveLeft = sbx: ActionLeft = (xc - 2): JoyMoveOp(2) = 2
        IF sby > 249 OR sby < 6 THEN MoveLeft = sby: ActionLeft = (xc - 1): JoyMoveOp(2) = 2
        IF dpx > 249 OR dpx < 6 THEN MoveLeft = dpx: ActionLeft = (xc - 2): JoyMoveOp(2) = 3
        IF dpy > 249 OR dpy < 6 THEN MoveLeft = dpy: ActionLeft = (xc - 1): JoyMoveOp(2) = 3
        IF bn > 0 THEN MoveLeft = bn: ActionLeft = cont%
    ELSEIF Selector = 10 THEN
        IF sax > 249 OR sax < 6 THEN MoveRight = sax: ActionRight = (xc - 2): JoyMoveOp(3) = 1
        IF say > 249 OR say < 6 THEN MoveRight = say: ActionRight = (xc - 1): JoyMoveOp(3) = 1
        IF sbx > 249 OR sbx < 6 THEN MoveRight = sbx: ActionRight = (xc - 2): JoyMoveOp(3) = 2
        IF sby > 249 OR sby < 6 THEN MoveRight = sby: ActionRight = (xc - 1): JoyMoveOp(3) = 2
        IF dpx > 249 OR dpx < 6 THEN MoveRight = dpx: ActionRight = (xc - 2): JoyMoveOp(3) = 3
        IF dpy > 249 OR dpy < 6 THEN MoveRight = dpy: ActionRight = (xc - 1): JoyMoveOp(3) = 3
        IF bn > 0 THEN MoveRight = bn: ActionRight = cont%
    ELSEIF Selector = 11 THEN
        IF sax > 249 OR sax < 6 THEN MoveRun = sax: ActionRun = (xc - 2): JoyMoveOp(4) = 1
        IF say > 249 OR say < 6 THEN MoveRun = say: ActionRun = (xc - 1): JoyMoveOp(4) = 1
        IF sbx > 249 OR sbx < 6 THEN MoveRun = sbx: ActionRun = (xc - 2): JoyMoveOp(4) = 2
        IF sby > 249 OR sby < 6 THEN MoveRun = sby: ActionRun = (xc - 1): JoyMoveOp(4) = 2
        IF dpx > 249 OR dpx < 6 THEN MoveRun = dpx: ActionRun = (xc - 2): JoyMoveOp(4) = 3
        IF dpy > 249 OR dpy < 6 THEN MoveRun = dpy: ActionRun = (xc - 1): JoyMoveOp(4) = 3
        IF bn > 0 THEN MoveRun = bn: ActionRun = cont%
    ELSEIF Selector = 12 THEN
        IF sax > 249 OR sax < 6 THEN MoveJump = sax: ActionJump = (xc - 2): JoyMoveOp(5) = 1
        IF say > 249 OR say < 6 THEN MoveJump = say: ActionJump = (xc - 1): JoyMoveOp(5) = 1
        IF sbx > 249 OR sbx < 6 THEN MoveJump = sbx: ActionJump = (xc - 2): JoyMoveOp(5) = 2
        IF sby > 249 OR sby < 6 THEN MoveJump = sby: ActionJump = (xc - 1): JoyMoveOp(5) = 2
        IF dpx > 249 OR dpx < 6 THEN MoveJump = dpx: ActionJump = (xc - 2): JoyMoveOp(5) = 3
        IF dpy > 249 OR dpy < 6 THEN MoveJump = dpy: ActionJump = (xc - 1): JoyMoveOp(5) = 3
        IF bn > 0 THEN MoveJump = bn: ActionJump = cont%
    ELSEIF Selector = 13 THEN
        IF sax > 249 OR sax < 6 THEN MoveAttack = sax: ActionAttack = (xc - 2): JoyMoveOp(6) = 1
        IF say > 249 OR say < 6 THEN MoveAttack = say: ActionAttack = (xc - 1): JoyMoveOp(6) = 1
        IF sbx > 249 OR sbx < 6 THEN MoveAttack = sbx: ActionAttack = (xc - 2): JoyMoveOp(6) = 2
        IF sby > 249 OR sby < 6 THEN MoveAttack = sby: ActionAttack = (xc - 1): JoyMoveOp(6) = 2
        IF dpx > 249 OR dpx < 6 THEN MoveAttack = dpx: ActionAttack = (xc - 2): JoyMoveOp(6) = 3
        IF dpy > 249 OR dpy < 6 THEN MoveAttack = dpy: ActionAttack = (xc - 1): JoyMoveOp(6) = 3
        IF bn > 0 THEN MoveAttack = bn: ActionAttack = cont%
    ELSEIF Selector = 14 THEN
        IF sax > 249 OR sax < 6 THEN MovePause = sax: ActionPause = (xc - 2): JoyMoveOp(7) = 1
        IF say > 249 OR say < 6 THEN MovePause = say: ActionPause = (xc - 1): JoyMoveOp(7) = 1
        IF sbx > 249 OR sbx < 6 THEN MovePause = sbx: ActionPause = (xc - 2): JoyMoveOp(7) = 2
        IF sby > 249 OR sby < 6 THEN MovePause = sby: ActionPause = (xc - 1): JoyMoveOp(7) = 2
        IF dpx > 249 OR dpx < 6 THEN MovePause = dpx: ActionPause = (xc - 2): JoyMoveOp(7) = 3
        IF dpy > 249 OR dpy < 6 THEN MovePause = dpy: ActionPause = (xc - 1): JoyMoveOp(7) = 3
        IF bn > 0 THEN MovePause = bn: ActionPause = cont%
    END IF
END IF
_PUTIMAGE (0, 0)-(319, 239), ps&, small
RETURN

'*********************************************************
REM $STATIC
' For the engine demo, it'll probably just be a working logo, since I plan on
' replacing it when I get closer to an actual beta. I do plan on putting a
' main menu in here, at some point.
SUB CricketMenu
'Let's see if I can't draw up a logo for our friend, master Kamodon...
'The "C"
DRAW "B M13,0"
DRAW "C" + STR$(&HFF00FF00)
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
DRAW "P" + STR$(&HFF00FF00) + "," + STR$(&HFF00FF00)

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
DRAW "P" + STR$(&HFF00FF00) + "," + STR$(&HFF00FF00)

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
DRAW "P" + STR$(&HFF00FF00) + "," + STR$(&HFF00FF00)

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
DRAW "P" + STR$(&HFF00FF00) + "," + STR$(&HFF00FF00)

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
DRAW "P" + STR$(&HFF00FF00) + "," + STR$(&HFF00FF00)

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
DRAW "P" + STR$(&HFF00FF00) + "," + STR$(&HFF00FF00)

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
DRAW "P" + STR$(&HFF00FF00) + "," + STR$(&HFF00FF00)

' Second row!

'The other "K"
DRAW "B M0,190"
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
DRAW "P" + STR$(&HFF00FF00) + "," + STR$(&HFF00FF00)

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
DRAW "P" + STR$(&HFF00FF00) + "," + STR$(&HFF00FF00)

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
DRAW "P" + STR$(&HFF00FF00) + "," + STR$(&HFF00FF00)

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
DRAW "P" + STR$(&HFF00FF00) + "," + STR$(&HFF00FF00)

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
DRAW "P" + STR$(&HFF00FF00) + "," + STR$(&HFF00FF00)

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
DRAW "P" + STR$(&HFF00FF00) + "," + STR$(&HFF00FF00)

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
DRAW "P" + STR$(&HFF00FF00) + "," + STR$(&HFF00FF00)

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

DRAW "B M33,87"
DRAW "C" + STR$(&HFFFFFFFF)
Font "COPYRIGHT 2011-2013 FLAMEWARE ZERONEXE"
DRAW "B M20,106"
Font "KAMION SHANORIKO PROGRAMMED THE ENGINE, AND"
DRAW "B M66,116"
Font "WROTE MOST OF THE SOURCE CODE"
'DRAW "B M11,126"
'Font "AARON SEVERN WROTE A PIXEL-BY-PIXEL SCROLLING"
'DRAW "B M22,136"
'Font "ROUTINE, WHICH KAMION TWEAKED FOR THIS GAME"
FadeIn

LET FlashColor = 1
LET TimeIsNow! = TIMER
DO ' EDIT: Unconditional loop, instead of waiting for CHR$(13).
    LET ClockCheck! = TIMER
    LET WhatIsTime! = ClockCheck! - TimeIsNow!
    IF WhatIsTime! >= .01 THEN
        LET CountChange = CountChange + 1
        IF FlashColor = 0 THEN
            DRAW "B M116,158"
            'DRAW "C" + STR$(&HFF000000)
            DRAW "C" + STR$(_RGBA32(240 - (16 * CountChange), 240 - (16 * CountChange), 240 - (16 * CountChange), 255))
            Font "PRESS ENTER"
            IF CountChange = 16 THEN
                CountChange = 0
                ClockTick = ClockTick + 1
                FlashColor = 1
            END IF
        ELSEIF FlashColor = 1 THEN
            DRAW "B M116,158"
            'DRAW "C" + STR$(&HFFFFFFFF)
            DRAW "C" + STR$(_RGBA32(0 + (16 * CountChange), 0 + (16 * CountChange), 0 + (16 * CountChange), 255))
            Font "PRESS ENTER"
            IF CountChange = 16 THEN
                CountChange = 0
                ClockTick = ClockTick + 1
                FlashColor = 0
            END IF
        END IF
        LET TimeIsNow! = TIMER
        LET ClockCheck! = TIMER
        LET WhatIsTime! = 0
    END IF
    colour% = 31 'British English to the rescue! lol
    IF ClockTick = 12 THEN 'Fade out the first list of credits
        'DRAW "C" + STR$(colour% - CountChange)
        DRAW "C" + STR$(_RGBA32(240 - (16 * CountChange), 240 - (16 * CountChange), 240 - (16 * CountChange), 255))
        DRAW "B M20,106"
        Font "KAMION SHANORIKO PROGRAMMED THE ENGINE, AND"
        DRAW "B M66,116"
        Font "WROTE MOST OF THE SOURCE CODE"
        'DRAW "B M11,126"
        'Font "AARON SEVERN WROTE A PIXEL-BY-PIXEL SCROLLING"
        'DRAW "B M22,136"
        'Font "ROUTINE, WHICH KAMION TWEAKED FOR THIS GAME"
    ELSEIF ClockTick = 13 THEN 'Fade in the second list of credits
        colour% = 16
        'DRAW "C" + STR$(colour% + CountChange)
        DRAW "C" + STR$(_RGBA32(0 + (16 * CountChange), 0 + (16 * CountChange), 0 + (16 * CountChange), 255))
        DRAW "B M2,106"
        Font "NINTENDO EAD DEVELOPED SUPER MARIO BROS. 2, WHICH"
        DRAW "B M20,116"
        Font "THIS TECH DEMO'S LEVELS ARE HEAVILY BASED ON"
        DRAW "B M35,126"
        Font "BLACK SQUIRREL PROVIDED THE NES SPRITES"
        DRAW "B M56,136"
        Font "SONIKKU PROVIDED THE SNES SPRITES"
    ELSEIF ClockTick = 26 THEN 'Fade out the second list of credits
        colour% = 31
        'DRAW "C" + STR$(colour% - CountChange)
        DRAW "C" + STR$(_RGBA32(240 - (16 * CountChange), 240 - (16 * CountChange), 240 - (16 * CountChange), 255))
        DRAW "B M2,106"
        Font "NINTENDO EAD DEVELOPED SUPER MARIO BROS. 2, WHICH"
        DRAW "B M20,116"
        Font "THIS TECH DEMO'S LEVELS ARE HEAVILY BASED ON"
        DRAW "B M35,126"
        Font "BLACK SQUIRREL PROVIDED THE NES SPRITES"
        DRAW "B M56,136"
        Font "SONIKKU PROVIDED THE SNES SPRITES"
    ELSEIF ClockTick = 27 THEN 'Fade in the third list of credits
        colour% = 16
        'DRAW "C" + STR$(colour% + CountChange)
        DRAW "C" + STR$(_RGBA32(0 + (16 * CountChange), 0 + (16 * CountChange), 0 + (16 * CountChange), 255))
        DRAW "B M19,106"
        Font "KAMION, DECEASED SUPERIOR TECHNICIAN, SZYMON"
        DRAW "B M5,116"
        Font "MATUSZEWSKI AND KEVIN MACLEOD CONTRIBUTED MUSIC"
        DRAW "B M28,126"
        Font "WWW.FREESFX.CO.UK, MIKE KOENIG AND MUSOPEN,"
        DRAW "B M35,136"
        Font "AMONG OTHERS, CONTRIBUTED SOUND EFFECTS"
    ELSEIF ClockTick = 40 THEN 'Fade out the third list of credits
        colour% = 31
        'DRAW "C" + STR$(colour% - CountChange)
        DRAW "C" + STR$(_RGBA32(240 - (16 * CountChange), 240 - (16 * CountChange), 240 - (16 * CountChange), 255))
        DRAW "B M19,106"
        Font "KAMION, DECEASED SUPERIOR TECHNICIAN, SZYMON"
        DRAW "B M5,116"
        Font "MATUSZEWSKI AND KEVIN MACLEOD CONTRIBUTED MUSIC"
        DRAW "B M28,126"
        Font "WWW.FREESFX.CO.UK, MIKE KOENIG AND MUSOPEN,"
        DRAW "B M35,136"
        Font "AMONG OTHERS, CONTRIBUTED SOUND EFFECTS"
    ELSEIF ClockTick = 41 THEN 'Fade in the first list of credits
        colour% = 16
        'DRAW "C" + STR$(colour% + CountChange)
        DRAW "C" + STR$(_RGBA32(0 + (16 * CountChange), 0 + (16 * CountChange), 0 + (16 * CountChange), 255))
        DRAW "B M20,106"
        Font "KAMION SHANORIKO PROGRAMMED THE ENGINE, AND"
        DRAW "B M66,116"
        Font "WROTE MOST OF THE SOURCE CODE"
        'DRAW "B M11,126"
        'Font "AARON SEVERN WROTE A PIXEL-BY-PIXEL SCROLLING"
        'DRAW "B M22,136"
        'Font "ROUTINE, WHICH KAMION TWEAKED FOR THIS GAME"
    END IF
    IF ClockTick = 42 THEN ClockTick = 0 'Reset it, so we can start over.
    IF _KEYDOWN(13) THEN _SNDPLAY SEF(4): EXIT DO
LOOP

'This seems to clear the keyboard buffer well enough on the Linux version,
'from what I've seen. On the Windows version, however, it seems to think that,
'if you press ENTER once, you're gonna keep randomly pressing it on the main
'menu screen, even if your hands aren't even on the keyboard.
DO
    sparevar& = _KEYHIT
LOOP UNTIL sparevar& = 0

'Should I let beta-testers play this tech demo, so they can test and help me
'tweak/fix the game physics? If I do, I should put in options to change the
'control keys (including left- and right-handed "presets"), along with
'anything else I should probably put in.
LINE (0, 53)-(320, 188), _RGBA32(0, 0, 0, 255), BF
DRAW "B M0,55"
DRAW "C" + STR$(&HFFFFFFFF)
Font "BUILD 12"
DRAW "B M107,55"
Font "2014 FOR SURE!!!"
DRAW "B M250,55"
Font "NOV 23 2013"
DRAW "B M133,90"
Font "START"
DRAW "B M122,100 C" + STR$(&HFF333333)
Font "CONTINUE"
DRAW "B M136,110 C" + STR$(&HFF333333)
Font "SAVE"
DRAW "B M126,120 C" + STR$(&HFFFFFFFF)
Font "OPTIONS"
DRAW "B M111,130 C" + STR$(&HFF333333)
Font "LEADERBOARD"
DRAW "B M107,145 C" + STR$(&HFFFFFFFF)
Font "EXIT THE DEMO"
DRAW "B M25,188 C" + STR$(&HFFFFFFFF)
Font "UP AND DOWN MOVES, ENTER SELECTS, ESC EXITS"

'Make sure nobody's holding down the ENTER key before we reach this routine
DO
    waitkey& = _KEYHIT
LOOP WHILE waitkey& <> 0 OR _KEYDOWN(13) 'Chill here if any keys are pressed.
LET FlashColor = 1
LET Highlight = 1
LET Quadrant = 0
LET TimeIsNow! = TIMER
DO ' EDIT: Unconditional loop, instead of waiting for CHR$(13).
    LET ClockCheck! = TIMER
    LET WhatIsTime! = ClockCheck! - TimeIsNow!
    IF WhatIsTime! >= .01 THEN
        IF mp = 0 THEN FlashColor = FlashColor + 1 ELSE FlashColor = FlashColor - 1
        IF FlashColor = 17 THEN mp = 1
        IF FlashColor = 0 THEN mp = 0
        'IF FlashColor THEN LET FlashColor = 0 ELSE LET FlashColor = 1

        IF Quadrant = 0 THEN 'The main menu.
            DRAW "C" + STR$(&HFFFFFFFF) + " BM133,90": Font "START"
            DRAW "C" + STR$(&HFF333333) + " BM122,100": Font "CONTINUE"
            DRAW "C" + STR$(&HFF333333) + " BM136,110": Font "SAVE" '<-- What should I change this option to?
            DRAW "C" + STR$(&HFFFFFFFF) + " BM126,120": Font "OPTIONS"
            DRAW "C" + STR$(&HFF333333) + " BM111,130": Font "LEADERBOARD"
            DRAW "C" + STR$(&HFFFFFFFF) + " BM107,145": Font "EXIT THE DEMO"
        ELSEIF Quadrant = 1 THEN 'The options in the "Start" (new game) menu.
            DRAW "C" + STR$(&HFFFFFFFF) + " BM80,85": Font "START FROM WHICH LEVEL?"
            DRAW "C" + STR$(&HFFFFFFFF): CenterFont "SUBCON 1", 110
            DRAW "C" + STR$(&HFFFFFFFF): CenterFont "SUBCON 2", 120
            DRAW "C" + STR$(&HFFFFFFFF): CenterFont "PHRINGDOTT 3", 130
        ELSEIF Quadrant = 2 THEN 'The options in the "Options" menu.
            DRAW "C" + STR$(&HFFFFFFFF) + " BM126,65": Font "OPTIONS"
            DRAW "C" + STR$(&HFFFFFFFF) + " BM1,75": Font "WEAPON OF CHOICE"
            DRAW "C" + STR$(&HFFFFFFFF) + " BM1,85": Font "RECONFIGURE CONTROLS"
            DRAW "C" + STR$(&HFFFFFFFF) + " BM1,115": Font "BACKGROUND MUSIC"
            DRAW "C" + STR$(&HFF333333) + " BM1,100": Font "LEADERBOARD OPTIONS"
            DRAW "C" + STR$(&HFFFFFFFF) + " BM1,125": Font "SOUND TEST"
            DRAW "C" + STR$(&HFFFFFFFF) + " BM1,135": Font "MUSIC TEST"
        ELSEIF Quadrant = 3 THEN 'The "Reconfigure Controls" menu.
            DRAW "C" + STR$(&HFFFFFFFF): CenterFont "CONTROL OPTIONS", 65
            DRAW "C" + STR$(&HFFFFFFFF) + " BM1,80": Font "CONTROL SCHEME PRESET"
            DRAW "C" + STR$(&HFFFFFFFF) + " BM1,95": Font "UP"
            DRAW "C" + STR$(&HFFFFFFFF) + " BM1,105": Font "DOWN"
            DRAW "C" + STR$(&HFFFFFFFF) + " BM1,115": Font "LEFT"
            DRAW "C" + STR$(&HFFFFFFFF) + " BM1,125": Font "RIGHT"
            DRAW "C" + STR$(&HFFFFFFFF) + " BM1,135": Font "RUN"
            DRAW "C" + STR$(&HFFFFFFFF) + " BM1,145": Font "JUMP"
            DRAW "C" + STR$(&HFFFFFFFF) + " BM1,155": Font "ATTACK"
            DRAW "C" + STR$(&HFFFFFFFF) + " BM1,165": Font "PAUSE"
            LINE (0, 182)-(319, 188), _RGBA32(0, 0, 0, 255), BF: DRAW "C" + STR$(&HFFFFFFFF): IF Quadrant = 3 AND Highlight > 12 AND Highlight < 21 THEN CenterFont "ARROW KEYS MOVE, ENTER CHANGES KEY, ESC BACKS OUT", 188 ELSE CenterFont "ARROW KEYS MOVE, ENTER TOGGLES, ESC BACKS OUT", 188
        ELSEIF Quadrant = 4 THEN 'The list of sound effects from the "Sound Test".
        END IF

        'First row is the main menu.
        IF Highlight = 1 THEN DRAW "C" + STR$(_RGBA32(0, 178, 0, (FlashColor * 15))) + " BM133,90": Font "START"
        'Initially skipping "CONTINUE", but "SAVE" should be changed, because you can't save from the main menu.
        IF Highlight = 2 THEN DRAW "C" + STR$(_RGBA32(0, 178, 0, (FlashColor * 15))) + " BM126,120": Font "OPTIONS"
        '...and "LEADERBOARD", since that'll be another full game feature
        IF Highlight = 3 THEN DRAW "C" + STR$(_RGBA32(0, 178, 0, (FlashColor * 15))) + " BM107,145": Font "EXIT THE DEMO"
        'Second row is the options in the "Start" (new game) menu. I might replace this with an overworld map.
        IF Highlight = 4 THEN DRAW "C" + STR$(_RGBA32(0, 178, 0, (FlashColor * 15))): CenterFont "SUBCON 1", 110
        IF Highlight = 5 THEN DRAW "C" + STR$(_RGBA32(0, 178, 0, (FlashColor * 15))): CenterFont "SUBCON 2", 120
        IF Highlight = 6 THEN DRAW "C" + STR$(_RGBA32(0, 178, 0, (FlashColor * 15))): CenterFont "PHRINGDOTT 3", 130
        'Third row is the options in the "Options" menu.
        IF Highlight = 7 THEN DRAW "C" + STR$(_RGBA32(0, 178, 0, (FlashColor * 15))) + " BM1,75": Font "WEAPON OF CHOICE"
        IF Highlight = 8 THEN DRAW "C" + STR$(_RGBA32(0, 178, 0, (FlashColor * 15))) + " BM1,85": Font "RECONFIGURE CONTROLS"
        IF Highlight = 9 THEN DRAW "C" + STR$(_RGBA32(0, 178, 0, (FlashColor * 15))) + " BM1,115": Font "BACKGROUND MUSIC"
        IF Highlight = 10 THEN DRAW "C" + STR$(_RGBA32(0, 178, 0, (FlashColor * 15))) + " BM1,125": Font "SOUND TEST"
        IF Highlight = 11 THEN DRAW "C" + STR$(_RGBA32(0, 178, 0, (FlashColor * 15))) + " BM1,135": Font "MUSIC TEST"
        'Fourth row is the options in the "Configure Controls" menu.
        IF Highlight = 12 THEN DRAW "C" + STR$(_RGBA32(0, 178, 0, (FlashColor * 15))) + " BM1,80": Font "CONTROL SCHEME PRESET"
        IF Highlight = 13 THEN DRAW "C" + STR$(_RGBA32(0, 178, 0, (FlashColor * 15))) + " BM1,95": Font "UP"
        IF Highlight = 14 THEN DRAW "C" + STR$(_RGBA32(0, 178, 0, (FlashColor * 15))) + " BM1,105": Font "DOWN"
        IF Highlight = 15 THEN DRAW "C" + STR$(_RGBA32(0, 178, 0, (FlashColor * 15))) + " BM1,115": Font "LEFT"
        IF Highlight = 16 THEN DRAW "C" + STR$(_RGBA32(0, 178, 0, (FlashColor * 15))) + " BM1,125": Font "RIGHT"
        IF Highlight = 17 THEN DRAW "C" + STR$(_RGBA32(0, 178, 0, (FlashColor * 15))) + " BM1,135": Font "RUN"
        IF Highlight = 18 THEN DRAW "C" + STR$(_RGBA32(0, 178, 0, (FlashColor * 15))) + " BM1,145": Font "JUMP"
        IF Highlight = 19 THEN DRAW "C" + STR$(_RGBA32(0, 178, 0, (FlashColor * 15))) + " BM1,155": Font "ATTACK"
        IF Highlight = 20 THEN DRAW "C" + STR$(_RGBA32(0, 178, 0, (FlashColor * 15))) + " BM1,165": Font "PAUSE"
        'Fifth row is the list of sound effects from the "Sound Test".

        LET TimeIsNow! = TIMER
        LET ClockCheck! = TIMER
        LET WhatIsTime! = 0
        _DISPLAY
    END IF
    SELECT CASE _KEYHIT 'Works with the keyboard repeat rate, unlike _KEYDOWN.
        CASE 18432 ' Up arrow
            Highlight = Highlight - 1
            IF Quadrant = 0 THEN IF Highlight = 0 THEN Highlight = 3
            IF Quadrant = 1 THEN IF Highlight = 3 THEN Highlight = 6
            IF Quadrant = 2 THEN IF Highlight = 6 THEN Highlight = 11
            IF Quadrant = 3 THEN IF Highlight = 11 THEN Highlight = 20
        CASE 20480 ' Down arrow
            Highlight = Highlight + 1
            IF Quadrant = 0 THEN IF Highlight = 4 THEN Highlight = 1
            IF Quadrant = 1 THEN IF Highlight = 7 THEN Highlight = 4
            IF Quadrant = 2 THEN IF Highlight = 12 THEN Highlight = 7
            IF Quadrant = 3 THEN IF Highlight = 21 THEN Highlight = 12
        CASE 13 ' ENTER
            SELECT CASE Highlight 'A tiny bit of code optimization, here.
                CASE 1 'Main Menu > Start
                    _SNDPLAY SEF(4)
                    LINE (0, 59)-(320, 188), _RGBA32(0, 0, 0, 255), BF 'Erase the menu options.
                    DRAW "B M16,188 C" + STR$(&HFFFFFFFF)
                    Font "UP AND DOWN MOVES, ENTER SELECTS, ESC CANCELS"
                    Quadrant = 1
                    Highlight = 4
                CASE 2 'Main Menu > Options
                    _SNDPLAY SEF(4)
                    LINE (0, 59)-(320, 188), _RGBA32(0, 0, 0, 255), BF 'Erase the menu options.
                    DRAW "B M155,75 C" + STR$(&HFF00FF00)
                    Font UCASE$(ContType(cont%))
                    DRAW "B M155,100 C" + STR$(&HFF890000)
                    Font "NOT IMPLEMENTED YET!"
                    DRAW "B M155,115"
                    IF actbgm% >= 0 THEN
                        DRAW "C" + STR$(&HFF00FF00)
                        Font "YES, PLEASE!"
                    ELSE 'Negative number (-1) means music is turned off.
                        DRAW "C" + STR$(&HFFFF0000)
                        Font "NO, THANKS!"
                    END IF
                    DRAW "B M21,188 C" + STR$(&HFFFFFFFF)
                    Font "UP AND DOWN MOVES, ENTER TOGGLES, ESC EXITS"
                    Quadrant = 2
                    Highlight = 7
                CASE 3 'Main Menu > Exit the Demo
                    _SNDPLAY SEF(4)
                    IF actbgm% >= 0 AND bgm& THEN _SNDSTOP bgm&
                    _FULLSCREEN _OFF 'Go back to windowed mode (if it's not)
                    SYSTEM 'Exit the demo
                CASE 4 'Start > Subcon 1 (soon to be replaced with Phringdott 1, when I get to a playable alpha)
                    _SNDPLAY SEF(4)
                    IF actbgm% >= 0 AND bgm& THEN _SNDSTOP bgm&
                    zone$ = "SUBCON 1" 'Zone code
                    LevelData$ = respath$ + "WLDXL1LD.NRT" 'Level and collision data, BGMs, and zone name
                    Foreground1$ = respath$ + "WLDXL1F1.NXT" 'Foreground layer 1
                    Foreground2$ = respath$ + "WLDXL1F2.NXT" 'Foreground layer 2
                    Background1$ = respath$ + "WLDXL1B1.NXT" 'Background layer 1
                    Background2$ = respath$ + "WLDXL1B2.NXT" 'Background layer 2
                    ActionDef$ = respath$ + "WLDXL1AD.KMD" 'Defines monsters and triggers (for right now)
                    ActionTable$ = respath$ + "WLDXL1AT.KMD" 'What monsters and triggers are loaded, and where
                    OPEN LevelData$ FOR INPUT AS #5
                    OPEN Foreground1$ FOR INPUT AS #1
                    OPEN Foreground2$ FOR INPUT AS #2
                    OPEN Background1$ FOR INPUT AS #3
                    OPEN Background2$ FOR INPUT AS #4
                    OPEN ActionDef$ FOR INPUT AS #6
                    OPEN ActionTable$ FOR INPUT AS #7
                    'First quick check: see how many songs should be loaded
                    FOR x = 0 TO 5 'Skip over the first six settings
                        INPUT #5, stuff$
                    NEXT x
                    msn = 0
                    DO 'Then, see how many lines after that, mention songs.
                        INPUT #5, song$
                        IF LEN(song$) > 3 THEN msn = msn + 1 ELSE EXIT DO
                    LOOP
                    'TODO: Throw an error if more than 10 songs are found.
                    adm = 0: atm = 0
                    'Second quick check: how many monsters are in the action def file? (WIP)
                    DO UNTIL EOF(6) 'WIP: Grab three variables each, for right now.
                        INPUT #6, a$, b$, c$
                        adm = adm + 1 'Set the maximum number
                    LOOP
                    'Third quick check: how many entries are in the action table? (WIP)
                    DO UNTIL EOF(7) 'WIP: Grab four variables each, for right now.
                        INPUT #7, j$, k$, l$, m$
                        atm = atm + 1 'Set the maximum number
                    LOOP
                    'Close and re-open level data, action def and action table, to start from the beginning
                    CLOSE #5: OPEN LevelData$ FOR INPUT AS #5
                    CLOSE #6: OPEN ActionDef$ FOR INPUT AS #6
                    CLOSE #7: OPEN ActionTable$ FOR INPUT AS #7
                    'Now we read the level data, for real!
                    INPUT #5, lvname$ 'The name of the level
                    INPUT #5, clock$ 'How high to set the clock
                    INPUT #5, scrnum$ 'How high can we scroll vertically?
                    INPUT #5, stpos$ 'On what screen level do we start?
                    INPUT #5, startx$ 'Then the X and Y coordinates of where
                    INPUT #5, starty$ 'Cricket should be placed, at the start.
                    'Then, a little bit of string to integer conversion...
                    tick = VAL(clock$) 'Set the clock
                    scrcnt% = VAL(scrnum$) 'Set the max number of screens
                    vert% = VAL(stpos$) 'Set the starting screen
                    CKL% = VAL(startx$) 'Set Cricket's starting X coordinate
                    CKT% = VAL(starty$) 'Set Cricket's starting Y coordinate
                    IF actbgm% >= 0 AND bgm& THEN _SNDCLOSE bgm&
                    FOR sf = 0 TO (msn - 1) 'Get each song filename.
                        INPUT #5, sf$ 'This way, the file is positioned at
                        BGM(sf) = sf$ 'the level data, where we need it.
                    NEXT sf 'TODO: Add "respath$ + bgmfldr$ + " to the front of each entry.
                    'BGM(0) = "/media/PHANTOM/IMPULSE/SOTRFALT.IT"
                    'BGM(1) = respath$ + bgmfldr$ + "SubUnderworld.mp3"
                    'BGM(2) = respath$ + bgmfldr$ + "Birdo.mp3"
                    IF actbgm% >= 0 THEN bgm& = _SNDOPEN(BGM(0), "VOL,PAUSE") ELSE prevbgm% = 0
                    EXIT DO
                CASE 5 'Start > Subcon 2 (soon to be replaced with Phringdott 2, when I get to a playable alpha)
                    _SNDPLAY SEF(4)
                    IF actbgm% >= 0 AND bgm& THEN _SNDSTOP bgm&
                    zone$ = "SUBCON 2"
                    LevelData$ = respath$ + "WLDXL2LD.KMD"
                    Foreground1$ = respath$ + "WLDXL2F1.KMD"
                    Foreground2$ = respath$ + "WLDXL2F2.KMD"
                    Background1$ = respath$ + "WLDXL2B1.KMD"
                    Background2$ = respath$ + "WLDXL2B2.KMD"
                    ActionDef$ = respath$ + "WLDXL2AD.KMD"
                    ActionTable$ = respath$ + "WLDXL2AT.KMD"
                    OPEN LevelData$ FOR INPUT AS #5
                    OPEN Foreground1$ FOR INPUT AS #1
                    OPEN Foreground2$ FOR INPUT AS #2
                    OPEN Background1$ FOR INPUT AS #3
                    OPEN Background2$ FOR INPUT AS #4
                    'Quick check, to see how many songs should be loaded
                    FOR x = 0 TO 5 'Skip over the first six settings
                        INPUT #5, stuff$
                    NEXT x
                    msn = 0
                    DO 'Then, see how many lines after that, mention songs.
                        INPUT #5, song$
                        IF LEN(song$) > 3 THEN msn = msn + 1 ELSE EXIT DO
                    LOOP
                    CLOSE #5: OPEN LevelData$ FOR INPUT AS #5
                    'Now we read the level data, for real!
                    INPUT #5, lvname$ 'The name of the level
                    INPUT #5, clock$ 'How high to set the clock
                    INPUT #5, scrnum$ 'How high can we scroll vertically?
                    INPUT #5, stpos$ 'On what screen level do we start?
                    INPUT #5, startx$ 'Then the X and Y coordinates of where
                    INPUT #5, starty$ 'Cricket should be placed, at the start.
                    'Then, a little bit of string to integer conversion...
                    tick = VAL(clock$) 'Set the clock
                    scrcnt% = VAL(scrnum$) 'Set the max number of screens
                    vert% = VAL(stpos$) 'Set the starting screen
                    CKL% = VAL(startx$) 'Set Cricket's starting X coordinate
                    CKT% = VAL(starty$) 'Set Cricket's starting Y coordinate
                    IF actbgm% >= 0 AND bgm& THEN _SNDCLOSE bgm&
                    FOR sf = 0 TO (msn - 1) 'Get each song filename.
                        INPUT #5, sf$ 'This way, the file is positioned at
                        BGM(sf) = sf$ 'the level data, where we need it.
                    NEXT sf
                    'BGM(0) = "/media/PHANTOM/IMPULSE/SOTRFALT.IT"
                    'BGM(1) = respath$ + bgmfldr$ + "SubUnderworld.mp3"
                    'BGM(2) = respath$ + bgmfldr$ + "Birdo.mp3"
                    IF actbgm% >= 0 THEN bgm& = _SNDOPEN(BGM(0), "VOL,PAUSE") ELSE prevbgm% = 0
                    EXIT DO
                CASE 6 'Start > Phringdott 3 (I have an idea for a level, that will either be here, or Phringdott 4)
                    _SNDPLAY SEF(4)
                    zone$ = "PHRINGDOTT 3"
                    IF actbgm% >= 0 AND bgm& THEN _SNDSTOP bgm&
                    LevelData$ = respath$ + "WLDXL3LD.KMD"
                    Foreground1$ = respath$ + "WLDXL3F1.KMD"
                    Foreground2$ = respath$ + "WLDXL3F2.KMD"
                    Background1$ = respath$ + "WLDXL3B1.KMD"
                    Background2$ = respath$ + "WLDXL3B2.KMD"
                    ActionDef$ = respath$ + "WLDXL3AD.KMD"
                    ActionTable$ = respath$ + "WLDXL3AT.KMD"
                    OPEN LevelData$ FOR INPUT AS #5
                    OPEN Foreground1$ FOR INPUT AS #1
                    OPEN Foreground2$ FOR INPUT AS #2
                    OPEN Background1$ FOR INPUT AS #3
                    OPEN Background2$ FOR INPUT AS #4
                    'Quick check, to see how many songs should be loaded
                    FOR x = 0 TO 5 'Skip over the first six settings
                        INPUT #5, stuff$
                    NEXT x
                    msn = 0
                    DO 'Then, see how many lines after that, mention songs.
                        INPUT #5, song$
                        IF LEN(song$) > 3 THEN msn = msn + 1 ELSE EXIT DO
                    LOOP
                    'TODO: Throw an error if more than 10 songs are found.
                    CLOSE #5: OPEN LevelData$ FOR INPUT AS #5
                    'Now we read the level data, for real!
                    INPUT #5, lvname$ 'The name of the level
                    INPUT #5, clock$ 'How high to set the clock
                    INPUT #5, scrnum$ 'How high can we scroll vertically?
                    INPUT #5, stpos$ 'On what screen level do we start?
                    INPUT #5, startx$ 'Then the X and Y coordinates of where
                    INPUT #5, starty$ 'Cricket should be placed, at the start.
                    'Then, a little bit of string to integer conversion...
                    tick = VAL(clock$) 'Set the clock
                    scrcnt% = VAL(scrnum$) 'Set the max number of screens
                    vert% = VAL(stpos$) 'Set the starting screen
                    CKL% = VAL(startx$) 'Set Cricket's starting X coordinate
                    CKT% = VAL(starty$) 'Set Cricket's starting Y coordinate
                    IF actbgm% >= 0 AND bgm& THEN _SNDCLOSE bgm&
                    FOR sf = 0 TO (msn - 1) 'Get each song filename.
                        INPUT #5, sf$ 'This way, the file is positioned at
                        BGM(sf) = sf$ 'the level data, where we need it.
                    NEXT sf
                    'BGM(0) = "/media/PHANTOM/IMPULSE/SOTRFALT.IT"
                    'BGM(1) = respath$ + bgmfldr$ + "SubUnderworld.mp3"
                    'BGM(2) = respath$ + bgmfldr$ + "Mouser.mp3"
                    IF actbgm% >= 0 THEN bgm& = _SNDOPEN(BGM(0), "VOL,PAUSE") ELSE prevbgm% = 0
                    EXIT DO
                CASE 7 'Options > Weapon of Choice (select controller)
                    _SNDPLAY SEF(8)
                    LINE (155, 75)-(319, 66), _RGBA32(0, 0, 0, 255), BF
                    cont% = cont% + 1
                    IF cont% > gp% THEN cont% = 0
                    DRAW "B M155,75 C" + STR$(&HFF00FF00)
                    Font UCASE$(ContType(cont%))
                    'Change the keymaps accordingly.
                    IF cont% = 0 THEN 'Keyboard
                        MoveUp = DefKeys(0, pn, 0, 0): MoveDown = DefKeys(0, pn, 0, 1)
                        MoveLeft = DefKeys(0, pn, 0, 2): MoveRight = DefKeys(0, pn, 0, 3)
                        MoveRun = DefKeys(0, pn, 0, 4): MoveJump = DefKeys(0, pn, 0, 5)
                        MoveAttack = DefKeys(0, pn, 0, 6): MovePause = DefKeys(0, pn, 0, 7)
                    ELSEIF cont% > 0 THEN 'Gamepad or Joystick
                        MoveUp = DefKeys(cont%, pn, 0, 0): ActionUp = DefKeys(cont%, pn, 1, 0)
                        MoveDown = DefKeys(cont%, pn, 0, 1): ActionDown = DefKeys(cont%, pn, 1, 1)
                        MoveLeft = DefKeys(cont%, pn, 0, 2): ActionLeft = DefKeys(cont%, pn, 1, 2)
                        MoveRight = DefKeys(cont%, pn, 0, 3): ActionRight = DefKeys(cont%, pn, 1, 3)
                        MoveRun = DefKeys(cont%, pn, 0, 4): ActionRun = DefKeys(cont%, pn, 1, 4)
                        MoveJump = DefKeys(cont%, pn, 0, 5): ActionJump = DefKeys(cont%, pn, 1, 5)
                        MoveAttack = DefKeys(cont%, pn, 0, 6): ActionAttack = DefKeys(cont%, pn, 1, 6)
                        MovePause = DefKeys(cont%, pn, 0, 7): ActionPause = DefKeys(cont%, pn, 1, 7)
                        FOR kn = 0 TO 7: JoyMoveOp(kn) = DefKeys(cont%, pn, 2, kn): NEXT kn
                    END IF
                CASE 8 'Options > Reconfigure Controls
                    _SNDPLAY SEF(4)
                    LINE (0, 59)-(320, 188), _RGBA32(0, 0, 0, 255), BF 'Erase the menu options.
                    DRAW "B M155,80 C" + STR$(&HFF00FF00)
                    Font UCASE$(ContPrst(cont%, pn))
                    DRAW "B M50,95 C" + STR$(&HFF00FF00)
                    IF cont% = 0 THEN Font ButtonDef$(MoveUp) ELSE Font GamepadDef$(0)
                    DRAW "B M50,105 C" + STR$(&HFF00FF00)
                    IF cont% = 0 THEN Font ButtonDef$(MoveDown) ELSE Font GamepadDef$(1)
                    DRAW "B M50,115 C" + STR$(&HFF00FF00)
                    IF cont% = 0 THEN Font ButtonDef$(MoveLeft) ELSE Font GamepadDef$(2)
                    DRAW "B M50,125 C" + STR$(&HFF00FF00)
                    IF cont% = 0 THEN Font ButtonDef$(MoveRight) ELSE Font GamepadDef$(3)
                    DRAW "B M50,135 C" + STR$(&HFF00FF00)
                    IF cont% = 0 THEN Font ButtonDef$(MoveRun) ELSE Font GamepadDef$(4)
                    DRAW "B M50,145 C" + STR$(&HFF00FF00)
                    IF cont% = 0 THEN Font ButtonDef$(MoveJump) ELSE Font GamepadDef$(5)
                    DRAW "B M50,155 C" + STR$(&HFF00FF00)
                    IF cont% = 0 THEN Font ButtonDef$(MoveAttack) ELSE Font GamepadDef$(6)
                    DRAW "B M50,165 C" + STR$(&HFF00FF00)
                    IF cont% = 0 THEN Font ButtonDef$(MovePause) ELSE Font GamepadDef$(7)
                    DRAW "C" + STR$(&HFFFFFFFF) 'Originally at 18,188
                    CenterFont "ARROW KEYS MOVE, ENTER TOGGLES, ESC BACKS OUT", 188
                    Quadrant = 3
                    Highlight = 12
                CASE 9 'Options > Background Music (no "ambience" option yet)
                    _SNDPLAY SEF(8)
                    LINE (155, 118)-(250, 109), _RGBA32(0, 0, 0, 255), BF
                    DRAW "B M155,115"
                    IF actbgm% >= 0 THEN
                        actbgm% = -1
                        DRAW "C" + STR$(&HFFFF0000)
                        Font "NO, THANKS!"
                        _SNDSTOP bgm&
                        _SNDCLOSE bgm&
                    ELSEIF actbgm% = -1 THEN
                        actbgm% = 0
                        DRAW "C" + STR$(&HFF00FF00)
                        Font "YES, PLEASE!"
                        bgm& = _SNDOPEN(BGM(0), "VOL,PAUSE")
                        _SNDLOOP bgm&
                    END IF
                CASE 10 'Options > Sound Test ... should optimize this later.
                    _SNDPLAY SEF(4)
                    IF actbgm% >= 0 AND bgm& THEN _SNDPAUSE bgm& 'Better spot.
                    LINE (0, 59)-(320, 188), _RGBA32(0, 0, 0, 255), BF 'Erase the menu options.
                    DRAW "B M119,65 C" + STR$(&HFFFFFFFF)
                    Font "SOUND TEST"
                    DRAW "B M1,76"
                    Font "00 " + UCASE$(SEN(0))
                    DRAW "B M1,86"
                    Font "01 " + UCASE$(SEN(1))
                    DRAW "B M1,96"
                    Font "02 " + UCASE$(SEN(2))
                    DRAW "B M1,106"
                    Font "03 " + UCASE$(SEN(3))
                    DRAW "B M1,116"
                    Font "04 " + UCASE$(SEN(4))
                    DRAW "B M1,126"
                    Font "05 " + UCASE$(SEN(5))
                    DRAW "B M1,136"
                    Font "06 " + UCASE$(SEN(6))
                    DRAW "B M1,146"
                    Font "07 " + UCASE$(SEN(7))
                    DRAW "B M1,156"
                    Font "08 " + UCASE$(SEN(8))
                    DRAW "B M1,166"
                    Font "09 " + UCASE$(SEN(9))
                    DRAW "B M1,176"
                    Font "10 " + UCASE$(SEN(10))
                    DRAW "B M18,188"
                    Font "UP AND DOWN MOVES, ENTER PLAYS, ESC BACKS OUT"
                    Quadrant = 4
                CASE 11 'Options > Music Test
                    _SNDPLAY SEF(4)
                    THAT = 0
                    'This should call a subcommand that'll bring up "myCrick"
                CASE 12 'Options > Reconfigure Controls > Control Scheme Preset
                    _SNDPLAY SEF(8)
                    LINE (155, 80)-(319, 71), _RGBA32(0, 0, 0, 255), BF 'Erase the preset name, since it's changing
                    pn = pn + 1
                    IF pn > 2 THEN pn = 0
                    DRAW "B M155,80 C" + STR$(&HFF00FF00)
                    Font UCASE$(ContPrst(cont%, pn))
                    LINE (50, 88)-(320, 173), _RGBA32(0, 0, 0, 255), BF 'Erase the keymaps, since we're changing them
                    IF cont% = 0 THEN 'Keyboard
                        MoveUp = DefKeys(0, pn, 0, 0): MoveDown = DefKeys(0, pn, 0, 1)
                        MoveLeft = DefKeys(0, pn, 0, 2): MoveRight = DefKeys(0, pn, 0, 3)
                        MoveRun = DefKeys(0, pn, 0, 4): MoveJump = DefKeys(0, pn, 0, 5)
                        MoveAttack = DefKeys(0, pn, 0, 6): MovePause = DefKeys(0, pn, 0, 7)
                    ELSE 'Gamepad or Joystick
                        MoveUp = DefKeys(cont%, pn, 0, 0): ActionUp = DefKeys(cont%, pn, 1, 0)
                        MoveDown = DefKeys(cont%, pn, 0, 1): ActionDown = DefKeys(cont%, pn, 1, 1)
                        MoveLeft = DefKeys(cont%, pn, 0, 2): ActionLeft = DefKeys(cont%, pn, 1, 2)
                        MoveRight = DefKeys(cont%, pn, 0, 3): ActionRight = DefKeys(cont%, pn, 1, 3)
                        MoveRun = DefKeys(cont%, pn, 0, 4): ActionRun = DefKeys(cont%, pn, 1, 4)
                        MoveJump = DefKeys(cont%, pn, 0, 5): ActionJump = DefKeys(cont%, pn, 1, 5)
                        MoveAttack = DefKeys(cont%, pn, 0, 6): ActionAttack = DefKeys(cont%, pn, 1, 6)
                        MovePause = DefKeys(cont%, pn, 0, 7): ActionPause = DefKeys(cont%, pn, 1, 7)
                        FOR kn = 0 TO 7: JoyMoveOp(kn) = DefKeys(cont%, pn, 2, kn): NEXT kn
                    END IF 'Now redraw the new keymaps
                    DRAW "B M50,95 C" + STR$(&HFF00FF00)
                    IF cont% = 0 THEN Font ButtonDef$(MoveUp) ELSE Font GamepadDef$(0)
                    DRAW "B M50,105 C" + STR$(&HFF00FF00)
                    IF cont% = 0 THEN Font ButtonDef$(MoveDown) ELSE Font GamepadDef$(1)
                    DRAW "B M50,115 C" + STR$(&HFF00FF00)
                    IF cont% = 0 THEN Font ButtonDef$(MoveLeft) ELSE Font GamepadDef$(2)
                    DRAW "B M50,125 C" + STR$(&HFF00FF00)
                    IF cont% = 0 THEN Font ButtonDef$(MoveRight) ELSE Font GamepadDef$(3)
                    DRAW "B M50,135 C" + STR$(&HFF00FF00)
                    IF cont% = 0 THEN Font ButtonDef$(MoveRun) ELSE Font GamepadDef$(4)
                    DRAW "B M50,145 C" + STR$(&HFF00FF00)
                    IF cont% = 0 THEN Font ButtonDef$(MoveJump) ELSE Font GamepadDef$(5)
                    DRAW "B M50,155 C" + STR$(&HFF00FF00)
                    IF cont% = 0 THEN Font ButtonDef$(MoveAttack) ELSE Font GamepadDef$(6)
                    DRAW "B M50,165 C" + STR$(&HFF00FF00)
                    IF cont% = 0 THEN Font ButtonDef$(MovePause) ELSE Font GamepadDef$(7)
                CASE 13: GOSUB Remap
                CASE 14: GOSUB Remap
                CASE 15: GOSUB Remap
                CASE 16: GOSUB Remap
                CASE 17: GOSUB Remap
                CASE 18: GOSUB Remap
                CASE 19: GOSUB Remap
                CASE 20: GOSUB Remap
            END SELECT
        CASE 27 'ESC
            IF Quadrant = 0 THEN 'Main menu
                'Maybe put in an "Are you sure?" screen here?
                IF actbgm% >= 0 AND bgm& THEN
                    FOR v = 10 TO 0 STEP -1
                        _LIMIT 10
                        _SNDVOL bgm&, (v / 10)
                    NEXT v
                    _SNDSTOP bgm&
                END IF
                IF _FULLSCREEN THEN _FULLSCREEN _OFF
                SYSTEM
            ELSEIF Quadrant = 3 THEN 'Reconfigure Controls menu
                _SNDPLAY SEF(1)
                LINE (0, 59)-(320, 188), _RGBA32(0, 0, 0, 255), BF 'Erase the menu options.
                DRAW "B M155,75 C" + STR$(&HFF00FF00)
                Font UCASE$(ContType(cont%))
                DRAW "B M155,100 C" + STR$(&HFFAA0000)
                Font "NOT IMPLEMENTED YET!"
                DRAW "B M155,115"
                IF actbgm% >= 0 THEN
                    DRAW "C" + STR$(&HFF00FF00)
                    Font "YES, PLEASE!"
                ELSE 'Negative number (-1) means music is turned off.
                    DRAW "C" + STR$(&HFFFF0000)
                    Font "NO, THANKS!"
                END IF
                DRAW "B M21,188 C" + STR$(&HFFFFFFFF)
                Font "UP AND DOWN MOVES, ENTER TOGGLES, ESC EXITS"
                Quadrant = 2
                Highlight = 8
            ELSEIF Quadrant = 1 OR 2 THEN 'Start (new game) and Options menus
                _SNDPLAY SEF(1) 'For lack of a better "back up" sound... xD
                IF actbgm% >= 0 THEN IF _SNDPAUSED(bgm&) THEN _SNDPLAY bgm&
                LINE (0, 59)-(320, 188), _RGBA32(0, 0, 0, 255), BF
                DRAW "B M25,188 C" + STR$(&HFFFFFFFF)
                Font "UP AND DOWN MOVES, ENTER SELECTS, ESC EXITS"
                IF Quadrant = 1 THEN Highlight = 1 ELSE Highlight = 2
                Quadrant = 0
            END IF
    END SELECT
LOOP

EXIT SUB

Remap: 'SELECT CASE wouldn't cooperate with me, so I just moved the key remapping into a subroutine.
LINE (50, (78 + ((Highlight - 12) * 10)))-(320, (86 + ((Highlight - 12) * 10))), _RGBA32(0, 0, 0, 255), BF
DRAW "B M50, " + STR$(85 + ((Highlight - 12) * 10)) + " C" + STR$(&HFF009900)
IF cont% = 0 THEN Font "PRESS THE NEW KEY, OR ESC TO CANCEL" ELSE Font "PRESS BUTTON, THUMBSTICK, OR ESC TO CANCEL"
_DISPLAY
_SNDPLAY SEF(8)
IF cont% = 0 THEN kc& = _KEYHIT: IF kc& = 13 THEN DO: kc& = _KEYHIT: LOOP WHILE kc& = 13 'Stop holding ENTER down!
IF cont% = 0 THEN
    DO: kg& = _KEYHIT: LOOP WHILE kg& <= 0 'Only capture a positive keycode.
    IF kg& = 27 THEN 'If the player pressed ESC...
        _SNDPLAY SEF(1)
        LINE (50, (78 + ((Highlight - 12) * 10)))-(320, (86 + ((Highlight - 12) * 10))), _RGBA32(0, 0, 0, 255), BF
    ELSE 'Or if the player actually pressed a different key...
        _SNDPLAY SEF(4)
        LINE (50, (78 + ((Highlight - 12) * 10)))-(320, (86 + ((Highlight - 12) * 10))), _RGBA32(0, 0, 0, 255), BF
        IF Highlight = 13 THEN MoveUp = kg&
        IF Highlight = 14 THEN MoveDown = kg&
        IF Highlight = 15 THEN MoveLeft = kg&
        IF Highlight = 16 THEN MoveRight = kg&
        IF Highlight = 17 THEN MoveRun = kg&
        IF Highlight = 18 THEN MoveJump = kg&
        IF Highlight = 19 THEN MoveAttack = kg&
        IF Highlight = 20 THEN MovePause = kg&
    END IF
ELSEIF cont% > 0 THEN
    sax = 0: say = 0: sbx = 0: sby = 0: dpx = 0: dpy = 0
    DO
        km& = _KEYHIT 'For gamepad checking, this just checks to see if the user presses ESC, instead.
        IF km& = 27 THEN _SNDPLAY SEF(1): EXIT DO 'Stop checking the active gamepad if ESC is pressed
        xc = cont% * 2
        sax = STICK(xc - 2, 1)
        IF sax > 249 OR sax < 6 THEN EXIT DO
        say = STICK(xc - 1, 1)
        IF say > 249 OR say < 6 THEN EXIT DO
        sbx = STICK(xc - 2, 2)
        IF sbx > 249 OR sbx < 6 THEN EXIT DO
        sby = STICK(xc - 1, 2)
        IF sby > 249 OR sby < 6 THEN EXIT DO
        dpx = STICK(xc - 2, 3)
        IF dpx > 249 OR dpx < 6 THEN EXIT DO
        dpy = STICK(xc - 1, 3)
        IF dpy > 249 OR dpy < 6 THEN EXIT DO
        IF cont% MOD 2 = 0 THEN bp = STRIG((cb * 4) + 3, cont%) ELSE bp = STRIG((cb * 4) + 1, cont%)
        IF bp THEN
            IF cont% MOD 2 = 0 THEN bn = ((cb * 4) + 3): JoyMoveOp(Highlight - 13) = 0: EXIT DO
            IF cont% MOD 2 = 1 THEN bn = ((cb * 4) + 1): JoyMoveOp(Highlight - 13) = 0: EXIT DO
        END IF
        IF bn = 0 THEN cb = cb + 1: IF cb = 30 THEN cb = 0
        bp = 0: bn = 0
    LOOP 'Unconditional loop; pressing a gamepad button or thumbstick, or pressing ESC on the keyboard, will end this
    IF km& <> 27 THEN _SNDPLAY SEF(4)
    LINE (50, (78 + ((Highlight - 12) * 10)))-(320, (86 + ((Highlight - 12) * 10))), _RGBA32(0, 0, 0, 255), BF
    IF Highlight = 13 THEN
        IF sax > 249 OR sax < 6 THEN MoveUp = sax: ActionUp = (xc - 2): JoyMoveOp(0) = 1
        IF say > 249 OR say < 6 THEN MoveUp = say: ActionUp = (xc - 1): JoyMoveOp(0) = 1
        IF sbx > 249 OR sbx < 6 THEN MoveUp = sbx: ActionUp = (xc - 2): JoyMoveOp(0) = 2
        IF sby > 249 OR sby < 6 THEN MoveUp = sby: ActionUp = (xc - 1): JoyMoveOp(0) = 2
        IF dpx > 249 OR dpx < 6 THEN MoveUp = dpx: ActionUp = (xc - 2): JoyMoveOp(0) = 3
        IF dpy > 249 OR dpy < 6 THEN MoveUp = dpy: ActionUp = (xc - 1): JoyMoveOp(0) = 3
        IF bn > 0 THEN MoveUp = bn: ActionUp = cont%
    ELSEIF Highlight = 14 THEN
        IF sax > 249 OR sax < 6 THEN MoveDown = sax: ActionDown = (xc - 2): JoyMoveOp(1) = 1
        IF say > 249 OR say < 6 THEN MoveDown = say: ActionDown = (xc - 1): JoyMoveOp(1) = 1
        IF sbx > 249 OR sbx < 6 THEN MoveDown = sbx: ActionDown = (xc - 2): JoyMoveOp(1) = 2
        IF sby > 249 OR sby < 6 THEN MoveDown = sby: ActionDown = (xc - 1): JoyMoveOp(1) = 2
        IF dpx > 249 OR dpx < 6 THEN MoveDown = dpx: ActionDown = (xc - 2): JoyMoveOp(1) = 3
        IF dpy > 249 OR dpy < 6 THEN MoveDown = dpy: ActionDown = (xc - 1): JoyMoveOp(1) = 3
        IF bn > 0 THEN MoveDown = bn: ActionDown = cont%
    ELSEIF Highlight = 15 THEN
        IF sax > 249 OR sax < 6 THEN MoveLeft = sax: ActionLeft = (xc - 2): JoyMoveOp(2) = 1
        IF say > 249 OR say < 6 THEN MoveLeft = say: ActionLeft = (xc - 1): JoyMoveOp(2) = 1
        IF sbx > 249 OR sbx < 6 THEN MoveLeft = sbx: ActionLeft = (xc - 2): JoyMoveOp(2) = 2
        IF sby > 249 OR sby < 6 THEN MoveLeft = sby: ActionLeft = (xc - 1): JoyMoveOp(2) = 2
        IF dpx > 249 OR dpx < 6 THEN MoveLeft = dpx: ActionLeft = (xc - 2): JoyMoveOp(2) = 3
        IF dpy > 249 OR dpy < 6 THEN MoveLeft = dpy: ActionLeft = (xc - 1): JoyMoveOp(2) = 3
        IF bn > 0 THEN MoveLeft = bn: ActionLeft = cont%
    ELSEIF Highlight = 16 THEN
        IF sax > 249 OR sax < 6 THEN MoveRight = sax: ActionRight = (xc - 2): JoyMoveOp(3) = 1
        IF say > 249 OR say < 6 THEN MoveRight = say: ActionRight = (xc - 1): JoyMoveOp(3) = 1
        IF sbx > 249 OR sbx < 6 THEN MoveRight = sbx: ActionRight = (xc - 2): JoyMoveOp(3) = 2
        IF sby > 249 OR sby < 6 THEN MoveRight = sby: ActionRight = (xc - 1): JoyMoveOp(3) = 2
        IF dpx > 249 OR dpx < 6 THEN MoveRight = dpx: ActionRight = (xc - 2): JoyMoveOp(3) = 3
        IF dpy > 249 OR dpy < 6 THEN MoveRight = dpy: ActionRight = (xc - 1): JoyMoveOp(3) = 3
        IF bn > 0 THEN MoveRight = bn: ActionRight = cont%
    ELSEIF Highlight = 17 THEN
        IF sax > 249 OR sax < 6 THEN MoveRun = sax: ActionRun = (xc - 2): JoyMoveOp(4) = 1
        IF say > 249 OR say < 6 THEN MoveRun = say: ActionRun = (xc - 1): JoyMoveOp(4) = 1
        IF sbx > 249 OR sbx < 6 THEN MoveRun = sbx: ActionRun = (xc - 2): JoyMoveOp(4) = 2
        IF sby > 249 OR sby < 6 THEN MoveRun = sby: ActionRun = (xc - 1): JoyMoveOp(4) = 2
        IF dpx > 249 OR dpx < 6 THEN MoveRun = dpx: ActionRun = (xc - 2): JoyMoveOp(4) = 3
        IF dpy > 249 OR dpy < 6 THEN MoveRun = dpy: ActionRun = (xc - 1): JoyMoveOp(4) = 3
        IF bn > 0 THEN MoveRun = bn: ActionRun = cont%
    ELSEIF Highlight = 18 THEN
        IF sax > 249 OR sax < 6 THEN MoveJump = sax: ActionJump = (xc - 2): JoyMoveOp(5) = 1
        IF say > 249 OR say < 6 THEN MoveJump = say: ActionJump = (xc - 1): JoyMoveOp(5) = 1
        IF sbx > 249 OR sbx < 6 THEN MoveJump = sbx: ActionJump = (xc - 2): JoyMoveOp(5) = 2
        IF sby > 249 OR sby < 6 THEN MoveJump = sby: ActionJump = (xc - 1): JoyMoveOp(5) = 2
        IF dpx > 249 OR dpx < 6 THEN MoveJump = dpx: ActionJump = (xc - 2): JoyMoveOp(5) = 3
        IF dpy > 249 OR dpy < 6 THEN MoveJump = dpy: ActionJump = (xc - 1): JoyMoveOp(5) = 3
        IF bn > 0 THEN MoveJump = bn: ActionJump = cont%
    ELSEIF Highlight = 19 THEN
        IF sax > 249 OR sax < 6 THEN MoveAttack = sax: ActionAttack = (xc - 2): JoyMoveOp(6) = 1
        IF say > 249 OR say < 6 THEN MoveAttack = say: ActionAttack = (xc - 1): JoyMoveOp(6) = 1
        IF sbx > 249 OR sbx < 6 THEN MoveAttack = sbx: ActionAttack = (xc - 2): JoyMoveOp(6) = 2
        IF sby > 249 OR sby < 6 THEN MoveAttack = sby: ActionAttack = (xc - 1): JoyMoveOp(6) = 2
        IF dpx > 249 OR dpx < 6 THEN MoveAttack = dpx: ActionAttack = (xc - 2): JoyMoveOp(6) = 3
        IF dpy > 249 OR dpy < 6 THEN MoveAttack = dpy: ActionAttack = (xc - 1): JoyMoveOp(6) = 3
        IF bn > 0 THEN MoveAttack = bn: ActionAttack = cont%
    ELSEIF Highlight = 20 THEN
        IF sax > 249 OR sax < 6 THEN MovePause = sax: ActionPause = (xc - 2): JoyMoveOp(7) = 1
        IF say > 249 OR say < 6 THEN MovePause = say: ActionPause = (xc - 1): JoyMoveOp(7) = 1
        IF sbx > 249 OR sbx < 6 THEN MovePause = sbx: ActionPause = (xc - 2): JoyMoveOp(7) = 2
        IF sby > 249 OR sby < 6 THEN MovePause = sby: ActionPause = (xc - 1): JoyMoveOp(7) = 2
        IF dpx > 249 OR dpx < 6 THEN MovePause = dpx: ActionPause = (xc - 2): JoyMoveOp(7) = 3
        IF dpy > 249 OR dpy < 6 THEN MovePause = dpy: ActionPause = (xc - 1): JoyMoveOp(7) = 3
        IF bn > 0 THEN MovePause = bn: ActionPause = cont%
    END IF
END IF
DRAW "B M50, " + STR$(85 + ((Highlight - 12) * 10)) + " C" + STR$(&HFF00FF00)
IF cont% = 0 THEN
    IF Highlight = 13 THEN Font ButtonDef$(MoveUp)
    IF Highlight = 14 THEN Font ButtonDef$(MoveDown)
    IF Highlight = 15 THEN Font ButtonDef$(MoveLeft)
    IF Highlight = 16 THEN Font ButtonDef$(MoveRight)
    IF Highlight = 17 THEN Font ButtonDef$(MoveRun)
    IF Highlight = 18 THEN Font ButtonDef$(MoveJump)
    IF Highlight = 19 THEN Font ButtonDef$(MoveAttack)
    IF Highlight = 20 THEN Font ButtonDef$(MovePause)
ELSEIF cont% > 0 THEN
    IF Highlight = 13 THEN Font GamepadDef$(0)
    IF Highlight = 14 THEN Font GamepadDef$(1)
    IF Highlight = 15 THEN Font GamepadDef$(2)
    IF Highlight = 16 THEN Font GamepadDef$(3)
    IF Highlight = 17 THEN Font GamepadDef$(4)
    IF Highlight = 18 THEN Font GamepadDef$(5)
    IF Highlight = 19 THEN Font GamepadDef$(6)
    IF Highlight = 20 THEN Font GamepadDef$(7)
END IF
RETURN

END SUB

REM $STATIC
SUB FlameWareLogo
CLS
DRAW "C" + STR$(&HFF999999)
DRAW "B M50,210" '50, 180
Font "F L A M E W A R E   Z E R O N E X E"
DRAW "B M70,190" '70, 160
DRAW "C" + STR$(&HFF990000)
DRAW "R172" '172
DRAW "U160" '140
DRAW "H3" '3
DRAW "L166" '166
DRAW "G3" '3
DRAW "D160" '140
DRAW "B R10" '10
DRAW "U90" '80
DRAW "R152" '152
DRAW "D90" '80
DRAW "B U116" '90
DRAW "B L20" '20
PAINT STEP(5, 10), &HFF990000, &HFF990000 '5, 5
DRAW "C" + STR$(&HFF999999)
DRAW "L123" '123
DRAW "U57" '52
DRAW "R123" '123
DRAW "D57" '52
DRAW "L123" '123
DRAW "B U51" '46
DRAW "B R25" '25
DRAW "L10" '10
DRAW "D45" '40
DRAW "R10" '10
DRAW "U45" '40
PAINT STEP(1, 1), &HFF999999, &HFF999999 '1, 1
DRAW "B M155,150" '155, 125
DRAW "C" + STR$(&HFF990000)
CIRCLE STEP(0, 0), 20, &HFF990000 '20 was radius; might have to readjust this
DRAW "B M135,145" '135, 121
DRAW "U23" '20
DRAW "F10" '10
'DRAW "B E5" '5
'DRAW "B L5" '5
'DRAW "B D4" '4
DRAW "E10" '10
DRAW "F10" '10
DRAW "E10" '10
DRAW "D23" '21
PAINT STEP(-3, -0), &HFF990000, &HFF990000 '-3, 0 <-- Bottom circle
PAINT STEP(-2, -13), &HFF990000, &HFF990000 '-2, -13 <-- Right spike
PAINT STEP(-8, 0), &HFF990000, &HFF990000 '-8, 0 <-- Middle spike (lower part)
PAINT STEP(-25, 2), &HFF990000, &HFF990000 '-25, 2 <-- Left spike
PAINT STEP(18, -6), &HFF990000, &HFF990000 '18, -6 <-- Middle spike (top part)
PSET STEP(-8, 3), &HFF990000 'Fill in the two holes
PSET STEP(16, 0), &HFF990000 'in the middle spike
_SNDPLAY SEF(0)
FadeIn
CheckThat! = TIMER
DO
    WhatClock! = TIMER
    IF _KEYDOWN(13) THEN EXIT DO
LOOP UNTIL (WhatClock! - CheckThat!) = 5!
isit = _SNDPLAYING(SEF(0))
IF isit THEN _SNDSTOP SEF(0)
'_SNDCLOSE fwlogo&
FadeOut
CLS

END SUB

SUB HardwareCheck

DRAW "C" + STR$(&HFF009900)
DRAW "B M0,55"
DRAW "U55"
DRAW "R319"
DRAW "D55"
DRAW "L319"
PAINT STEP(3, -3), &HFF00FF00, &HFF009900
DRAW "B U5"
DRAW "B R92"
DRAW "C" + STR$(&HFF000000)
DRAW "B U40"
DRAW "B R12"
Font "CRICKET KAMODON"
DRAW "B L129"
DRAW "B D10"
DRAW "C" + STR$(&HFF009900)
Font "ENGINE MILESTONE BUILD 12"
DRAW "B L209"
DRAW "B D35"
DRAW "C" + STR$(&HFF000000)
Font "COPYRIGHT 2011-2013 FLAMEWARE ZERONEXE"

'Now for the bottom half of the loading screen...
DRAW "B M0,55"
DRAW "C" + STR$(&HFF009900)
DRAW "D184"
DRAW "R319"
DRAW "U184"
DRAW "B D174"
DRAW "L319"
DRAW "B D7"
DRAW "B R79"
DRAW "C" + STR$(&HFFFFFFFF)
Font "PRESS ANY KEY TO CONTINUE"
'DRAW "B M19,90"
DRAW "C" + STR$(&HFF009900)
CenterFont "THIS PROVES I'M NOT TRISKAIDEKAPHOBIC, BECAUSE", 90
'DRAW "B M18,100"
CenterFont "UNLIKE MILTON BRADLEY, I'M NOT SUPERSTITIOUS!", 100
DRAW "B M4,110"
'Font "TO SEE IS WHAT I WOULD CALL MY ": DRAW "C" + STR$(&HFF00FF00): Font "DANGEROUS DAVE IN"
DRAW "B M21,120"
'Font "COPYRIGHT INFRINGEMENT": DRAW "C" + STR$(&HFF009900): Font ", IN A SENSE. THE ONLY"
DRAW "B M30,130"
'Font "DIFFERENCES WOULD BE CRICKET IN PLACE OF"
DRAW "B M8,140"
'Font "DANGEROUS DAVE, AND A CLONE OF WORLD 1, LEVELS 1"
DRAW "B M13,150"
'Font "THROUGH 3 FROM SUPER MARIO BROS. 2, INSTEAD OF"
DRAW "B M7,160"
'Font "WORLD 1, LEVEL 1 FROM SUPER MARIO BROS. 3. I ALSO"
DRAW "B M2,170"
'Font "WANT TO POINT OUT THAT THE NEXT DEMO, AND THE FULL"
DRAW "B M21,180"
'Font "GAME, WHEN I FINALLY FINISH IT, MIGHT HAVE AT"
DRAW "B M5,190"
'Font "LEAST THE FIRST LEVEL FROM THIS DEMO, BUT IT WILL"
DRAW "B M18,200"
'Font "DEFINITELY HAVE A WHOLE BUNCH OF NEW LEVELS."

DO
    dumvar$ = INKEY$
LOOP WHILE dumvar$ = ""
FadeOut
EXIT SUB 'I'm skipping the rest, because it's not needed for Windows, Mac OS X, Linux, or any OS that isn't DOS.

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
'DO
'    IF MULTIKEY(28) THEN EXIT DO
'LOOP WHILE NOT MULTIKEY(28)
PAINT (25, 195), 0, 2
DRAW "B M95,196"
DRAW "C15"
Font "STARTING THE GAME"
'FadePal 0, Pal&()

END SUB

SUB CenterFont (Sentence$, StartRow%)
'Figures out how to position the text, then sends the text to the Font SUB.
LeftPos% = 1
LtrCount% = 0
FullPhrase$ = Sentence$
DO UNTIL FullPhrase$ = BuildPhrase$
    Dummy$ = LEFT$(Sentence$, LeftPos%)
    Letter$ = RIGHT$(Dummy$, 1)
    LtrCount% = LtrCount% + 7 'If it's none of the following, it just adds 7.
    IF Letter$ = "I" THEN LtrCount% = LtrCount% - 1
    IF Letter$ = "K" THEN LtrCount% = LtrCount% - 2
    IF Letter$ = "M" THEN LtrCount% = LtrCount% + 1
    IF Letter$ = "T" THEN LtrCount% = LtrCount% - 1
    IF Letter$ = "V" THEN LtrCount% = LtrCount% - 1
    IF Letter$ = "W" THEN LtrCount% = LtrCount% + 1
    IF Letter$ = "Y" THEN LtrCount% = LtrCount% - 1
    IF Letter$ = "1" THEN LtrCount% = LtrCount% - 1
    IF Letter$ = "'" THEN LtrCount% = LtrCount% - 4
    IF Letter$ = "," THEN LtrCount% = LtrCount% - 4
    IF Letter$ = "." THEN LtrCount% = LtrCount% - 3
    IF Letter$ = "-" THEN LtrCount% = LtrCount% - 1
    IF Letter$ = ":" THEN LtrCount% = LtrCount% - 3
    IF Letter$ = " " THEN LtrCount% = LtrCount% - 2
    BuildPhrase$ = (BuildPhrase$ + Letter$)
    LeftPos% = LeftPos% + 1
LOOP
StartCol% = LtrCount% / 2
rmc = LtrCount% MOD 2
IF rmc > 0 THEN StartCol% = StartCol% + 1
DRAW "B M" + STR$(161 - StartCol%) + "," + STR$(StartRow%) 'Center in 320x240
Font BuildPhrase$ 'Then it passes it on to the Font SUB.
END SUB

SUB Font (Sentence$)
LeftPos% = 1
FullPhrase$ = Sentence$
DO UNTIL FullPhrase$ = BuildPhrase$
    Dummy$ = LEFT$(Sentence$, LeftPos%)
    Letter$ = RIGHT$(Dummy$, 1)
    GOSUB DrawLetter
    BuildPhrase$ = (BuildPhrase$ + Letter$)
    LeftPos% = LeftPos% + 1
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

    CASE ":"
        DRAW "R1"
        DRAW "B L1"
        DRAW "B U1"
        DRAW "R1"
        DRAW "B L1"
        DRAW "B U2"
        DRAW "R1"
        DRAW "B L1"
        DRAW "B U1"
        DRAW "R1"
        DRAW "B R2"
        DRAW "B D4"

    CASE " "
        DRAW "B R5"

END SELECT
RETURN

GetOuttaHere:
END SUB

SUB TileFont (Sentence$, StartX%, StartY%)
LeftPos% = 1
FullPhrase$ = Sentence$
DO UNTIL FullPhrase$ = BuildPhrase$
    Dummy$ = LEFT$(Sentence$, LeftPos%)
    Letter$ = RIGHT$(Dummy$, 1)
    charnum = ASC(Letter$)
    IF charnum = 42 THEN cc = 36 'The asterisk becomes the star tile
    IF charnum > 47 AND charnum < 58 THEN cc = charnum - 48 '0-9
    IF charnum > 64 AND charnum < 91 THEN cc = charnum - 55 'A-Z
    IF charnum = 32 THEN ELSE _PUTIMAGE ((StartX% + ((LeftPos% - 1) * 8)), StartY%), FontTile(cc)
    BuildPhrase$ = (BuildPhrase$ + Letter$)
    LeftPos% = LeftPos% + 1
LOOP
END SUB

SUB FadeIn ()
IF picture& THEN _FREEIMAGE plcture& 'Clear it, if it hasn't already been done
picture& = _COPYIMAGE(0) 'Question is, can I do this if the screen is blank?
FOR b = 255 TO 0 STEP -1
    _LIMIT 290 'Control the speed (good for fast PCs; how about slower ones?)
    _PUTIMAGE (0, 0), picture& 'Put the old image back up
    LINE (0, 0)-(319, 239), _RGBA32(0, 0, 0, b), BF 'Slowly fade in from black
    _DISPLAY 'Draw the screen again, each turn
NEXT b
_AUTODISPLAY 'Make the screen automatically start updating itself again
END SUB

SUB FadeOut ()
_DISPLAY 'Stop the screen from automatically updating every millisecond
picture& = _COPYIMAGE(0) 'Get a picture of what's on the screen, now
FOR a = 0 TO 255
    _LIMIT 290 'Control the speed (good for fast PCs; how about slower ones?)
    _PUTIMAGE (0, 0), picture& 'Put the old image back up
    LINE (0, 0)-(319, 239), _RGBA32(0, 0, 0, a), BF 'Slowly fade to black
    _DISPLAY 'Draw the screen again, each turn
NEXT a
END SUB

FUNCTION ButtonDef$ (btncode AS LONG)
SELECT CASE btncode
    CASE 18432: ButtonDef$ = "UP ARROW"
    CASE 20480: ButtonDef$ = "DOWN ARROW"
    CASE 19200: ButtonDef$ = "LEFT ARROW"
    CASE 19712: ButtonDef$ = "RIGHT ARROW"
    CASE 100306: ButtonDef$ = "LEFT CONTROL"
    CASE 100305: ButtonDef$ = "RIGHT CONTROL"
    CASE 100311: ButtonDef$ = "LEFT SUPER" ' Or the "Windows logo" key, as the Windows build calls it.
    CASE 100312: ButtonDef$ = "RIGHT SUPER" 'When you press this key on Piccolo, it doesn't register as this code.
    CASE 100308: ButtonDef$ = "LEFT ALT"
    CASE 100307: ButtonDef$ = "RIGHT ALT"
    CASE 100319: ButtonDef$ = "CONTEXT MENU"
    CASE 32: ButtonDef$ = "SPACEBAR"
    CASE 100304: ButtonDef$ = "LEFT SHIFT"
    CASE 100303: ButtonDef$ = "RIGHT SHIFT"
    CASE 100301: ButtonDef$ = "CAPS LOCK"
    CASE 20992: ButtonDef$ = "INSERT"
    CASE 21248: ButtonDef$ = "DELETE"
    CASE 18176: ButtonDef$ = "HOME"
    CASE 20224: ButtonDef$ = "END"
    CASE 18688: ButtonDef$ = "PAGE UP"
    CASE 20736: ButtonDef$ = "PAGE DOWN"
    CASE 100316: ButtonDef$ = "PRINT SCREEN"
    CASE 100302: ButtonDef$ = "SCROLL LOCK"
    CASE 100019: ButtonDef$ = "PAUSE"
    CASE 100300: ButtonDef$ = "NUM LOCK"
    CASE 113: ButtonDef$ = "Q"
    CASE 119: ButtonDef$ = "W"
    CASE 101: ButtonDef$ = "E"
    CASE 114: ButtonDef$ = "R"
    CASE 116: ButtonDef$ = "T"
    CASE 121: ButtonDef$ = "Y"
    CASE 117: ButtonDef$ = "U"
    CASE 105: ButtonDef$ = "I"
    CASE 111: ButtonDef$ = "O"
    CASE 112: ButtonDef$ = "P"
    CASE 97: ButtonDef$ = "A"
    CASE 115: ButtonDef$ = "S"
    CASE 100: ButtonDef$ = "D"
    CASE 102: ButtonDef$ = "F"
    CASE 103: ButtonDef$ = "G"
    CASE 104: ButtonDef$ = "H"
    CASE 106: ButtonDef$ = "J"
    CASE 107: ButtonDef$ = "K"
    CASE 108: ButtonDef$ = "L"
    CASE 122: ButtonDef$ = "Z"
    CASE 120: ButtonDef$ = "X"
    CASE 99: ButtonDef$ = "C"
    CASE 118: ButtonDef$ = "V"
    CASE 98: ButtonDef$ = "B"
    CASE 110: ButtonDef$ = "N"
    CASE 109: ButtonDef$ = "M"
    CASE 27: ButtonDef$ = "ESCAPE"
    CASE 96: ButtonDef$ = "BACKTICK"
    CASE 9: ButtonDef$ = "TAB"
    CASE 8: ButtonDef$ = "BACKSPACE"
    CASE 45: ButtonDef$ = "HYPHEN"
    CASE 61: ButtonDef$ = "EQUAL SIGN"
    CASE 91: ButtonDef$ = "LEFT BRACKET"
    CASE 93: ButtonDef$ = "RIGHT BRACKET"
    CASE 92: ButtonDef$ = "BACKSLASH"
    CASE 59: ButtonDef$ = "SEMICOLON"
    CASE 39: ButtonDef$ = "APOSTROPHE"
    CASE 44: ButtonDef$ = "COMMA"
    CASE 46: ButtonDef$ = "PERIOD"
    CASE 47: ButtonDef$ = "FORWARD SLASH"
    CASE 13: ButtonDef$ = "ENTER"
    CASE 42: ButtonDef$ = "ASTERISK"
    CASE 43: ButtonDef$ = "PLUS SIGN"
    CASE 48: ButtonDef$ = "0"
    CASE 49: ButtonDef$ = "1"
    CASE 50: ButtonDef$ = "2"
    CASE 51: ButtonDef$ = "3"
    CASE 52: ButtonDef$ = "4"
    CASE 53: ButtonDef$ = "5"
    CASE 54: ButtonDef$ = "6"
    CASE 55: ButtonDef$ = "7"
    CASE 56: ButtonDef$ = "8"
    CASE 57: ButtonDef$ = "9"
    CASE 15104: ButtonDef$ = "F1"
    CASE 15360: ButtonDef$ = "F2"
    CASE 15616: ButtonDef$ = "F3"
    CASE 15872: ButtonDef$ = "F4"
    CASE 16128: ButtonDef$ = "F5"
    CASE 16384: ButtonDef$ = "F6"
    CASE 16640: ButtonDef$ = "F7"
    CASE 16896: ButtonDef$ = "F8"
    CASE 17152: ButtonDef$ = "F9"
    CASE 17408: ButtonDef$ = "F10"
    CASE 34048: ButtonDef$ = "F11"
    CASE 34304: ButtonDef$ = "F12"
    CASE 0: ButtonDef$ = "THIS KEY IS NOT SET"
    CASE ELSE: ButtonDef$ = "SOME KEY I'VE NEVER HEARD OF"
END SELECT
END FUNCTION

FUNCTION GamepadDef$ (action AS INTEGER)
SELECT CASE action
    CASE 0 'Direction "up"
        IF JoyMoveOp(0) = 0 THEN 'Is the controller number (Action*) an even number (divisible by 2)?
            IF ActionUp MOD 2 = 0 THEN gb = (MoveUp - 3) / 4 ELSE gb = (MoveUp - 1) / 4
            IF gb < 0 THEN GamepadDef$ = "THIS ACTION IS NOT SET" ELSE GamepadDef$ = "BUTTON" + STR$(gb + 1)
        ELSEIF JoyMoveOp(0) > 0 THEN
            at$ = "THUMBSTICK" + STR$(JoyMoveOp(0))
            IF ActionUp MOD 2 = 0 THEN
                IF MoveUp <= 5 THEN at$ = at$ + " LEFT"
                IF MoveUp >= 250 THEN at$ = at$ + " RIGHT"
            ELSEIF ActionUp MOD 2 = 1 THEN
                IF MoveUp <= 5 THEN at$ = at$ + " UP"
                IF MoveUp >= 250 THEN at$ = at$ + " DOWN"
            END IF
            IF at$ = "" THEN at$ = "STICK" + STR$(JoyMoveOp(0)) + " COORD" + STR$(MoveUp) + " AXIS" + STR$(ActionUp)
            GamepadDef$ = at$
        ELSE: IF at$ = "" THEN at$ = "STICK" + STR$(JoyMoveOp(0)) + " COORD" + STR$(MoveUp) + " AXIS" + STR$(ActionUp)
        END IF
    CASE 1 'Direction "down"
        IF JoyMoveOp(1) = 0 THEN
            IF ActionDown MOD 2 = 0 THEN gb = (MoveDown - 3) / 4 ELSE gb = (MoveDown - 1) / 4
            IF gb < 0 THEN GamepadDef$ = "THIS ACTION IS NOT SET" ELSE GamepadDef$ = "BUTTON" + STR$(gb + 1)
        ELSEIF JoyMoveOp(1) > 0 THEN
            at$ = "THUMBSTICK" + STR$(JoyMoveOp(1))
            IF ActionDown MOD 2 = 0 THEN
                IF MoveDown <= 5 THEN at$ = at$ + " LEFT"
                IF MoveDown >= 250 THEN at$ = at$ + " RIGHT"
            ELSEIF ActionDown MOD 2 = 1 THEN
                IF MoveDown <= 5 THEN at$ = at$ + " UP"
                IF MoveDown >= 250 THEN at$ = at$ + " DOWN"
            END IF
            GamepadDef$ = at$
        END IF
    CASE 2 'Direction "left"
        IF JoyMoveOp(2) = 0 THEN
            IF ActionLeft MOD 2 = 0 THEN gb = (MoveLeft - 3) / 4 ELSE gb = (MoveLeft - 1) / 4
            IF gb < 0 THEN GamepadDef$ = "THIS ACTION IS NOT SET" ELSE GamepadDef$ = "BUTTON" + STR$(gb + 1)
        ELSEIF JoyMoveOp(2) > 0 THEN
            at$ = "THUMBSTICK" + STR$(JoyMoveOp(2))
            IF ActionLeft MOD 2 = 0 THEN
                IF MoveLeft <= 5 THEN at$ = at$ + " LEFT"
                IF MoveLeft >= 250 THEN at$ = at$ + " RIGHT"
            ELSEIF ActionLeft MOD 2 = 1 THEN
                IF MoveLeft <= 5 THEN at$ = at$ + " UP"
                IF MoveLeft >= 250 THEN at$ = at$ + " DOWN"
            END IF
            GamepadDef$ = at$
        END IF
    CASE 3 'Direction "right"
        IF JoyMoveOp(3) = 0 THEN
            IF ActionRight MOD 2 = 0 THEN gb = (MoveRight - 3) / 4 ELSE gb = (MoveRight - 1) / 4
            IF gb < 0 THEN GamepadDef$ = "THIS ACTION IS NOT SET" ELSE GamepadDef$ = "BUTTON" + STR$(gb + 1)
        ELSEIF JoyMoveOp(3) > 0 THEN
            at$ = "THUMBSTICK" + STR$(JoyMoveOp(3))
            IF ActionRight MOD 2 = 0 THEN
                IF MoveRight <= 5 THEN at$ = at$ + " LEFT"
                IF MoveRight >= 250 THEN at$ = at$ + " RIGHT"
            ELSEIF ActionRight MOD 2 = 1 THEN
                IF MoveRight <= 5 THEN at$ = at$ + " UP"
                IF MoveRight >= 250 THEN at$ = at$ + " DOWN"
            END IF
            GamepadDef$ = at$
        END IF
    CASE 4 'Action "run"
        IF JoyMoveOp(4) = 0 THEN
            IF ActionRun MOD 2 = 0 THEN gb = (MoveRun - 3) / 4 ELSE gb = (MoveRun - 1) / 4
            IF gb < 0 THEN GamepadDef$ = "THIS ACTION IS NOT SET" ELSE GamepadDef$ = "BUTTON" + STR$(gb + 1)
        ELSEIF JoyMoveOp(4) > 0 THEN
            at$ = "THUMBSTICK" + STR$(JoyMoveOp(4))
            IF ActionRun MOD 2 = 0 THEN
                IF MoveRun <= 5 THEN at$ = at$ + " LEFT"
                IF MoveRun >= 250 THEN at$ = at$ + " RIGHT"
            ELSEIF ActionRun MOD 2 = 1 THEN
                IF MoveRun <= 5 THEN at$ = at$ + " UP"
                IF MoveRun >= 250 THEN at$ = at$ + " DOWN"
            END IF
            GamepadDef$ = at$
        END IF
    CASE 5 'Action "jump"
        IF JoyMoveOp(5) = 0 THEN
            IF ActionJump MOD 2 = 0 THEN gb = (MoveJump - 3) / 4 ELSE gb = (MoveJump - 1) / 4
            IF gb < 0 THEN GamepadDef$ = "THIS ACTION IS NOT SET" ELSE GamepadDef$ = "BUTTON" + STR$(gb + 1)
        ELSEIF JoyMoveOp(5) > 0 THEN
            at$ = "THUMBSTICK" + STR$(JoyMoveOp(5))
            IF ActionJump MOD 2 = 0 THEN
                IF MoveJump <= 5 THEN at$ = at$ + " LEFT"
                IF MoveJump >= 250 THEN at$ = at$ + " RIGHT"
            ELSEIF ActionJump MOD 2 = 1 THEN
                IF MoveJump <= 5 THEN at$ = at$ + " UP"
                IF MoveJump >= 250 THEN at$ = at$ + " DOWN"
            END IF
            GamepadDef$ = at$
        END IF
    CASE 6 'Action "attack"
        IF JoyMoveOp(6) = 0 THEN
            IF ActionAttack MOD 2 = 0 THEN gb = (MoveAttack - 3) / 4 ELSE gb = (MoveAttack - 1) / 4
            IF gb < 0 THEN GamepadDef$ = "THIS ACTION IS NOT SET" ELSE GamepadDef$ = "BUTTON" + STR$(gb + 1)
        ELSEIF JoyMoveOp(6) > 0 THEN
            at$ = "THUMBSTICK" + STR$(JoyMoveOp(6))
            IF ActionAttack MOD 2 = 0 THEN
                IF MoveAttack <= 5 THEN at$ = at$ + " LEFT"
                IF MoveAttack >= 250 THEN at$ = at$ + " RIGHT"
            ELSEIF ActionAttack MOD 2 = 1 THEN
                IF MoveAttack <= 5 THEN at$ = at$ + " UP"
                IF MoveAttack >= 250 THEN at$ = at$ + " DOWN"
            END IF
            GamepadDef$ = at$
        END IF
    CASE 7 'Action "pause"
        IF JoyMoveOp(7) = 0 THEN
            IF ActionPause MOD 2 = 0 THEN gb = (MovePause - 3) / 4 ELSE gb = (MovePause - 1) / 4
            IF gb < 0 THEN GamepadDef$ = "THIS ACTION IS NOT SET" ELSE GamepadDef$ = "BUTTON" + STR$(gb + 1)
        ELSEIF JoyMoveOp(7) > 0 THEN
            at$ = "THUMBSTICK" + STR$(JoyMoveOp(7))
            IF ActionPause MOD 2 = 0 THEN
                IF MovePause <= 5 THEN at$ = at$ + " LEFT"
                IF MovePause >= 250 THEN at$ = at$ + " RIGHT"
            ELSEIF ActionPause MOD 2 = 1 THEN
                IF MovePause <= 5 THEN at$ = at$ + " UP"
                IF MovePause >= 250 THEN at$ = at$ + " DOWN"
            END IF
            GamepadDef$ = at$
        END IF
END SELECT
EXIT FUNCTION

GetButton:
FOR gb = 0 TO 19
    btn% = STRIG((gb * 4) + 1, cont%)
    IF btn% THEN GamepadDef$ = "BUTTON" + STR$(gb + 1)
NEXT gb
RETURN

END FUNCTION
