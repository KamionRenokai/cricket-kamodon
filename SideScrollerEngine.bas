'Let's try to figure out why exactly the sprite strips glitch, but the full-
'sized sprites display just fine. I've tried changing the array types from
'DOUBLE to LONG, CURRENCY and just removing a type, but if I set them to
'something other than DOUBLE or CURRENCY, you just get a whole bunch of gray
'lines, instead of sprite strips. It's kinda strange how the 8x8 sprites
'display just fine, but the 1x8 strips glitch, near the bottom, and yet if
'you display them after you capture them, they look fine, but exit the FOR
'loop where they're captured, and they mess up. I wonder what I'm doing wrong?

'Now I wanna find out how many times to loop through the background resource
'files, if there's more than 340 sprites, per 22 rows. It should be as simple
'as a math equation, like how many times 340 goes into the total number of
'sprites per row. If it's exactly, or less than, 340, it should just run
'through the loop once. If it's anywhere between 341 and 680, it should run
'through the loop twice, and so on and so forth. Take a number like 478, for
'example. It doesn't go into 340 evenly, but it does have a remainder. With
'that in mind, the engine should run through the loop twice: once to get the
'first 340 sprites, then again to get the remaining 138 sprites, and stick
'them on the ends of the first 22 rows. So what would the math equation be,
'to make the engine know how many times it should run through the setup loop?
'WAIT! Divide total number by 340, use that number (without the remainder) as
'a factor multiplied by 340, and subtract that from the total, and if there's
'any number there, add one onto the factor that was multiplied by 340, and
'make that the total number of loops? That might be the answer!

DECLARE SUB Font (Sentence$)                    ' My custom font I use a lot

DECLARE SUB LoadPal ()                                  'From DarkDread's RPG
DECLARE SUB FadePal (Direction%, PaletteArray&())       '"Mysterious Song"

DECLARE SUB KEYTEST (LOWERLIMIT, UPPERLIMIT)    ' "Multikey Function Update"
DECLARE FUNCTION MULTIKEY (KEYNUM)              ' by Joe Huber, Jr./Eric Carr

DIM SHARED Red%(255), Green%(255), Blue%(255) ' Required for fade-in/out
DIM SHARED Pal&(0 TO 255) ' Required for DD's palette

'First, set the screen to mode 13, then see how many sprites we have.
'SCREEN 13 ' 320x200x256 (VGA)
SCREEN _NEWIMAGE(640, 480, 256), 0, 0 ' 640x480x256 (SVGA) maybe?
DRAW "B M0, 5": Font "READING ALL SPRITES - SANITY CHECK..."
OPEN "/media/PHANTOM/QBX/CRICKET/WLDXL1S0.KMD" FOR INPUT AS #1 ' Open sprite frame file 1/16
DO UNTIL EOF(1) ' Stop at the end of the file
    FOR n = 0 TO 63 ' 64 3-digit numbers plus END
        INPUT #1, drawsomething$ ' Grab a set from the file
        ' PRINT testpixel(n) + " ";             ' Prove that we got something
    NEXT n ' Keep looping this 64 times
    INPUT #1, end$ ' This should say "END"
    'TODO: Catch and throw an error, should end$ not equal "END"?
    loopz = loopz + 1 ' Add 1 to sprite number total
LOOP ' Do this over and over again
CLOSE 1 ' We hit the end, so CLOSE it

'Second, we reopen the file to actually load the sprite data into memory.
DIM SHARED spritedata(0 TO (loopz - 1), 0 TO 15, 0 TO 63) AS INTEGER
'The first one is the total number of sprites. The second is the total number
'of frames each sprite can have (16), and the third is each individual pixel.
DRAW "B M0, 12": Font "LOADING ALL SPRITES..."
OPEN "/media/PHANTOM/QBX/CRICKET/WLDXL1S0.KMD" FOR INPUT AS #1 ' Reopen the file from before
DO UNTIL EOF(1) ' Read the whole entire file
    FOR s = 0 TO (loopz - 1) ' Grab every single sprite
        FOR p = 0 TO 63 ' Every pixel of every sprite
            INPUT #1, flip$ ' Grab a three-digit number
            spritedata(s, 0, p) = VAL(flip$) ' Make into integer to store
        NEXT p ' Keep the second set looping
        INPUT #1, flip$ ' This should just be "END"
    NEXT s ' Repeat the first loop, too
LOOP ' Going, and going, and going
'Should I move the sprite drawing and saving code up here, and have it
'convert the sprites as it's reading them, rather than saving them into a
'different array, and converting them, later?
CLOSE #1 ' Close it again, we're done

'TODO: Duplicate the above routine for each individual sprite frame file
'(from WLDXL1S0.KMD to WLDXL1SF.KMD), to add all sixteen frames into the
'spritedata integer. Of course, will all of this fit into memory?

'PRINT ""
'PRINT "----------------------------------------"

DRAW "B M0, 19": Font "READING BG LAYER 1 - SANITY CHECK..."
OPEN "/media/PHANTOM/QBX/CRICKET/WLDXL1BG.KMD" FOR INPUT AS #2 ' First background layer file
'Theoretically divide the number by 22 (number of rows on the screen), to see
'exactly how many lines of number sets I'm gonna need to store in memory.
DO UNTIL EOF(2) ' Read through the whole file
    'There's no "END" at the end of each line this time, so we can just read
    'right on through to the end of the file.
    INPUT #2, num$ ' Grab a two-digit number
    'PRINT num$;                    ' Prove that we grabbed one (DEBUG)
    grab = grab + 1 ' Add one to the total
LOOP ' Keep going until the end
CLOSE #2 ' Close the file, for now

'PRINT ""
DRAW "B M0, 33": Font LTRIM$(STR$(grab)) + " SETS OF NUMBERS."
DRAW "B M0, 40": Font "DIVIDED BY 22 ROWS, THAT'S" + STR$(grab / 22) + "."
IF (grab / 22) < 340 THEN
    DRAW "B M0, 47": Font LTRIM$(STR$((grab / 22))) + " IS LOWER THAN 340."
ELSE
    DRAW "B M0, 47": Font LTRIM$(STR$((grab / 22))) + " IS HIGHER THAN 340."
END IF

'Before we load the primary background plane, we'll check the primary
'foreground plane. Within the sanity check routine, I should make it double-
'check the number of sprites on the primary background plane, and make sure
'they match. If they don't, it should stop, and throw an error message,
'because if there are more or less sprites on either plane than the other, the
'map will look wrong.
DRAW "B M0, 61": Font "READING FG LAYER 1 - SANITY CHECK..."
OPEN "/media/PHANTOM/QBX/CRICKET/WLDXL1FG.KMD" FOR INPUT AS #3 ' First foreground layer file
'Theoretically divide the number by 22 (number of rows on the screen), to see
'exactly how many lines of number sets I'm gonna need to store in memory.
DO UNTIL EOF(3) ' Read through the whole file
    'There's no "END" at the end of each line this time, so we can just read
    'right on through to the end of the file.
    INPUT #3, num$ ' Grab a two-digit number
    'PRINT num$;                                     ' Prove that we grabbed one
    hold = hold + 1 ' Add one to the total
LOOP ' Keep going until the end
CLOSE #3 ' Close the file, for now

'We'll do something similar to the message that comes up after the primary
'background plane sanity check routine. The only difference is, this one
'makes sure that the primary foreground plane has as many sprites as the
'primary background plane.
DRAW "B M0, 75": Font LTRIM$(STR$(hold)) + " SETS OF NUMBERS."
DRAW "B M0, 82": Font "DIVIDED BY 22 ROWS, THAT'S" + STR$(hold / 22) + "."
IF grab = hold THEN
    DRAW "B M0, 89": Font "BG1 AND FG1 LAYERS MATCH. GOOD!"
ELSE
    DRAW "B M0, 89": Font "BG1 AND FG1 LAYERS DON'T MATCH!"
    STOP
END IF

'Now that we know how many sprites are in each line, we re-open both files,
'and put each sprite in its correct line, even if the sprites that are
'supposed to be on a certain line are 22 lines below it.
DRAW "B M0, 103": Font "LOADING BG AND FG LAYERS..."
OPEN "/media/PHANTOM/QBX/CRICKET/WLDXL1BG.KMD" FOR INPUT AS #2 'Re-open background layer file
OPEN "/media/PHANTOM/QBX/CRICKET/WLDXL1FG.KMD" FOR INPUT AS #3 'Re-open foreground layer file
'Can we set up a variable that can hold all of the sprites, without running
'out of what little memory we have?
'So far, we can do two! But that was just with 340. How about 1,000 or more?
DIM SHARED Background1(0 TO 21, 0 TO ((grab / 22) - 1)) AS INTEGER
DIM SHARED Foreground1(0 TO 21, 0 TO ((grab / 22) - 1)) AS INTEGER
'First is row number (1 to 22), second is the sprite number on that row.
'Do we have enough memory to hold both sets of sprite planes (so far)?

'Since "grab" and "hold" should both equal the same, it won't matter which
'integer we use.
lc% = (grab / 22) / 340 'Divide sprite total by 340
rc% = (grab / 22) MOD 340 'Does it divide evenly?

'If rc% is any number HIGHER than zero (even if it's not a whole number), then
'the engine should run through one more loop, counting ONLY the sprites that
'were the remainder of <total number> / 340, after it goes through as many
'loops as the number of times 340 did go into <total number>.

IF lc% < 1 THEN 'Is the total less than 340?
    FOR r% = 0 TO 21 'Row number (1 - 22)
        FOR s% = 0 TO ((grab / 22) - 1) 'Total number of sprites
            INPUT #2, ns$ 'Grab a set from the BG plane
            Background1(r%, s%) = VAL(ns$) 'Store that number
            ns$ = "" 'Erase it first, just in case
            INPUT #3, ns$ 'Grab a set from the FG plane
            Foreground1(r%, s%) = VAL(ns$) 'Store that number
        NEXT s% 'Continue the sprite chain
    NEXT r% 'Continue the row chain

ELSEIF lc% >= 1 THEN 'Is there at least 340?
    'TODO: Change this so it could run through more than one loop, if needed.
    FOR r% = 0 TO 21 'Row number (1 - 22)
        FOR s% = 0 TO 339 'Sprite number (1 - 340)
            INPUT #2, ns$ 'Grab a set of numbers
            Background1(r%, s%) = VAL(ns$) 'Store that number
            ns$ = "" 'Erase that before we go on
            INPUT #3, ns$ 'Grab another set of numbers
            Foreground1(r%, s%) = VAL(ns$) 'Store that number
        NEXT s% 'Continue the sprite chain
    NEXT r% 'Continue the row chain

    'Now that we've looped as many times as there are multiples of 340, are
    'there any sprites that we forgot to get?

    IF rc% > 0 THEN 'Are there any sprites left?
        FOR r% = 0 TO 21 'Row number (1 - 22)
            FOR s% = 0 TO (((grab / 22) - (340 * lc%)) - 1) 'Remaining sprites
                INPUT #2, ns$ 'Grab a set of numbers
                Background1(r%, ((340 * lc%) + s%)) = VAL(ns$) 'Store it!
                ns$ = "" 'Erase it to prevent glitches
                INPUT #3, ns$ 'Grab another set of numbers
                Foreground1(r%, ((340 * lc%) + s%)) = VAL(ns$) 'Store it!
            NEXT s% 'Continue the sprite chain
        NEXT r% 'Continue the row chain
    END IF

ELSE PRINT "Error! Check the values of LC% or RC%": STOP
    'If one of those integers is wrong, the code will stop right here, and
    'that line will light up.

END IF 'End of all possible choices

CLOSE #2 'Test close, just for now
CLOSE #3 '^^

'Now, let's make sure it's storing the right numbers into that variable!

'PRINT ""
'PRINT "Now, did we remember all that?"
'PRINT ""

'FOR r% = 0 TO 21
'  FOR s% = 0 TO 477
'    PRINT STR$(Background1(r%, s%));
'  NEXT s%
'NEXT r%

'There seems to be a bit of a glitch, though... near the end, in one of the
'lines (in row 22, I think), one of the numbers appears as a zero, when it
'should be a one. Of course, there's also a randomly-placed 49 that probably
'belongs in one of the previous rows, too...

DIM SHARED Segment(0 TO (loopz - 1), 0 TO 15, 0 TO 16) AS DOUBLE
DIM SHARED FullSprite(0 TO 5000) AS DOUBLE
'If I keep this as the 1x8 sprite strip array, the first number is the total
'number of sprites, the second is the frame number (1-16), and the third
'number is the actual sprite strip data, in multiples of two. So, for example,
'the first eighth is 0, the second eighth is 2, the third is 4, and so on.

CLS
DRAW "B M0, 5": Font "PREPARING ALL SPRITES..."
'DRAW "B M10, 30": Font "SPRITE LOADED FROM RESOURCE FILE"
'DRAW "B M10, 46": Font "1X8 PIXEL STRIPS PIECED TOGETHER"
'DRAW "B M10, 62": Font "WHOLE SPRITE LOADED FROM ARRAY"
FOR u = 0 TO (loopz - 1)
    FOR X = 0 TO 7
        FOR y = 0 TO 7
            z = (y * 8) + X
            PSET (X, y + 24), spritedata(u, 0, z)
        NEXT y
    NEXT X
    'This will tell us if we can store all 50-some sprites into this array.
    GET (0, 24)-(7, 31), FullSprite(u * 30)
    LINE (0, 24)-(7, 31), 0, BF
NEXT u

CLS
FOR q = 0 TO (loopz - 1)
    PUT (0, 56), FullSprite(q * 30), PSET
    FOR t = 0 TO 7
        GET (t, 56)-(t, 63), Segment(q, 0, (t * 2))
    NEXT t
NEXT q

'Make it recall the sprite strips outside the FOR loop, and they glitch. So,
'I'll probably have to have the engine capture the full sprites first, then
'capture the sprite strips by using the captured full sprites. Hopefully that
'doesn't backfire, either...

'I can store them, but yet... unless I draw them again right after I store
'them, somehow they glitch. Am I pulling a GPF, and not knowing it?

'How fast can we draw all the sprites onto the screen, from memory this time?
CLS
LoadPal
GOSUB FadeInit

'FOR l = 0 TO (loopz - 1)
FOR m = 0 TO 21 'Number of screen rows
    FOR n = 0 TO 39 'Number of sprites per row
        'FOR o = 0 TO 7 'Eight 1x8 strips per sprite
        'PUT (n * 8, (m * 8 + 24)), FullSprite(l * 30), PSET
        PUT (n * 8, (m * 8 + 24)), FullSprite((Background1(m, n) * 30)), PSET
        IF Foreground1(m, n) = 12 THEN 'Tile 12 is a blank space.
        ELSE 'This should skip it, if it is 12.
            PUT (n * 8, (m * 8 + 24)), FullSprite((Foreground1(m, n) * 30)), PSET
        END IF
        'PUT (((n * 8) + o), ((m * 8) + 24)), Segment(0, 0, o * 2), PSET
        'And yet I can do this! So why can't I draw all the strips right?
        'NEXT o 'Move to the next 1x8 strip
    NEXT n 'Move to the next sprite
NEXT m 'Move to the next row
'NEXT l
SLEEP

'GET (0, 24)-(0, 31), Segment(0, 0, 0)
'GET (1, 24)-(1, 31), Segment(0, 2, 0)
'GET (2, 24)-(2, 31), Segment(0, 4, 0)
'GET (3, 24)-(3, 31), Segment(0, 6, 0)
'GET (4, 24)-(4, 31), Segment(0, 8, 0)
'GET (5, 24)-(5, 31), Segment(0, 10, 0)
'GET (6, 24)-(6, 31), Segment(0, 12, 0)
'GET (7, 24)-(7, 31), Segment(0, 14, 0)

'PUT (0, 40), Segment(u, 0, 0), PSET
'PUT (1, 40), Segment(u, 0, 2), PSET
'PUT (2, 40), Segment(u, 0, 4), PSET
'PUT (3, 40), Segment(u, 0, 6), PSET
'PUT (4, 40), Segment(u, 0, 8), PSET
'PUT (5, 40), Segment(u, 0, 10), PSET
'PUT (6, 40), Segment(u, 0, 12), PSET
'PUT (7, 40), Segment(u, 0, 14), PSET


END ' So the game doesn't run through subroutines

'"FadeInit", "FadeOut" and "FadeIn" were from "Example of QuickBasic Fade
'Effect" by Terry Cavanagh of Dark Legends Software.
FadeInit: ' (Terry Cavanagh) Set up fade-in/fade-out
FOR I% = 0 TO 255
    OUT &H3C7, I%
    Red%(I%) = INP(&H3C9)
    Green%(I%) = INP(&H3C9)
    Blue%(I%) = INP(&H3C9)
NEXT I%
RETURN

FadeIn: ' (Terry Cavanagh) Fade back in from black
FOR I% = 0 TO 63
    FOR X% = 0 TO 255
        OUT &H3C7, X%
        r% = INP(&H3C9) + 1: IF r% > Red%(X%) THEN r% = Red%(X%)
        g% = INP(&H3C9) + 1: IF g% > Green%(X%) THEN g% = Green%(X%)
        B% = INP(&H3C9) + 1: IF B% > Blue%(X%) THEN B% = Blue%(X%)
        OUT &H3C8, X%
        OUT &H3C9, r%
        OUT &H3C9, g%
        OUT &H3C9, B%
    NEXT X%
NEXT I%
RETURN

FadeOut: ' (Terry Cavanagh) Fade the screen to black
FOR I% = 0 TO 63
    FOR X% = 0 TO 255
        OUT &H3C7, X%
        r% = INP(&H3C9) - 1: IF r% < 0 THEN r% = 0
        g% = INP(&H3C9) - 1: IF g% < 0 THEN g% = 0
        B% = INP(&H3C9) - 1: IF B% < 0 THEN B% = 0
        OUT &H3C8, X%
        OUT &H3C9, r%
        OUT &H3C9, g%
        OUT &H3C9, B%
    NEXT X%
NEXT I%
RETURN

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

SUB LoadPal
'DarkDread's "Get Palette" subcommand from his game Mysterious Song, with the
'filename changed, but everything else is the same.

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


