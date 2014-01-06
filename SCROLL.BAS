'                           Pixel-by-Pixel Scroller
'                          -=======================-
'                              by Tyler Spaulding
'Not tile-by-tile
'Not pixel-by-tile-by-pixel-by-tile
'An actual one-pixel-per-keypress scroller
'I've commented everything but the math, which is the hard part (I don't even
'know how some of it works), so you can see how everything basically works.
'Maybe later I'll comment the math so you can alter it for you own use more
'easily.
'You can do whatever you want with this program as long as you GIVE ME CREDIT!
'
'Special thanks to RATM for speeding it up with his INPUT stuff.
'
'e-mail:        insty@uslink.net
'URL:           www.uslink.net/~insty/qbasic.htm
'
'$DYNAMIC
DECLARE SUB loadpal (file$)
DECLARE SUB pal (colour%, r%, g%, B%)
DECLARE SUB Clip (tilenum%, x1%, y1%, x2%, y2%, xput%, yput%)
DECLARE SUB clrkbuffer ()
DEFINT A-Z

TYPE rgb
        r AS INTEGER
        g AS INTEGER
        B AS INTEGER
END TYPE

SCREEN 13
DIM SHARED colors(255) AS rgb                                                                                                                           'holds palette
DIM SHARED scroll(32001) AS INTEGER                                                                                                                     'holds screen when scrolling
DIM SHARED Cliptile(0) AS INTEGER                                                                                                                       'temporary storage for clipped tile
DIM SHARED rgbval AS STRING * 1                                                                                                                         'used for loading palette

'Loads Map Tiles=====>--------------------------------------------------------

OPEN "newgame.put" FOR BINARY AS 1
Ints = (LOF(1) - 7) / 2
CLOSE #1
DIM SHARED Tiles(51, Ints / 52 - 1) AS INTEGER
DEF SEG = VARSEG(Tiles(0, 0))
BLOAD "newgame.put", 0
DEF SEG

'Loads Palette============>---------------------------------------------------

loadpal "scroll.pal"

'Loads Map================>---------------------------------------------------

OPEN "scroll.map" FOR INPUT AS 1
INPUT #1, xs, ys                                                                                                                                        'x and y size
DIM SHARED Map(ys, xs) AS INTEGER                                                                                                                       'dimensions map
FOR y = 1 TO ys                                                                                                                                         'every y
        FOR x = 1 TO xs                                                                                                                                 'every x
                INPUT #1, Map(y, x)                                                                                                                     'read map file
        NEXT
NEXT

x = 10 * INT(xs / 2)                                                                                                                                    'starting x
y = 10 * INT(ys / 2)                                                                                                                                    'starting y
screenx1 = 8                                                                                                                                            'x screen size (from center, max. 14)
screeny1 = 8                                                                                                                                            'y screen size (from center, max. 8)
screenx2 = 2 * screenx1 + 1                                                                                                                             'total x screen size
screeny2 = 2 * screeny1 + 1                                                                                                                             'total y screen size

'Draws Initial Screen=====>---------------------------------------------------

LINE (9, 9)-(10 * screenx2 + 10, 10 * screeny2 + 10), 15, B                                                                                             'outside box
FOR x2 = 1 TO screenx2                                                                                                                                  'every x shown
        FOR y2 = 1 TO screeny2                                                                                                                          'every y shown
                PUT (10 * x2, 10 * y2), Tiles(0, Map(y / 10 - screeny1 + y2 - 1, x / 10 - screenx1 + x2 - 1)), PSET                                     'PUTs correct tile
        NEXT
NEXT


'Start of Scrolling=======>---------------------------------------------------

scrollstep = 1                                                                                                                                          'scrolling step (1, 2, 5, or 10); hardly effects speed

f = 0
DO

 DO                                                                                                                                              '\
  a = INP(&H60)                                                                                                                           ' >-waits for keypress
 LOOP WHILE a = 0                                                                                                                                '/
 IF a = 1 THEN END                                                                                                                               'ESC
 IF a = 72 AND y / 10 > screeny1 + 1 THEN                                                                                                        'up
  y = y - scrollstep                                                                                                                      'changes y
  y2 = INT(y / 10)                                                                                                                        'center y tile
  GET (10, 10)-(10 * screenx2 + 9, 10 * screeny2 + 9 - scrollstep), scroll
   PUT (10, 10 + scrollstep), scroll, PSET                                                                                                 'scroll screen
   IF x / 10 = INT(x / 10) THEN                                                                                                            'if even with tile
    FOR x2 = 1 TO screenx2                                                                                                          'every x shown on side
     x1 = x / 10 + x2 - screenx1 - 1                                                                                         'x tile
     tile% = Map(y2 - screeny1, x1)                                                                                          'correct tile
     Clip tile%, 0, y - 10 * y2, 9, y - 10 * y2 - 1 + scrollstep, 10 * x2, 10                                                'calls CLIP SUB
    NEXT
   ELSE                                                                                                                                    'not even
    x1 = INT(x / 10) - screenx1                                                                                                     'x tile
    x3 = x - 10 * INT(x / 10)                                                                                                       'x-part shown
    tile% = Map(y2 - screeny1, x1)                                                                                                  'top-left corner tile
    Clip tile%, x3, y - 10 * y2, 9, y - 10 * y2 - 1 + scrollstep, 10, 10                                                            'calls CLIP SUB
    FOR x2 = 2 TO screenx2                                                                                                          'every x shown NOT on corner
     x1 = INT(x / 10) + x2 - screenx1 - 1                                                                                    'x tile
     tile% = Map(y2 - screeny1, x1)                                                                                          'tile shown
     Clip tile%, 0, y - 10 * y2, 9, y - 10 * y2 - 1 + scrollstep, 10 * x2 - x3, 10                                           'calls CLIP SUB
    NEXT
    x1 = INT(x / 10) + screenx2 - screenx1                                                                                          'x tile
    x3 = x - 10 * INT(x / 10)                                                                                                       'x-part shown
    tile% = Map(y2 - screeny1, x1)                                                                                                  'top-right corner tile
    Clip tile%, 0, y - 10 * y2, x3 - 1, y - 10 * y2 - 1 + scrollstep, 10 * screenx2 + 10 - x3, 10                                   'calls CLIP SUB
  END IF
 END IF
 IF a = 80 AND y / 10 < ys - screeny1 THEN                                                                                                       'down
  y2 = INT(y / 10)                                                                                                                        'center y tile
  GET (10, 10 + scrollstep)-(10 * screenx2 + 9, 10 * screeny2 + 9), scroll
  PUT (10, 10), scroll, PSET                                                                                                              'above two:scrolls screen
  IF x / 10 = INT(x / 10) THEN                                                                                                            'if even with tile
   FOR x2 = 1 TO screenx2                                                                                                          'every tile on side
    x1 = x / 10 + x2 - screenx1 - 1                                                                                         'x tile
    tile% = Map(y2 + screeny1 + 1, x1)                                                                                      'tile shown
    Clip tile%, 0, y - 10 * y2, 9, y - 10 * y2 - 1 + scrollstep, 10 * x2, 10 * screeny2 + 10 - scrollstep                   'calls CLIP SUB
   NEXT
  ELSE                                                                                                                                    'not even
   x1 = INT(x / 10) - screenx1                                                                                                     'x tile
   x3 = x - 10 * INT(x / 10)                                                                                                       'x-part shown
   tile% = Map(y2 + screeny1 + 1, x1)                                                                                              'bottom-left tile
   Clip tile%, x3, y - 10 * y2, 9, y - 10 * y2 - 1 + scrollstep, 10, 10 * screeny2 + 10 - scrollstep                               'calls CLIP SUB
   FOR x2 = 2 TO screenx2                                                                                                          'every x shown NOT on corner
    x1 = INT(x / 10) + x2 - screenx1 - 1                                                                                    'x tile
    tile% = Map(y2 + screeny1 + 1, x1)                                                                                      'tile shown
    Clip tile%, 0, y - 10 * y2, 9, y - 10 * y2 - 1 + scrollstep, 10 * x2 - x3, 10 * screeny2 + 10 - scrollstep              'calls CLIP SUB
   NEXT
   x1 = INT(x / 10) + screenx2 - screenx1                                                                                          'x tile
   x3 = x - 10 * INT(x / 10)                                                                                                       'x-part shown
   tile% = Map(y2 + screeny1 + 1, x1)                                                                                              'bottom-right tile
   Clip tile%, 0, y - 10 * y2, x3 - 1, y - 10 * y2 - 1 + scrollstep, 10 * screenx2 + 10 - x3, 10 * screeny2 + 10 - scrollstep      'calls CLIP SUB
  END IF
  y = y + scrollstep                                                                                                                      'changes y
 END IF
 IF a = 75 AND x / 10 > screenx1 + 1 THEN                                                                                                        'left
  x = x - scrollstep                                                                                                                      'changes x
  x2 = INT(x / 10)                                                                                                                        'center x tile
  GET (10, 10)-(10 * screenx2 + 9 - scrollstep, 10 * screeny2 + 9), scroll
  PUT (10 + scrollstep, 10), scroll, PSET                                                                                                 'above two:scroll screen
  IF y / 10 = INT(y / 10) THEN                                                                                                            'if even with tile
   FOR y2 = 1 TO screeny2                                                                                                          'every y shown on side
    y1 = y / 10 + y2 - screeny1 - 1                                                                                         'y tile
    tile% = Map(y1, x2 - screenx1)                                                                                          'tile shown
    Clip tile%, x - 10 * x2, 0, x - 10 * x2 - 1 + scrollstep, 9, 10, 10 * y2                                                'calls CLIP SUB
   NEXT
  ELSE                                                                                                                                    'not even
  y1 = INT(y / 10) - screeny1                                                                                                     'y tile
  y3 = y - 10 * INT(y / 10)                                                                                                       'y-part shown
  tile% = Map(y1, x2 - screenx1)                                                                                                  'top-left corner
  Clip tile%, x - 10 * x2, y3, x - 10 * x2 - 1 + scrollstep, 9, 10, 10                                                            'calls CLIP SUB
  FOR y2 = 2 TO screeny2                                                                                                          'every y shown NOT on corner
   y1 = INT(y / 10) + y2 - screeny1 - 1                                                                                    'y tile
   tile% = Map(y1, x2 - screenx1)                                                                                          'tile shown
   Clip tile%, x - 10 * x2, 0, x - 10 * x2 - 1 + scrollstep, 9, 10, 10 * y2 - y3                                           'calls CLIP SUB
  NEXT
  y1 = INT(y / 10) + screeny2 - screeny1                                                                                          'y tile
  y3 = y - 10 * INT(y / 10)                                                                                                       'y-part shown
  tile% = Map(y1, x2 - screenx1)                                                                                                  'bottom-left corner
  Clip tile%, x - 10 * x2, 0, x - 10 * x2 - 1 + scrollstep, y3 - 1, 10, 10 * screeny2 + 10 - y3                                   'calls CLIP SUB
 END IF
END IF
IF a = 77 AND x / 10 < xs - screenx1 THEN                                                                                                       'right
 x2 = INT(x / 10)                                                                                                                        'center x tile
 GET (10 + scrollstep, 10)-(10 * screenx2 + 9, 10 * screeny2 + 9), scroll
 PUT (10, 10), scroll, PSET                                                                                                              'above two:scroll screen
  IF y / 10 = INT(y / 10) THEN                                                                                                            'if even with tile
   FOR y2 = 1 TO screeny2                                                                                                          'every y shown on side
    y1 = y / 10 + y2 - screeny1 - 1                                                                                         'y tile
    tile% = Map(y1, x2 + screenx1 + 1)                                                                                      'tile shown
    Clip tile%, x - 10 * x2, 0, x - 10 * x2 - 1 + scrollstep, 9, 10 * screenx2 + 10 - scrollstep, 10 * y2                   'calls CLIP SUB
   NEXT
  ELSE                                                                                                                                    'not even
  y1 = INT(y / 10) - screeny1                                                                                                     'y tile
  y3 = y - 10 * INT(y / 10)                                                                                                       'y-part shown
  tile% = Map(y1, x2 + screenx1 + 1)                                                                                              'top-right corner
  Clip tile%, x - 10 * x2, y3, x - 10 * x2 - 1 + scrollstep, 9, 10 * screenx2 + 10 - scrollstep, 10                               'calls CLIP SUB
  FOR y2 = 2 TO screeny2                                                                                                          'ever y NOT on corner
   y1 = INT(y / 10) + y2 - screeny1 - 1                                                                                    'y tile
   tile% = Map(y1, x2 + screenx1 + 1)                                                                                      'tile shown
   Clip tile%, x - 10 * x2, 0, x - 10 * x2 - 1 + scrollstep, 9, 10 * screenx2 + 10 - scrollstep, 10 * y2 - y3              'calls CLIP SUB
  NEXT
  y1 = INT(y / 10) + screeny2 - screeny1                                                                                          'y tile
  y3 = y - 10 * INT(y / 10)                                                                                                       'y-part shown
  tile% = Map(y1, x2 + screenx1 + 1)                                                                                              'bottom-right corner
  Clip tile%, x - 10 * x2, 0, x - 10 * x2 - 1 + scrollstep, y3 - 1, 10 * screenx2 + 10 - scrollstep, 10 * screeny2 + 10 - y3      'calls CLIP SUB
 END IF
 x = x + scrollstep                                                                                                                      'changes x
END IF
clrkbuffer

LOOP

REM $STATIC
'tilenum=       tile number
'x1=            first x shown
'y1=            first y shown
'x2=            last x shown
'y2=            last y shown
'xput=          PUT x coord.
'yput=          PUT y coord.
SUB Clip (tilenum%, x1%, y1%, x2%, y2%, xput%, yput%)


xsize = Tiles(0, tilenum%) / 8                  'x size of array
ysize = Tiles(1, tilenum%)                      'y size of array

FOR Clipy = y1% TO y2%                          'every y
        FOR Clipx = x1% TO x2%                  'every x
                clipspace! = 2 + (xsize * Clipy + Clipx) / 2    'space of tile
                cliparray = INT(clipspace!)                     'that's used
                IF clipspace! = cliparray THEN  'first pixel stored in space
                        colour = Tiles(cliparray, tilenum%) - 256 * INT(Tiles(cliparray, tilenum%) / 256)
                        'color used
                ELSE
                        colour = INT(Tiles(cliparray, tilenum%) / 256)
                        'color used
                END IF
                PSET (xput% + Clipx - x1, yput% + Clipy - y1), colour
        NEXT
NEXT

END SUB

SUB clrkbuffer
DEF SEG = &H40
POKE &H1A, PEEK(&H1C)
END SUB

SUB loadpal (file$)
OPEN file$ FOR BINARY AS 1
FOR colour% = 0 TO 255
        GET 1, , rgbval
        colors(colour%).r = ASC(rgbval)
        GET 1, , rgbval
        colors(colour%).g = ASC(rgbval)
        GET 1, , rgbval
        colors(colour%).B = ASC(rgbval)
        pal colour%, colors(colour%).r, colors(colour%).g, colors(colour%).B
  NEXT
CLOSE #1
END SUB

SUB pal (colour%, r%, g%, B%)
OUT &H3C8, colour%
OUT &H3C9, r%
OUT &H3C9, g%
OUT &H3C9, B%
END SUB

