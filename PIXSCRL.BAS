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
DECLARE SUB pal (colour%, r%, g%, B%)
DECLARE SUB clrkbuffer ()

SUB clrkbuffer
DEF SEG = &H40
POKE &H1A, PEEK(&H1C)
END SUB

SUB pal (colour%, r%, g%, B%)
OUT &H3C8, colour%
OUT &H3C9, r%
OUT &H3C9, g%
OUT &H3C9, B%
END SUB

