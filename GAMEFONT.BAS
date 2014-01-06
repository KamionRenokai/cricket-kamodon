' Naruto: The Path to Hokage
' Program Font and Graphic Module (GAMEFONT.BAS)
'
DECLARE SUB GameFont (Letter$)
DECLARE SUB Sentence (Text$)
DECLARE SUB GameFontAscii (Symbol$)

DECLARE SUB DrawBox (BoxType%)

' TODO: Add a fifth type, which will be BGCOLOR 8, for grayed-out buttons.
SUB DrawBox (BoxType%)

SELECT CASE BoxType%

CASE 1  'The box on the bottom. The most common one this game will use.
LINE (0, 135)-(319, 199), , B
LINE (1, 136)-(318, 198), , B
LINE (2, 137)-(317, 197), , B
LINE (3, 138)-(316, 196), 0, B
LINE (4, 139)-(315, 195), , B
DRAW "B M5,140"
DRAW "C0"
FOR i% = 1 TO 27
DRAW "R309"
DRAW "B D1"
DRAW "L309"
DRAW "B D1"
NEXT i%
DRAW "R309"

CASE 2  'Just like box 1, except this one's on top. It's the help box.
LINE (0, 0)-(319, 59), , B
LINE (1, 1)-(318, 58), , B
LINE (2, 2)-(317, 57), , B
LINE (3, 3)-(316, 56), 0, B
LINE (4, 4)-(315, 55), , B
DRAW "B M5,5"
DRAW "C0"
FOR i% = 1 TO 25
DRAW "R309"
DRAW "B D1"
DRAW "L309"
DRAW "B D1"
NEXT i%

CASE 3  'One of the mini option boxes used by this game. This one's enabled.
LINE STEP(0, 0)-STEP(61, 16), , B
DRAW "B L61"
DRAW "B U16"
LINE STEP(1, 1)-STEP(59, 14), , B
DRAW "B L58"
DRAW "B U13"
DRAW "C6"
FOR i% = 1 TO 6
DRAW "R57"
DRAW "B D1"
DRAW "L57"
DRAW "B D1"
NEXT i%
DRAW "R57"

CASE 4  'Same as #3, except this one you can't select. It's disabled.
LINE STEP(0, 0)-STEP(61, 16), , B
DRAW "B L61"
DRAW "B U16"
LINE STEP(1, 1)-STEP(59, 14), , B
DRAW "B L58"
DRAW "B U13"
DRAW "C8"
FOR i% = 1 TO 6
DRAW "R57"
DRAW "B D1"
DRAW "L57"
DRAW "B D1"
NEXT i%
DRAW "R57"

END SELECT
END SUB

SUB GameFont (Letter$)

SELECT CASE Letter$
CASE "0" ' Number 0
DRAW "B R1"
DRAW "R2"
DRAW "B R1"
DRAW "B U1"
DRAW "U6"
DRAW "B L1"
DRAW "B U1"
DRAW "L2"
DRAW "B L1"
DRAW "B D1"
DRAW "D6"
DRAW "B R6"
DRAW "B D1"
EXIT SUB

CASE "1" ' Number 1
DRAW "B R3"
DRAW "U8"
DRAW "B D2"
DRAW "L2"
DRAW "B R2"
DRAW "B D6"
DRAW "B R5"
EXIT SUB

CASE "2" ' Number 2
DRAW "R5"
DRAW "B L5"
DRAW "E5"
DRAW "U2"
DRAW "B U1"
DRAW "B L1"
DRAW "L3"
DRAW "B L1"
DRAW "B D1"
DRAW "D1"
DRAW "B D6"
DRAW "B R7"
EXIT SUB

CASE "3" ' Number 3
DRAW "B U1"
DRAW "U1"
DRAW "B D2"
DRAW "B R1"
DRAW "R3"
DRAW "B R1"
DRAW "B U1"
DRAW "U2"
DRAW "B U1"
DRAW "B L1"
DRAW "L2"
DRAW "B R3"
DRAW "B U1"
DRAW "U2"
DRAW "B U1"
DRAW "B L1"
DRAW "L3"
DRAW "B L1"
DRAW "B D1"
DRAW "D1"
DRAW "B R8"
DRAW "B D6"
EXIT SUB

CASE "4" ' Number 4
DRAW "B R3"
DRAW "U8"
DRAW "G4"
DRAW "R5"
DRAW "B D4"
DRAW "B R3"
EXIT SUB

CASE "5" ' Number 5
DRAW "B U1"
DRAW "U1"
DRAW "B D2"
DRAW "B R1"
DRAW "R3"
DRAW "B U1"
DRAW "B R1"
DRAW "U4"
DRAW "L5"
DRAW "U3"
DRAW "R5"
DRAW "B R3"
DRAW "B D8"
EXIT SUB

CASE "6" ' Number 6
DRAW "B R1"
DRAW "R3"
DRAW "B R1"
DRAW "B U1"
DRAW "U2"
DRAW "B U1"
DRAW "B L1"
DRAW "L3"
DRAW "B L1"
DRAW "B D1"
DRAW "D2"
DRAW "B U3"
DRAW "E4"
DRAW "B R3"
DRAW "B D8"
EXIT SUB

CASE "7" ' Number 7
DRAW "B U8"
DRAW "R5"
DRAW "D2"
DRAW "G2"
DRAW "D4"
DRAW "B R4"
EXIT SUB

CASE "8" ' Number 8
DRAW "B R1"
DRAW "R3"
DRAW "B R1"
DRAW "B U1"
DRAW "U2"
DRAW "B U1"
DRAW "B L1"
DRAW "L3"
DRAW "B U1"
DRAW "B L1"
DRAW "U2"
DRAW "B U1"
DRAW "B R1"
DRAW "R3"
DRAW "B R1"
DRAW "B D1"
DRAW "D2"
DRAW "B D1"
DRAW "B L1"
DRAW "B L3"
DRAW "B L1"
DRAW "B D1"
DRAW "D2"
DRAW "B D1"
DRAW "B R7"
EXIT SUB

CASE "9" ' Number 9
DRAW "R2"
DRAW "E3"
DRAW "U4"
DRAW "B U1"
DRAW "B L1"
DRAW "L3"
DRAW "B L1"
DRAW "B D1"
DRAW "D3"
DRAW "B D1"
DRAW "B R1"
DRAW "R3"
DRAW "B R3"
DRAW "B D3"
EXIT SUB

CASE "A" ' Uppercase A
DRAW "U5"
DRAW "E2"
DRAW "F2"
DRAW "D5"
DRAW "B U2"
DRAW "L4"
DRAW "B R4"
DRAW "B D2"
DRAW "B R2"
EXIT SUB

CASE "B" ' Uppercase B
DRAW "B U1"
DRAW "U6"
DRAW "R3"
DRAW "B R1"
DRAW "B D1"
DRAW "D1"
DRAW "B D1"
DRAW "B L1"
DRAW "L3"
DRAW "B R3"
DRAW "B R1"
DRAW "D3"
DRAW "B L1"
DRAW "B D1"
DRAW "L3"
DRAW "U1"
DRAW "B D2"
DRAW "B R6"
DRAW "B U1"
EXIT SUB

CASE "C" ' Uppercase C
DRAW "B U1"
DRAW "U5"
DRAW "B U1"
DRAW "B R1"
DRAW "R3"
DRAW "B D7"
DRAW "L3"
DRAW "B R5"
EXIT SUB

CASE "D" ' Uppercase D
DRAW "U7"
DRAW "R3"
DRAW "B R1"
DRAW "B D1"
DRAW "D5"
DRAW "B D1"
DRAW "B L1"
DRAW "L3"
DRAW "B R6"
EXIT SUB

CASE "E" ' Uppercase E
DRAW "U7"
DRAW "R3"
DRAW "B L3"
DRAW "B D3"
DRAW "R2"
DRAW "B L2"
DRAW "B D4"
DRAW "R3"
DRAW "B R2"
EXIT SUB

CASE "F" ' Uppercase F
DRAW "U7"
DRAW "R3"
DRAW "B L3"
DRAW "B D3"
DRAW "R2"
DRAW "B L2"
DRAW "B D4"
DRAW "B R6"
EXIT SUB

CASE "G" ' Uppercase G
DRAW "B R1"
DRAW "R4"
DRAW "B L5"
DRAW "B U1"
DRAW "U5"
DRAW "B U1"
DRAW "B R1"
DRAW "R4"
DRAW "B D7"
DRAW "B R1"
DRAW "B U1"
DRAW "U2"
DRAW "B U1"
DRAW "B L1"
DRAW "L2"
DRAW "B R5"
DRAW "B D4"
EXIT SUB

CASE "H" ' Uppercase H
DRAW "U7"
DRAW "B D4"
DRAW "R4"
DRAW "B U4"
DRAW "D7"
DRAW "B R3"
EXIT SUB

CASE "I" ' Uppercase I
DRAW "R4"
DRAW "B L2"
DRAW "U7"
DRAW "B L2"
DRAW "R4"
DRAW "B R2"
DRAW "B D7"
EXIT SUB

CASE "J" ' Uppercase J
DRAW "B U1"
DRAW "U2"
DRAW "B D3"
DRAW "B R1"
DRAW "R3"
DRAW "B R1"
DRAW "B U1"
DRAW "U6"
DRAW "B D7"
DRAW "B R3"
EXIT SUB

CASE "K" ' Uppercase K
DRAW "U7"
DRAW "B D4"
DRAW "E4"
DRAW "B G4"
DRAW "B U1"
DRAW "F4"
DRAW "B R3"
EXIT SUB

CASE "L" ' Uppercase L
DRAW "U7"
DRAW "B D7"
DRAW "R4"
DRAW "B R2"
EXIT SUB

CASE "M" ' Uppercase M
DRAW "U7"
DRAW "F3"
DRAW "E3"
DRAW "D7"
DRAW "B R2"
EXIT SUB

CASE "N" ' Uppercase N
DRAW "U7"
DRAW "F7"
DRAW "U7"
DRAW "B D7"
DRAW "B R3"
EXIT SUB

CASE "O" ' Uppercase O
DRAW "B R1"
DRAW "R3"
DRAW "B R1"
DRAW "B U1"
DRAW "U5"
DRAW "B U1"
DRAW "B L1"
DRAW "L3"
DRAW "B L1"
DRAW "B D1"
DRAW "D5"
DRAW "B D1"
DRAW "B R7"
EXIT SUB

CASE "P" ' Uppercase P
DRAW "U7"
DRAW "R4"
DRAW "B R1"
DRAW "B D1"
DRAW "D2"
DRAW "B D1"
DRAW "B L1"
DRAW "L4"
DRAW "B D3"
DRAW "B R7"
EXIT SUB

CASE "Q" ' Uppercase Q
DRAW "B R1"
DRAW "R3"
DRAW "B R1"
DRAW "B U1"
DRAW "U5"
DRAW "B U1"
DRAW "B L1"
DRAW "L3"
DRAW "B L1"
DRAW "B D1"
DRAW "D5"
DRAW "B D2"
DRAW "B R6"
DRAW "H3"
DRAW "B R3"
DRAW "B D2"
DRAW "B R3"
EXIT SUB

CASE "R" ' Uppercase R
DRAW "U7"
DRAW "R3"
DRAW "B R1"
DRAW "B D1"
DRAW "D2"
DRAW "B D1"
DRAW "B L1"
DRAW "L3"
DRAW "B R1"
DRAW "F3"
DRAW "B R2"
EXIT SUB

CASE "S" ' Uppercase S
DRAW "B R1"
DRAW "R3"
DRAW "B R1"
DRAW "B U1"
DRAW "U2"
DRAW "B U1"
DRAW "B L1"
DRAW "L3"
DRAW "B U1"
DRAW "B L1"
DRAW "U1"
DRAW "B U1"
DRAW "B R1"
DRAW "R3"
DRAW "B D7"
DRAW "B R3"
EXIT SUB

CASE "T" ' Uppercase T
DRAW "B R2"
DRAW "U7"
DRAW "L3"
DRAW "R6"
DRAW "B R2"
DRAW "B D7"
EXIT SUB

CASE "U" ' Uppercase U
DRAW "B U1"
DRAW "U6"
DRAW "B D7"
DRAW "B R1"
DRAW "R4"
DRAW "B R1"
DRAW "B U1"
DRAW "U6"
DRAW "B D7"
DRAW "B R2"
EXIT SUB

CASE "V" ' Uppercase V
DRAW "B U3"
DRAW "U4"
DRAW "B D4"
DRAW "F3"
DRAW "E3"
DRAW "U4"
DRAW "B D7"
DRAW "B R3"
EXIT SUB

CASE "W" ' Uppercase W
DRAW "U7"
DRAW "B D7"
DRAW "E3"
DRAW "F3"
DRAW "U7"
DRAW "B D7"
DRAW "B R2"
EXIT SUB

CASE "X" ' Uppercase X
DRAW "U2"
DRAW "E3"
DRAW "U2"
DRAW "B D7"
DRAW "U2"
DRAW "H3"
DRAW "U2"
DRAW "B R5"
DRAW "B D7"
EXIT SUB

CASE "Y" ' Uppercase Y
DRAW "B R2"
DRAW "U4"
DRAW "H2"
DRAW "U1"
DRAW "B D1"
DRAW "B F2"
DRAW "E2"
DRAW "U1"
DRAW "B D7"
DRAW "B R2"
EXIT SUB

CASE "Z" ' Uppercase Z
DRAW "R5"
DRAW "B L5"
DRAW "U1"
DRAW "E5"
DRAW "U1"
DRAW "L5"
DRAW "B D7"
DRAW "B R7"
EXIT SUB

CASE "a" ' Lowercase a
DRAW "B R1"
DRAW "R3"
DRAW "U4"
DRAW "B U1"
DRAW "B L1"
DRAW "L2"
DRAW "B R3"
DRAW "B D2"
DRAW "L3"
DRAW "B L1"
DRAW "B D1"
DRAW "D1"
DRAW "B D1"
DRAW "B R6"
EXIT SUB

CASE "b" ' Lowercase b
DRAW "U7"
DRAW "B D3"
DRAW "R3"
DRAW "B R1"
DRAW "B D1"
DRAW "D2"
DRAW "B D1"
DRAW "B L1"
DRAW "L3"
DRAW "B R6"
EXIT SUB

CASE "c" ' Lowercase c
DRAW "B U1"
DRAW "U3"
DRAW "B U1"
DRAW "B R1"
DRAW "R2"
DRAW "B L2"
DRAW "B D1"
DRAW "B D3"
DRAW "B D1"
DRAW "R2"
DRAW "B R2"
EXIT SUB

CASE "d" ' Lowercase d
DRAW "B R1"
DRAW "R3"
DRAW "U7"
DRAW "B D3"
DRAW "L3"
DRAW "B L1"
DRAW "B D1"
DRAW "D2"
DRAW "B D1"
DRAW "B R6"
EXIT SUB

CASE "e" ' Lowercase e
DRAW "B R1"
DRAW "R3"
DRAW "B L3"
DRAW "B U1"
DRAW "B L1"
DRAW "U3"
DRAW "B U1"
DRAW "B R1"
DRAW "R3"
DRAW "B R1"
DRAW "B D1"
DRAW "D1"
DRAW "B D1"
DRAW "B L1"
DRAW "L3"
DRAW "B R6"
DRAW "B D2"
EXIT SUB

CASE "f" ' Lowercase f
DRAW "B R2"
DRAW "U6"
DRAW "B U1"
DRAW "B R1"
DRAW "R3"
DRAW "B L4"
DRAW "B D4"
DRAW "L2"
DRAW "R4"
DRAW "B R4"
DRAW "B D3"
EXIT SUB

CASE "g" ' Lowercase g
DRAW "B U2"
DRAW "B R1"
DRAW "R3"
DRAW "B R1"
DRAW "B U1"
DRAW "U1"
DRAW "B U1"
DRAW "B L1"
DRAW "L3"
DRAW "B L1"
DRAW "B D1"
DRAW "D1"
DRAW "B D1"
DRAW "B R5"
DRAW "D2"
DRAW "B D1"
DRAW "B L1"
DRAW "L3"
DRAW "B D2"
DRAW "B U3"
DRAW "B R6"
EXIT SUB

CASE "h" ' Lowercase h
DRAW "U7"
DRAW "B D3"
DRAW "R3"
DRAW "B R1"
DRAW "B D1"
DRAW "D3"
DRAW "B R2"
EXIT SUB

CASE "i" ' Lowercase i
DRAW "U4"
DRAW "B U2"
DRAW "U1"
DRAW "B D7"
DRAW "B R3"
EXIT SUB

CASE "j" ' Lowercase j
DRAW "B U1"
DRAW "U1"
DRAW "B D2"
DRAW "B R1"
DRAW "R2"
DRAW "B R1"
DRAW "B U1"
DRAW "U3"
DRAW "B U2"
DRAW "U1"
DRAW "B D7"
DRAW "B R2"
EXIT SUB

CASE "k" ' Lowercase k
DRAW "U7"
DRAW "B D5"
DRAW "E3"
DRAW "B G3"
DRAW "B U1"
DRAW "F3"
DRAW "B R2"
EXIT SUB

CASE "l" ' Lowercase l
DRAW "U7"
DRAW "B D7"
DRAW "B R3"
EXIT SUB

CASE "m" ' Lowercase m
DRAW "U4"
DRAW "B U1"
DRAW "B R1"
DRAW "R1"
DRAW "B D1"
DRAW "B R1"
DRAW "D4"
DRAW "B R1"
DRAW "B U5"
DRAW "R1"
DRAW "B R1"
DRAW "B D1"
DRAW "D4"
DRAW "B R2"
EXIT SUB

CASE "n" ' Lowercase n
DRAW "U5"
DRAW "R3"
DRAW "B R1"
DRAW "B D1"
DRAW "D4"
DRAW "B R2"
EXIT SUB

CASE "o" ' Lowercase o
DRAW "B R1"
DRAW "R3"
DRAW "B R1"
DRAW "B U1"
DRAW "U3"
DRAW "B U1"
DRAW "B L1"
DRAW "L3"
DRAW "B L1"
DRAW "B D1"
DRAW "D3"
DRAW "B D1"
DRAW "B R7"
EXIT SUB

CASE "p" ' Lowercase p
DRAW "U5"
DRAW "R3"
DRAW "B R1"
DRAW "B D1"
DRAW "D1"
DRAW "B D1"
DRAW "B L1"
DRAW "L3"
DRAW "B D2"
DRAW "B R6"
EXIT SUB

CASE "q" ' Lowercase q
DRAW "B R4"
DRAW "U5"
DRAW "L3"
DRAW "B L1"
DRAW "B D1"
DRAW "D1"
DRAW "B D1"
DRAW "B R1"
DRAW "R3"
DRAW "B D2"
DRAW "B R3"
EXIT SUB

CASE "r" ' Lowercase r
DRAW "U5"
DRAW "R2"
DRAW "B D5"
DRAW "B R2"
EXIT SUB

CASE "s" ' Lowercase s
DRAW "R3"
DRAW "B U1"
DRAW "B R1"
DRAW "U1"
DRAW "B U1"
DRAW "B L1"
DRAW "L3"
DRAW "U1"
DRAW "B U1"
DRAW "B R1"
DRAW "R3"
DRAW "B R3"
DRAW "B D5"
EXIT SUB

CASE "t" ' Lowercase t
DRAW "B R3"
DRAW "U7"
DRAW "B D2"
DRAW "L3"
DRAW "R6"
DRAW "B D5"
DRAW "B R2"
EXIT SUB

CASE "u" ' Lowercase u
DRAW "B U1"
DRAW "U4"
DRAW "B D5"
DRAW "B R1"
DRAW "R2"
DRAW "B R1"
DRAW "B U1"
DRAW "U4"
DRAW "B D5"
DRAW "B R2"
EXIT SUB

CASE "v" ' Lowercase v
DRAW "B R3"
DRAW "H3"
DRAW "U2"
DRAW "B D2"
DRAW "B F3"
DRAW "E3"
DRAW "U2"
DRAW "B D5"
DRAW "B R2"
EXIT SUB

CASE "w" ' Lowercase w
DRAW "U5"
DRAW "B D5"
DRAW "E3"
DRAW "F3"
DRAW "U5"
DRAW "B D5"
DRAW "B R2"
EXIT SUB

CASE "x" ' Lowercase x
DRAW "E5"
DRAW "B D5"
DRAW "H5"
DRAW "B D5"
DRAW "B R7"
EXIT SUB

CASE "y" ' Lowercase y
DRAW "E5"
DRAW "B G3"
DRAW "H3"
DRAW "B F5"
DRAW "B R3"
EXIT SUB

CASE "z" ' Lowercase z
DRAW "R5"
DRAW "B L5"
DRAW "E5"
DRAW "L5"
DRAW "B D5"
DRAW "B R8"
EXIT SUB

CASE " " ' Space
DRAW "B R4"
EXIT SUB

CASE ELSE
GameFontAscii Letter$
EXIT SUB

END SELECT
END SUB

SUB GameFontAscii (Symbol$)
SELECT CASE Symbol$

CASE "(" ' Left Parenthese "("
DRAW "B R1"
DRAW "H2"
DRAW "U4"
DRAW "E2"
DRAW "B D8"
DRAW "B R3"
EXIT SUB

CASE ")" ' Right Parenthese ")"
DRAW "B R1"
DRAW "E2"
DRAW "U4"
DRAW "H2"
DRAW "B D8"
DRAW "B R3"
EXIT SUB

CASE "?" ' Question Mark "?"
DRAW "B R2"
DRAW "U1"
DRAW "B U2"
DRAW "U1"
DRAW "E1"
DRAW "U1"
DRAW "B U1"
DRAW "B L1"
DRAW "L1"
DRAW "B L1"
DRAW "B D1"
DRAW "D1"
DRAW "B D5"
DRAW "B R5"
EXIT SUB

CASE "." ' Period "."
DRAW "R1"
DRAW "B R2"
EXIT SUB

CASE "/" ' Forward Slash "/"
FOR i% = 1 TO 2
DRAW "U1"
DRAW "B U1"
DRAW "B R1"
DRAW "U1"
NEXT i%
DRAW "U1"
DRAW "B R2"
DRAW "B D7"
EXIT SUB

CASE "\" ' Backward Slash "\"
DRAW "B R2"
FOR i% = 1 TO 2
DRAW "U1"
DRAW "B U1"
DRAW "B L1"
DRAW "U1"
NEXT i%
DRAW "U1"
DRAW "B R5"
DRAW "B D7"
EXIT SUB

CASE ":" ' Colon ":"
DRAW "B R2"
DRAW "B U2"
DRAW "U1"
DRAW "B U2"
DRAW "U1"
DRAW "B R3"
DRAW "B D6"
EXIT SUB

CASE "*" ' Star or asterisk "*"
DRAW "B U2"
DRAW "E4"
DRAW "B L2"
DRAW "D4"
DRAW "B R2"
DRAW "H4"
DRAW "B D6"
DRAW "B R8"
EXIT SUB

CASE "," ' Comma ","
DRAW "R1"
DRAW "B R1"
DRAW "B U1"
DRAW "U2"
DRAW "B D3"
DRAW "B R2"
EXIT SUB

CASE "-" ' Hyphen or dash "-"
DRAW "B U4"
DRAW "R4"
DRAW "B D4"
DRAW "B R2"
EXIT SUB

CASE "'" ' Apostrophe "'"
DRAW "B U4"
DRAW "U3"
DRAW "B D7"
DRAW "B R2"
EXIT SUB

CASE "!" ' Exclamation point "!"
DRAW "U1"
DRAW "B U2"
DRAW "U4"
DRAW "B D7"
DRAW "B R3"
EXIT SUB

CASE ELSE
EXIT SUB

END SELECT
END SUB

SUB Sentence (Text$)
LET TextData$ = Text$                   'Get the variable so I can use it
IF TextData$ = "" THEN EXIT SUB         'If there's nothing there, EXIT SUB.
LET nl% = 1                             'Set number of spaces to 1.
DO                                      'Begin loop
LET r$ = LEFT$(TextData$, nl%)          'Get text from the LEFT side
LET l$ = RIGHT$(r$, 1)                  'Read one letter from the RIGHT side
GameFont l$                             'Draw the corresponding letter/number
LET nl% = nl% + 1                       'Increase the number of spaces
LET EndText$ = (EndText$ + l$)          'Try to spell the text in TextData$.
LOOP UNTIL EndText$ = TextData$         'If EndText$ matches TextData$, STOP!
END SUB

