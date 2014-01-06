' Program Name : MKJLDR.BAS
' Description  : A program that loads and plays MKJamz files.
' Look at This : The LoadJamFile SUBroutine, and note how MaxChannel% and
'                MaxNote% are no longer CONSTant values.

' Written in 1996 by Molnar \ Kucalaba Productions

'  This program has two LoadJamFile routines.  LoadJamFileA works fine
' under QBasic but is very slow.  LoadJamFileB works very fast, but
' for some reason has a tendency to crash your system.  (It does, however,
' function fine when compiled).

' Note :  If you plan on distributing any MKJ files you create to AOL or
' anywhere in general, that is great; we encourage that.  Therefore
' this program can be distributed singularily for that reason only.


DECLARE SUB SBInit ()
DECLARE SUB CloseChannels ()
DECLARE SUB PlayMusic ()
DECLARE SUB SetIns (Channel%)
DECLARE SUB WriteReg (Reg%, value%)
DECLARE FUNCTION LoadJamFileB% (File$)
DECLARE FUNCTION LoadJamFileA% (File$)


'$DYNAMIC  ' We have to resize the Song Buffer when loading


CONST BasePort% = &H220  ' Change this for other sound cards.


DIM SHARED MaxChannel%, MaxNotes%

CONST MaxPlay% = -1

' Instrument-type.  For your convenience, use the MKJamz Instrument Creator.
' After all, that's what it's there for.
TYPE InsType
 MMult AS INTEGER     ' Modulator's Multiple
 MLevel AS INTEGER    ' Modulator's Level
 MAttack AS INTEGER   ' Modulator's Attack
 MSustain AS INTEGER  ' Modulator's Sustain
 CMult AS INTEGER     ' Carrier's Multiple
 CLevel AS INTEGER    ' Carrier's Level
 CAttack AS INTEGER   ' Carrier's Attack
 CSustain AS INTEGER  ' Carrier's Sustain
END TYPE


' This template holds the current channel information.
TYPE ChannelType
 Defined AS INTEGER   ' Has the user defined the channel and it's notes? (1 if yes)
 Speed AS SINGLE      ' The duration of each note
 SongPtr AS INTEGER   ' A pointer to the current song position
 PStat AS SINGLE      ' Pause counter 2
 Octave AS INTEGER    ' The Octave of each channel
 WaveForm AS INTEGER  ' The Wave Form of each channel
 Flag AS INTEGER      ' Misc control flag
 Flag2 AS INTEGER     ' Number of times to play song
 IsPlaying AS INTEGER ' Is a note playing (1=yes)
 IsDone AS INTEGER    ' Has the note stopped? (1=yes)
END TYPE



DIM SHARED ChannelStat(1 TO 2) AS ChannelType ' We will resize this when
                                              ' loading a MK Jamz file.

DIM SHARED SongBuf(1 TO 2) AS INTEGER ' We will resize this when loading
                                      ' a MK Jamz file.

DIM SHARED CurrentIns AS InsType ' We use this buffer for loading and setting FM
                                 ' instrument types.


CLS

INPUT "MK Jamz Song to load (*.MKJ) : ", FileName$

SBInit

CLS
COLOR 14

' Use the slow but sure method of loading

LoadStatus% = LoadJamFileA(FileName$) ' The load routine is a little different.
                                     ' It's a function that returns various
                                     ' error codes if the load failed.  So
                                     ' we have to store the return code in
                                     ' a variable.  This uses LoadStatus%

IF LoadStatus% <> 1 THEN
   
 SELECT CASE LoadStatus%
   CASE -1: PRINT "File not found!": SYSTEM
   CASE -2: PRINT "Invalid .MKJ file.": SYSTEM
   CASE -3: PRINT "Advanced MKJ file encountered.": SYSTEM
 END SELECT

END IF

PRINT "Playing "; FileName$; "..."
PRINT : COLOR 10

FOR x% = 1 TO MaxChannel%
 PRINT "Channel "; x%; ": "
NEXT

LOCATE 15, 1: PRINT "Time : "
LOCATE 17, 1
PRINT "Times Played :"; : COLOR 15: PRINT " 0"

COLOR 15

Time! = TIMER
TimesPlayed% = 0

DO

 PlayMusic                          ' Play some music

 FOR x% = 1 TO MaxChannel%          ' Display some information to keep user
  LOCATE x% + 2, 14                                           ' awake.
  PRINT SongBuf(ChannelStat(x%).SongPtr); "  "
 NEXT
 LOCATE 15, 9
 PRINT USING "###.###"; TIMER - Time!
 IF ChannelStat(1).Flag2 = -1 THEN
  TimesPlayed% = TimesPlayed% + 1
  LOCATE 17, 15
  PRINT TimesPlayed%
  ChannelStat(1).Flag2 = 0
 END IF

LOOP UNTIL INKEY$ <> ""



CloseChannels

SBInit

SYSTEM

REM $STATIC
SUB CloseChannels
IF MaxChannel% > 4 THEN FirstLoop% = 3 ELSE FirstLoop% = MaxChannel%
 FOR Channel% = 0 TO FirstLoop%
  WriteReg &HB0 + Channel%, 0
 NEXT
 IF FirstLoop% = MaxChannel% THEN EXIT SUB
 FOR Channel% = Channel% TO MaxChannel%
  WriteReg &HB0 + Channel% + 5, 0
 NEXT
END SUB

FUNCTION LoadJamFileA% (File$)


OPEN File$ FOR BINARY AS #1

 IF LOF(1) < 2 THEN
  CLOSE #1
  KILL File$
  LoadJamFileA% = -1
  EXIT FUNCTION
 END IF

 IF INPUT$(6, #1) <> "MKJamz" THEN
   LoadJamFileA% = -2
   EXIT FUNCTION
 END IF

 GET #1, , Version!
 IF Version! > 1.1 THEN
  LoadJamFileA% = -3
  EXIT FUNCTION
 END IF

 GET #1, , MaxChannel%

  FOR x% = 1 TO MaxChannel%
    GET #1, , CurrentIns
    Save% = x%
    SetIns x%
    x% = Save%
   NEXT

 GET #1, , MaxNotes%

  REDIM SongBuf(1 TO (MaxChannel% + 1) * MaxNotes%) AS INTEGER
  REDIM ChannelStat(1 TO MaxChannel%) AS ChannelType

  FOR x% = 1 TO MaxChannel%
    GET #1, , ChannelStat(x%).Defined
    ChannelStat(x%).SongPtr = 1
    ChannelStat(x%).Octave = 4
  NEXT

  FOR x% = 1 TO UBOUND(SongBuf)
   GET #1, , SongBuf(x%)
  NEXT

CLOSE #1

LoadJamFileA% = 1

END FUNCTION

FUNCTION LoadJamFileB% (File$)


OPEN File$ FOR BINARY AS #1

 IF LOF(1) < 2 THEN
  CLOSE #1
  KILL File$
  LoadJamFileB% = -1
  EXIT FUNCTION
 END IF

 IF INPUT$(6, #1) <> "MKJamz" THEN
   LoadJamFileB% = -2
   EXIT FUNCTION
 END IF

 GET #1, , Version!
 IF Version! > 1.1 THEN
  LoadJamFileB% = -3
  EXIT FUNCTION
 END IF

 GET #1, , MaxChannel%

  FOR x% = 1 TO MaxChannel%
    GET #1, , CurrentIns
    Save% = x%
    SetIns x%
    x% = Save%
   NEXT

 GET #1, , MaxNotes%

  REDIM SongBuf(1 TO (MaxChannel% + 1) * MaxNotes%) AS INTEGER
  REDIM ChannelStat(1 TO MaxChannel%) AS ChannelType

  FOR x% = 1 TO MaxChannel%
    GET #1, , ChannelStat(x%).Defined
    ChannelStat(x%).SongPtr = 1
    ChannelStat(x%).Octave = 4
  NEXT


  ' Begin of FAST music loader...load 40,000 bytes of data in no time flat!
  ' Almost like BLOAD/BSAVE but much more flexible.  (If you know what you
  ' are doing.)


  RemBytes& = LOF(1) - SEEK(1)  ' The size, in bytes (not ints) of song data

  BufferSize% = 2000           ' The buffer size to use.

  DEF SEG = VARSEG(SongBuf(1)) ' The segment of the song buffer
  Ptr& = VARPTR(SongBuf(1))    ' Pointer to the song buffer

  LeftBytes& = RemBytes& MOD BufferSize% ' The amount of left over bytes

  IF (LeftBytes& < RemBytes&) THEN
    FOR QuickLoad% = 1 TO (RemBytes& - LeftBytes&) / BufferSize%
       Buffer$ = INPUT$(BufferSize%, #1) ' Load all the data into buffer
         FOR x% = 1 TO LEN(Buffer$)
            POKE Ptr&, ASC(MID$(Buffer$, x%, 1))
            Ptr& = Ptr& + 1
         NEXT
       RemBytes& = RemBytes& - BufferSize%
    NEXT
  END IF

  IF (LeftBytes& > 0) THEN
    Buffer$ = INPUT$(LeftBytes&, #1)
     FOR x% = 1 TO LEN(Buffer$)
        POKE Ptr&, ASC(MID$(Buffer$, x%, 1))
        Ptr& = Ptr& + 1
     NEXT
   END IF

  DEF SEG

CLOSE #1

LoadJamFileB% = 1

END FUNCTION

SUB PlayMusic

FOR Channel% = 1 TO MaxChannel%

  IF ChannelStat(Channel%).Defined = 0 THEN GOTO NoGotosPlease

  RChan% = Channel% - 1

  IF ChannelStat(Channel%).IsDone = 1 THEN
   ChannelStat(Channel%).PStat = TIMER + ChannelStat(Channel%).Speed
   ChannelStat(Channel%).SongPtr = ChannelStat(Channel%).SongPtr + MaxChannel%
   ChannelStat(Channel%).IsDone = 0
   ChannelStat(Channel%).IsPlaying = 0
  END IF

   IF ChannelStat(Channel%).IsPlaying = 0 AND SongBuf(ChannelStat(Channel%).SongPtr + Channel% - 1) <> 0 THEN
    ChannelStat(Channel%).PStat = TIMER + ChannelStat(Channel%).Speed
     SELECT CASE SongBuf(ChannelStat(Channel%).SongPtr + RChan%)
       CASE 68   ' "D"
         WriteReg &HA0 + RChan%, &H81
         WriteReg &HB0 + RChan%, &H21 + 4 * ChannelStat(Channel%).Octave
       CASE 69   ' "E"
         WriteReg &HA0 + RChan%, &HB0
         WriteReg &HB0 + RChan%, &H21 + 4 * ChannelStat(Channel%).Octave
       CASE 70   ' "F"
         WriteReg &HA0 + RChan%, &HCA
         WriteReg &HB0 + RChan%, &H21 + 4 * ChannelStat(Channel%).Octave
       CASE 71    ' "G"
         WriteReg &HA0 + RChan%, &H2
         WriteReg &HB0 + RChan%, &H22 + 4 * ChannelStat(Channel%).Octave
       CASE 65 ' "A"
         WriteReg &HA0 + RChan%, &H41
         WriteReg &HB0 + RChan%, &H22 + 4 * ChannelStat(Channel%).Octave
       CASE 66   ' "B"
         WriteReg &HA0 + RChan%, &H87
         WriteReg &HB0 + RChan%, &H22 + 4 * ChannelStat(Channel%).Octave
       CASE 67  '  "C"
         WriteReg &HA0 + RChan%, &HAE
         WriteReg &HB0 + RChan%, &H22 + 4 * ChannelStat(Channel%).Octave
       CASE 17 ' "C#"
         WriteReg &HA0 + RChan%, &H6B
         WriteReg &HB0 + RChan%, &H21 + 4 * ChannelStat(Channel%).Octave
       CASE 18 ' "D#"
         WriteReg &HA0 + RChan%, &H98
         WriteReg &HB0 + RChan%, &H21 + 4 * ChannelStat(Channel%).Octave
       CASE 20 ' F#"
         WriteReg &HA0 + RChan%, &HE5
         WriteReg &HB0 + RChan%, &H21 + 4 * ChannelStat(Channel%).Octave
       CASE 21 ' "G#"
         WriteReg &HA0 + RChan%, &H20
         WriteReg &HB0 + RChan%, &H22 + 4 * ChannelStat(Channel%).Octave
       CASE 15 ' "A#"
         WriteReg &HA0 + RChan%, &H63
         WriteReg &HB0 + RChan%, &H22 + 4 * ChannelStat(Channel%).Octave
       
       CASE 255
         ChannelStat(Channel%).IsPlaying = 1
         ChannelStat(Channel%).PStat = TIMER + SongBuf(ChannelStat(Channel%).SongPtr + Channel% - 1 + MaxChannel%) / 100
         ChannelStat(Channel%).Flag = -1
       CASE 254
         ChannelStat(Channel%).Octave = SongBuf(ChannelStat(Channel%).SongPtr + Channel% - 1 + MaxChannel%)
         ChannelStat(Channel%).SongPtr = ChannelStat(Channel%).SongPtr + MaxChannel%
       CASE 253
         ChannelStat(Channel%).Speed = SongBuf(ChannelStat(Channel%).SongPtr + Channel% - 1 + MaxChannel%) / 100
         ChannelStat(Channel%).SongPtr = ChannelStat(Channel%).SongPtr + MaxChannel%
       CASE 252
         ChannelStat(Channel%).WaveForm = SongBuf(ChannelStat(Channel%).SongPtr + Channel% - 1 + MaxChannel%) - 300
         ChannelStat(Channel%).SongPtr = ChannelStat(Channel%).SongPtr + MaxChannel%
         IF RChan% > 2 THEN RRChan% = RChan% + 6 ELSE RRChan% = 0
         WriteReg &HE0 + RChan% + RRChan%, ChannelStat(Channel%).WaveForm
       CASE 251
         ChannelStat(1).Flag2 = -1
          FOR MaxChan% = 1 TO MaxChannel%
           ChannelStat(MaxChan%).SongPtr = 1
           ChannelStat(MaxChan%).IsPlaying = 0
           ChannelStat(MaxChan%).IsDone = 0
          NEXT
         CloseChannels
         EXIT SUB
     END SELECT
   END IF

    IF ChannelStat(Channel%).IsPlaying = 1 THEN
     IF TIMER >= ChannelStat(Channel%).PStat THEN
      WriteReg &HB0 + RChan%, 0
      ChannelStat(Channel%).IsDone = 1
       IF ChannelStat(Channel%).Flag = -1 THEN
        ChannelStat(Channel%).SongPtr = ChannelStat(Channel%).SongPtr + MaxChannel%
        ChannelStat(Channel%).Flag = 0
       END IF
     END IF
    END IF
  
   IF ChannelStat(Channel%).SongPtr > MaxChannel% + 1 THEN
     IF SongBuf(ChannelStat(Channel%).SongPtr + RChan%) <> 0 AND SongBuf(ChannelStat(Channel%).SongPtr - MaxChannel% + RChan%) < 250 THEN
       IF ChannelStat(Channel%).IsPlaying = 0 THEN ChannelStat(Channel%).IsPlaying = 1
     END IF
   END IF

NoGotosPlease:
NEXT Channel%


FOR Channel% = 1 TO MaxChannel%

 IF ChannelStat(Channel%).IsPlaying = 0 THEN
  ChannelStat(Channel%).SongPtr = ChannelStat(Channel%).SongPtr + MaxChannel%
  IF ChannelStat(Channel%).SongPtr >= UBOUND(SongBuf) / MaxChannel% THEN
   ChannelStat(Channel%).SongPtr = 1
  END IF
 END IF
NEXT Channel%


END SUB

SUB SBInit
   FOR z% = 1 TO &HF5
     CALL WriteReg(z%, 0)
   NEXT z%
END SUB

SUB SetIns (Channel%)
CarChan% = Channel% - 1

IF Channel% > 3 THEN CarChan% = CarChan% + 5
IF Channel% > 6 THEN CarChan% = CarChan% + 5


WriteReg &H20 + CarChan%, CurrentIns.CMult  'Plays carrier note at specified octave ch. 1
WriteReg &H23 + CarChan%, CurrentIns.MMult  'Plays modulator note at specified octave ch. 1
WriteReg &H40 + CarChan%, CurrentIns.CLevel   'Set carrier total level to softest ch. 1
WriteReg &H43 + CarChan%, CurrentIns.MLevel   'Set modulator level to loudest ch. 1
WriteReg &H60 + CarChan%, CurrentIns.CAttack  'Set carrier attack and decay ch. 1
WriteReg &H63 + CarChan%, CurrentIns.MAttack  'Set modulator attack and decay ch. 1
WriteReg &H80 + CarChan%, CurrentIns.CSustain 'Set carrier sustain and release ch. 1
WriteReg &H83 + CarChan%, CurrentIns.MSustain 'Set modulator sustain and release ch. 1

END SUB

DEFINT A-Z
SUB WriteReg (Reg%, value%)
OUT BasePort% + 8, Reg%
 FOR V% = 1 TO 6
  Buf% = INP(BasePort% + 8)
 NEXT
OUT BasePort% + 9, value%
 FOR V% = 1 TO 34
  Buf% = INP(BasePort% + 9)
 NEXT
END SUB

