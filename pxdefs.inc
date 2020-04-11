;зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
;Ё       A series of useful symbols to define which may need to      Ё
;Ё                      be changed from time to time                 Ё
;юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

.386

vBASE	=	0A000h   ;This is the segment which contains the video memory for
Mode	=	13h      ;<- this mode, which is 320*200*256 clrs
MaxX    =       320      ;Maximum logical X value for this mode
MaxY    =       240      ;    "   "   "   Y   "    "    "    "
TILEWIDTH =     32       ;width for screen tiles
TILEHEIGHT =    32       ;height for tiles
SHIFT1  =       4        ;first value to shift column by
SHIFT2  =       2        ;second value to shift by
SplitHeight	=   48			;fixed value for height of the splitscreen
SplitStart  =	192			;starting line on screen of split screen
