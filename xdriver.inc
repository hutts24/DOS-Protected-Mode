;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³       A series of useful symbols to define which may need to      ³
;³                      be changed from time to time                 ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
 
	;IDEAL  ;Turbo Assembler's Special mode
	DOSSEG ;Use Intel segment ordering method! WOW..
	.MODEL small;Memory model used - allows 64K for code and 64K for data

		.386P ;use non protected 386 instructions

vBASE	=	1dh   ;This is the segment which contains the video memory for
Mode	=	13h      ;<- this mode, which is 320*200*256 clrs
MaxX    =       320      ;Maximum logical X value for this mode
MaxY    =       192      ;    "   "   "   Y   "    "    "    "
TILEWIDTH =     32       ;width for screen tiles
TILEHEIGHT =    32       ;height for tiles
SHIFT1  =       4        ;first value to shift column by
SHIFT2  =       2        ;second value to shift by
SplitHeight	=   48			;fixed value for height of the splitscreen
SplitStart  =	192			;starting line on screen of split screen
