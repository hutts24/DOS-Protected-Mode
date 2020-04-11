EXTRN  getallpalreg:PROC;(char *palbuf,int first,int count);
;/* returns contents of multiple VGA palette registers, starting at colour
;	'first' for count number of colours. */
extrn	setallpalreg:PROC;(char *palbuf,int first,int count);
;/* sets the contents of multiple VGA palette registers, starting at colour
;	'first' for count number of colours. */
extrn	getpalreg:PROC;(int reg,char *red,char *grn,char *blu);
;/* return the red, green and blue values for palette register 'reg' */
extrn	setpalreg:PROC;(int reg,char red,char grn,char blu);
;/* sets the red, green and blue values for palette register 'reg' */
extrn	greyscale:PROC;(int first,int count);

extrn  cyclepalette:PROC;(int lower,int higher,int step);
;/* cycles the palette through from colour 'lower' to colour 'higher'
;	jumping	'step' colours at a time */
extrn  flushpalette:PROC;();
;/* writes the contents of the palette buffer to the VGA registers */

extrn  fadepalette:PROC;(int low,int high,char redadj,char grenad,char bluadj);

;/* adjusts each colour in range by the given increments to limit 127,128 */