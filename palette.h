extern  void getallpalreg(char *palbuf,int first,int count);
/* returns contents of multiple VGA palette registers, starting at colour
	'first' for count number of colours. */
extern	void setallpalreg(char *palbuf,int first,int count);
/* sets the contents of multiple VGA palette registers, starting at colour
	'first' for count number of colours. */
extern	void getpalreg(int reg,char *red,char *grn,char *blu);
/* return the red, green and blue values for palette register 'reg' */
extern	void setpalreg(int reg,char red,char grn,char blu);
/* sets the red, green and blue values for palette register 'reg' */
extern	void greyscale(int first,int count);

extern  void cyclepalette(int lower,int higher,int step);
/* cycles the palette through from colour 'lower' to colour 'higher'
	jumping	'step' colours at a time */
extern  void flushpalette();
/* writes the contents of the palette buffer to the VGA registers */

extern  void fadepalette(int low,int high,char redadj,char grenad,char bluadj);

/* adjusts each colour in range by the given increments to limit 127,128 */