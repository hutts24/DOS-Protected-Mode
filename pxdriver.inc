extrn   graphicsmode:NEAR;(void)
extrn	virtualwidth:NEAR;(void)
extrn	set320x240:NEAR;(void)
extrn	returntextmode:NEAR;(void)
extrn	clearvideomem:NEAR;(void)
extrn	clearscreen:NEAR;(void)
extrn	setactivepage:NEAR;(int pagenum)
extrn	setvisualpage:NEAR;(int pagenum,int col,int row)
extrn	setview:NEAR;(int left,int top,int right,int bottom)
extrn	clearviewport:NEAR;(void)
extrn	startsplitscreen:NEAR;(void)
extrn	setsplitscreen:NEAR;(int startline)
extrn	shiftscreen:NEAR;(int horiz,int vert)
extrn	copyvideo:NEAR;(int screen1,int x1,int y1,int x2,int y2,int screen2,int dx,int dy)
extrn	retrace:NEAR;(void)
extrn	startvsync:NEAR;(int skips)
extrn	stopvsync:NEAR;(void)






