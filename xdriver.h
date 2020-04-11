extern  void graphicsmode(void);
extern	void virtualwidth(void);
extern	void set320x240(void);
extern	void returntextmode(void);
extern	void clearvideomem(void);
extern	void clearscreen(void);
extern	void setactivepage(int pagenum);
extern	void setvisualpage(int pagenum,int col,int row);
extern	void setview(int left,int top,int right,int bottom);
extern	void clearviewport(void);
extern	void startsplitscreen(void);
extern	void setsplitscreen(int startline);
extern	void shiftscreen(int horiz,int vert);
extern	void copyvideo(int screen1,int x1,int y1,int x2,int y2,int screen2,int dx,int dy);
extern	void retrace(void);
extern	void startvsync(int skips);
extern	void stopvsync(void);






