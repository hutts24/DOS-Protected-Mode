
	void extern resetmouse(void);
	void extern showcursor(void);
	void extern hidecursor(void);
	void extern mousestatus(int *x,int *y,int *btn1,int *btn2);
	void extern setcursorpos(int x,int y);
	void extern getbtnpressinfo(int btn,unsigned *num,int *lastx,int *lasty);
	void extern getbtnreleaseinfo(int btn,unsigned *num,int *lastx,int *lasty);
	void extern setboundaries(int minx,int miny,int maxx,int maxy);
	void extern setgraphicscursor(int hx,int hy,char *masks);
	void extern getsensitivity(int *horiz,int *vert,int *threshold);
	void extern setsensitivity(int horiz,int vert,int threshold);
