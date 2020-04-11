extern "C"
{

	void far _cdecl	graphicsmode(void);
	void far _cdecl textmode(void);
	void far _cdecl putpixel(int Col,int Row,int Colour);
	void far _cdecl clearvideomem(void);
	void far _cdecl writebuffer(void);
	int  far _cdecl getpixel(int col,int row);
	void far _cdecl horizline(int row,int startcol,int endcol,int Colour);
	void far _cdecl vertline(int col,int startrow,int endrow,int colour);
	void far _cdecl getimage(int left,int top,int right,int bottom,void far *image);
	void far _cdecl putimage(int col,int row,void far *image);
	unsigned far _cdecl imagesize(int no_of_cols,int no_of_rows);
	void far _cdecl drawrect(int left,int top,int right,int bottom,int colour);
	void far _cdecl solidrect(int left,int top,int right,int bottom,int colour);
	void far _cdecl setview(int left,int top,int right,int bottom);
	void far _cdecl clearviewport(void);
	void far _cdecl putcharc(int col,int row,char ch,int colour);
	void far _cdecl putstring(int col,int row,char *string,int colour);
}





