extern	void getimage(int left,int top,int right,int bottom,void *image);
extern	void putimage(int col,int row,void *image);
extern	void clipimagex4(int col,int row,void *image);
extern	void maskimage(int col,int row,void *image);
extern	unsigned imagesize(int no_of_cols,int no_of_rows);
extern	void tileimage(void *image);
extern	void putimagex4(int col,int row,void *image);
extern	void clipmaskimagex4(int col,int row,void *image);

extern  void freadpcx(void *ibuffer,void *pbuffer,int filehandle);
/* if palettebuffer pointer is NULL, palette is ignored */