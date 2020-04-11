extern 	void getvideo(int left,int top,int right,int bottom,void *buffer);
extern	void putvideo(int col,int row,void *buffer);
extern	void clipvideo(int col,int row,void *buffer);
extern	void maskvideo(int col,int row,void *buffer);
extern	unsigned videosize(int col,int row);
extern	void setfreevideo(void);
extern	void* valloc(unsigned buffersize);


