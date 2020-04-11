extrn putpixel:PROC    ;(BX: Col,CX: Row,AL: Colour);
extrn getpixel:PROC    ;(BX: col,CX: row);
extrn horizline:PROC   ;(CX: row,BX:startcol,DX:endcol,AL: Colour);
extrn vertline:PROC    ;(BX: col,CX: startrow,DX: endrow,AL: colour);
extrn drawrect:PROC    ;(int left,int top,int right,int bottom,int colour);
extrn solidrect:PROC   ;(int left,int top,int right,int bottom,int colour);
extrn line:PROC        ;(int x1,int y1,int x2, int y2,int colour);

