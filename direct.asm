%TITLE "VGA Driver - 15/1/94"

;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³       A series of procedures for using VGA mode 13h and C++       ³
;³           Written by Mr Hutts & Cergy Cranger (c)1994             ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
 
	IDEAL  ;Turbo Assembler's Special mode
	DOSSEG ;Use Intel segment ordering method! WOW..
	MODEL large;Memory model used - allows 64K for code and 64K for data

vBASE	=	0A000h   ;This is the segment which contains the video memory for
Mode	=	13h      ;<- this mode, which is 320*200*256 clrs
MaxX    =       320      ;Maximum X value for this mode
MaxY    =       200      ;    "   Y   "    "    "    "

DATASEG 
ViewPort dw 0,0,319,199
                                     

	CODESEG

;Public declarations for functions - allows their use outside this module
	PUBLIC	_graphicsmode,_textmode,_putpixel,_getpixel  
	PUBLIC	_clearvideomem,_horizline,_vertline
        PUBLIC  _getimage, _putimage,_setview,_clearviewport
        PUBLIC  _drawrect,_solidrect,_putcharc,_putstring
        PUBLIC  _imagesize


;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                               GraphicsMode                              ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes Video Mode to 320x200x256 (13h).                                ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: AX                                                             ³
;³ Input: None                                                             ³
;³ Output: exit code 255 if vga not present                                ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

PROC	_graphicsmode 

	mov ax,1A00h ; Test for presence of MCGA
	int 10h      ; using Video BIOS int
	cmp al,1Ah   ; If al=1A then we have MCGA!
        jz OK        ;Jump to OK if test successful
	mov ax,4CFFh ;Otherwise terminate program and
	int 21h      ;Exit to DOS
OK:
	xor ah,ah    ;Ah=00 - BIOS change mode function
	mov al,Mode  ;Move desired video mode to al
	int 10h      ;Do it - changes graphics mode
	ret

ENDP

;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                                 TextMode                                ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes Video Mode to 80x25 Text (03h).                                 ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: AX                                                             ³
;³ Input: None                                                             ³
;³ Output: None                                                            ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC	_textmode 

	xor ah,ah    ;Use BIOS to change mode
	mov al,3     ;Mode is 3 for 80x25
	int 10h      ;Do it
	ret

ENDP

;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                                PutPixel                                 ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Writes a pixel at a specified point with a specified colour.            ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: AX,BX,CX,DI,ES                                                 ³
;³ Input: Parameters passed thru stack in this order: Colour,Row,Col       ³
;³ Output: None	                                                           ³
;³ Notes: Writes to temporary buffer. Use WriteBuffer to show changes.     ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC	_putpixel 

	ARG Col:word,Row:word,Colour:word ;Arguments for this proc are
                                          ;passed in style usable by C

	push bp                  ;Preserve Stack Base Pointer
	mov bp,sp                ;Move base pointer. TASM does some
                                 ;juggling to allow the above listed
                                 ;variables to contain values from stack.
                                 ;Cool, huh??
        mov ax,vbase             ;Move segment address of temp buffer
        mov es,ax                ;To ES for use of ES:BX
	mov ax,[row]
        mov cx,ax
	mov ax,[col]
        mov bx,ax
	call far GetOffset		 ;DI now holds mem offset of specified coords
	mov bx,ax
        mov ax,[colour]          ;mov colour to set pixel to into AL
        mov [byte ptr es:bx],al  ;change temp buffer location

        pop bp                   ;put back bp

        ret                     ;remove 6 bytes from stack (variables)
ENDP


;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                                GetPixel                                 ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´ 
;³ Reads a colour value from the specified pixel.                          ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´ 
;³ Changes: AX,BX,CX,DI,ES                                                 ³
;³ Output: AL=Colour                                                       ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC    _getpixel 

	ARG Col:word,Row:word     ;Arguments in C passing style

	push bp                   ;preserve BP
	mov bp,sp                 ;let the compiler get args
	
        mov ax,vbase              ;Set ES to contain segment containing
        mov es,ax                 ;temp buffer
	
        mov ax,[row]
        mov cx,ax               ;move row to cx
	mov ax,[col]
	mov bx,ax               ;move col to bx

        call far GetOffset		  ;DI now holds mem offset of specified coords
        mov bx,ax
        xor ah,ah                 ;Clear AH because we felt like it
        mov al,[byte ptr es:bx]   ;Put colour at pixel into AL

        pop bp                    ;Put back BP
        
	ret 

ENDP

;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                             ClearVideoMem                               ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´ 
;³ Clears the buffer.                                                      ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´ 
;³ Changes: AX,CX,DI,ES                                                    ³
;³ Notes: Clears buffer only. Requires call to WriteBuffer to see effects. ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC	_clearvideomem 

	mov dx,di
        mov ax,vbase              ;Put the segment of temp buffer into
        mov es,ax                 ;ES for use in string instructions
        xor di,di
        mov cx,maxx*maxy/2          ;Set count to no. of bytes in buffer
        xor ax,ax                ;Set ax to 0 - value which goes to memory
        cld
        rep stosw               ;store word in ax at memory ie zero
        mov di,dx
        ret
ENDP


;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                                 GetOffset                               ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Works out the offset in the temporary buffer given co-ordinates         ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: AX,CX,DI                                                       ³
;³ Input: BX = Column, CX = Row                                            ³
;³ Output: AX = Memory offset.                                             ³
;³ Notes: This is a private routine.                                       ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

PROC	GetOffset 
        
        mov ch,cl           ;copy row value to CH
        xor ah,ah
        mov al,ch               ;ax gets row
        mov cl,8
        shl ax,cl           
        mov dx,ax           ;store first product
        
        xor ah,ah
        mov al,ch
        mov cl,6
        shl ax,cl                   ;shift left twice to multiply by 320
        add ax,bx                    ;add the col
        add ax,dx                    ;Put final address into DI
        
        mov cl,ch
        xor ch,ch           ;restore cx to original value
        ret
ENDP


;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                               HorizLine                                 ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Draw a horizontal line on the screen in one colour.                     ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: AX,CX,DI,SI,ES                                                 ³
;³ Input: Using Stack in this order: StartCol,EndCol,Row,Colour            ³
;³ Output: None                                                            ³
;³ Notes: Yet another routine which writes to the Video Buffer.            ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC	_horizline 

	ARG	Row:word,StartCol:word,EndCol:word,Colour:word

	push bp
	mov bp,sp

	mov ax,[row]                ;Put row into SI and starting col pos
	mov cx,ax
        mov ax,[startcol]           ;into DI and 
	mov bx,ax
        call GetOffset              ;Call the function, and get the correct
	mov dx,di                   ;preserve DI
        mov di,ax

        mov ax,vbase                  ;position in DI
	mov es,ax                   ;Get ES ready for string instruction
	
        mov ax,[EndCol]
        sub ax,bx
	inc ax			    ;CX for a rep instruction
	mov cx,ax
        cld

        mov ax,[Colour]             ;put our line colour into AL, then
	rep stosb                   ;away we go...line is now drawn!

	mov di,dx               ;restore di
        pop bp             
	
	ret 
ENDP
	


;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                                Vertline                                 ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Draw a vertical line on the screen in one colour.                       ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: AX,CX,DI,SI,ES                                                 ³
;³ Input: Using Stack in this order: StartRow,EndRow,Col,Colour            ³
;³ Output: None                                                            ³
;³ Notes: Yet another routine which writes to the Video Buffer.            ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC	_vertline 

	ARG	Col:word,StartRow:word,EndRow:word,Colour:word

	push bp
	mov bp,sp
            
        mov ax,vbase                ;position in DI
	mov es,ax                   ;Get ES ready for string instruction
        

	mov ax,[StartRow]           ;Put row into bx and starting col pos
	mov cx,ax
        mov ax,[col]                ;into cx and 
	mov bx,ax
        call GetOffset              ;Call the function, and get the correct
	mov bx,ax
        
        mov ax,[EndRow]             ;Get the count by subtracting 
	neg cx                  ;starting row from ending row and put in
	inc cx			    ;CX for a rep instruction
	add cx,ax
        
        mov ax,[Colour]             ;put our line colour into AL, then
        
@@1:
	mov [byte ptr es:bx],al     ;away we go...line is now drawn!
        add bx,maxx
        loop @@1

        pop bp            
	
	ret 
ENDP

;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                                 GetImage                                ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Gets a binary image from the virtual screen to a buffer                 ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: ???????????                                                    ³
;³ Input: Thru stack: TLHC - row,col,pointer to buffer,invisible clr       ³
;³ Notes: copies from temporary screen buffer to pointer.                  ³
;³ ES holds new pointer,DS gets temp screen buffer                         ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC    _getimage 

        ARG     Left:word,Top:word,Right:word,Bottom:word,Image:dword

	push bp
	mov bp,sp   ;usual 
        
        push ds
        push di             ;preserver DI - C uses it
        push si

        mov ax,vbase
        mov ds,ax           ;set data segment to temp screen buffer
        
        mov ax,[Top]
        mov cx,ax
        mov ax,[Left]       ;parameters for get offset
        mov bx,ax

        call getoffset      ;places screen offset in DI
        mov si,ax           ;move offset to source index
        cld
        les di,[image]      ;set es:di to hold pointer
       
        mov ax,[Right]
        sub ax,bx       ;subtract left from right to give number of cols
        inc ax
        stosw           ;deposit horiz dimension in buffer
        mov dx,ax       ;store column count       
           
        mov ax,[Bottom]
        sub ax,cx           ;subtract top from bottom to give number of rows
        inc ax
        stosw               ;store vertical dimension in buffer
        mov bx,ax           ;dx has vertical count
        
        mov ax,maxx
        sub ax,dx           ;adjust vertical increment
        
        
Vertloop:                   ;loops till ax is 0
        mov cx,dx           ;restore count from bx to cx
        rep movsb          ;loops using cx as count variable
        add si,ax
        dec bl
        jnz Vertloop

        
        pop si
        pop di
        pop ds
        pop bp
        ret         ;do not forget
ENDP


;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                                 PutImage                                ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Puts a binary image to the screen                                       ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: ???????????                                                    ³
;³ Input: Thru stack: TLHC - row,col,image offset                          ³
;³ Notes: Affects buffer only. Will clip if it goes beyond viewport limits ³
;³  Important: Define pallette colour 255 as black ie, 255 is solid black  ³
;³             Normal black 0 (default background colour) is transparent   ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC    _putimage  

        ARG     Col:word,Row:word,Image:dword

	push bp
	mov bp,sp       ;usual 
        push ds         ;preserve ds
        push si         ;preserve si
        push di         ;preserve di

        mov ax,@data
        mov es,ax                   ;set extra segment data seg for viewport
        
        lds si,[image]
        
        mov ax,[Row]                    ;¿
        mov cx,ax
        mov ax,[es:Viewport+6]             ;³
        cmp cx,ax
        jg cont                         ;ÃÄ check if paste coordinates lie
                                        ;³
                                        ;³    
        mov ax,[Col]                    ;³  outside far right and bottom   
        mov bx,ax
        mov ax,[es:Viewport+4]             ;³
        cmp bx,ax
        jg cont                         ;Ù
            
    
        mov ax,[es:Viewport]
        cmp bx,ax       ;check if negative
        jge ColOk
        neg bx          ;convert negative value to absolute value
        add bx,ax
        mov dx,[word ptr ds:si]
        cmp bx,dx
        jge cont        ;check if image is completely off screen 
        mov [col],ax
        jmp short sh
ColOk:  xor bx,bx      

sh:   mov ax,[es:Viewport+2]
        cmp cx,ax       ;check if row negative
        jge RowOk
        neg cx          ;convert to absolute offset
        add cx,ax
        mov dx,[word ptr ds:si+2]
        cmp cx,dx
        jge cont        ;check if image is completely off screen
        mov [row],ax
        jmp short sh
RowOk:  xor cx,cx

sh:   
        mov ax,[word ptr ds:si]
        mul cx
        add ax,bx       ;this gives extra offset for clipping image
        add ax,si
        add ax,4        ;shift 4 bytes paste dimensions in buffer
        mov [word ptr image],ax ;updates image to offset to copy from
        
        xchg bx,[col]
        xchg cx,[row]
               
        call getoffset  ;get position to paste image to screen temp buffer
        mov di,ax
        mov ax,vbase
        mov es,ax

        xchg bx,[col]
        xchg cx,[row]
        cld        

        mov ax,[col]
        mov dx,ax   ;store screen col offset in dx
        lodsw
        sub ax,bx          ;subtract col offsett from width to give print width
        add dx,ax           ;add width to offset
        sub dx,maxx         ;check if total goes over edge of screen
        jle @@1
        sub ax,dx        
@@1:
        mov bx,ax           ;bx gets column count control value

        jmp short blah
cont:   jmp short quit
        
blah:
        mov ax,[row]
        mov dx,ax
        lodsw
        sub ax,cx
        add dx,ax           ;add vertical column offset with print width
        sub dx,maxy
        jle @@2
        sub ax,dx
@@2:
                
        push ax            ;store row count in stack
        
        mov ax,[ds:si-4]
        sub ax,bx           ;subtract column count to give loop add
        mov cx,ax           ;cx gets si increment
        
        mov ax,maxx
        sub ax,bx           ;dx get loop add
        mov dx,ax
         
        pop ax
        mov ah,al           ;store row count in al
        
        lds si,[image]        ;restore ds:si
        mov bp,cx           ;store bx in bp
        mov cx,bx
Vloop:                   ;loops till bx is 0
        lodsb
        or al,al
        jz  none
        stosb          ;loops using cx as count variable
        jmp short printok
none:   
        inc di
printok:        
        loop Vloop
        mov cx,bx         ;restore column count from stack
        add si,bp          ;adjust down a row within image
        add di,dx         ;adjust down a row on screen memory buffer
        dec ah
        jnz Vloop
        
Quit:       ;end of procedure
        pop di
        pop si
        pop ds
        pop bp
        ret 

ENDP


;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³  Notes: for Procedure imagesize                                         ³
;³  calculates the size of a buffer needed to store an image               ³
;³  given columns and rows                                                 ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC    _imagesize 

        ARG     Cols:word,Rows:word
        
        push bp
        mov bp,sp

        mov ax,[rows]
        mov bx,ax
        mov ax,[cols]
        mul bx
        add ax,4
        
        pop bp

        ret         ;do not forget
ENDP

;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³  Notes: for Procedure Rectangle                                         ³
;³  draws a rectangle! wow..                                               ³
;³  given corners                                                          ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

PROC    _drawrect 

        ARG     Left:word,Top:word,Right:word,Bottom:word,Colour:word
        
        push bp
        mov bp,sp
        push si
        push di        
        mov ax,vbase
        mov es,ax

        
        mov ax,[Top]
        mov cx,ax
        mov ax,[Left]
        mov bx,ax
        call GetOffset      ;offset on screen for top left corner
        
        mov si,ax
        mov di,ax           ;copy offset to si        
        mov dx,bx

        mov ax,[Bottom]
        sub ax,cx           ;bx=number of rows
        mov dx,ax
        inc ax
        mov cx,ax
        
        
        mov ax,[Right]      ;number of columns
        sub ax,bx
        mov bx,ax           ;bx has index to line        

        mov ax,[Colour]     ;colour of rectangle
        
thebigc:          
        mov [byte ptr es:di],al   ; draw left line
        mov [byte ptr es:di+bx],al; draw right line
        add di,maxx                 ;adjust screen position
        loop thebigc

        inc bx           ;set count to number of columns
        mov ch,dl       ;ch has number of rows
        
        xor ah,ah
        mov al,ch           ;multiply   rows by 256,by 64 and add both results
        mov cl,8
        shl ax,cl
        mov dx,ax           ;store first product
        
        xor ah,ah
        mov al,ch
        mov cl,6
        shl ax,cl
        
        add ax,dx    
        mov cx,bx

        mov bx,ax           ;set bx to offset addition

        mov di,si           ;restore screen offset
        mov ax,[Colour]
        cld
thebigh:
        mov [byte ptr es:di+bx],al
        stosb
        loop thebigh

        pop di
        pop si
        pop bp        
        ret         
ENDP

;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³  Notes: for Procedure SolidRect                                         ³
;³  draws a solid rectangle! wow..                                         ³
;³  given corners and colour                                               ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

PROC    _solidrect 

        ARG     Left:word,Top:word,Right:word,Bottom:word,Colour:word

        push bp
        mov bp,sp
        push di

        mov ax,vbase
        mov es,ax
        
        mov ax,[Top]
        mov cx,ax
        mov ax,[Left]
        mov bx,ax
        call GetOffset      ;offset on screen for top left corner
        mov di,ax
      
        mov ax,[right]
        sub ax,bx           ;bx has column count
        inc ax
        mov bx,ax

        mov ax,[bottom]     ;cx has row count
        sub ax,cx
        inc ax
        mov cx,ax

        mov ax,maxx
        sub ax,bx           ;dx has adjust down a row on screen                
        mov dx,ax

        mov ax,[Colour]     ;ax has colour       
        mov ah,cl           ;copy row count to ah     
        cld
fill:
        mov cx,bx
        rep stosb
        add di,dx
        dec ah
        jnz fill
    
        pop di        
        pop bp
        ret 
ENDP

;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³  Notes: for Procedure SetView                                           ³
;³  Sets the co-ordinates of the active clipping viewport (images only)    ³
;³  Changes local variables defined in data segment                        ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC    _setview 

    ARG     Left:word,Top:word,Right:word,Bottom:word

        push bp
        mov bp,sp
        
        mov ax,[left]
        mov [Viewport],ax
        mov ax,[top]
        mov [Viewport+2],ax
        mov ax,[right]
        mov [Viewport+4],ax
        mov ax,[bottom]
        mov [Viewport+6],ax

        pop bp
        
        ret 
ENDP        

;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                             ClearViewPort                               ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´ 
;³ Clears the current viewport                                             ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´ 
;³ Changes: AX,CX,DI,ES                                                    ³
;³ Notes: Clears viewport only.Requires call to WriteBuffer to see effects.³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC	_clearviewport 

        
	push di
        
        mov ax,[Viewport]         ;fetch left into ax
        mov bx,ax
        mov ax,[Viewport+2]       ;fetch top into ax
        mov cx,ax
        call getoffset
        mov di,ax

        mov ax,[Viewport+4]       ;fetch right into ax
        neg bx
        add bx,ax           
        inc bx                  ;find clear width left-right+1 inclusive    

        mov ax,[Viewport+6]
        neg cx
        add cx,ax
        inc cx
        
               
        mov ax,vbase
        mov es,ax
        mov ax,maxx
        sub ax,bx           ;dx has adjust down a row on screen                
        mov dx,ax    
    
        xor ax,ax       
        mov ah,cl
        cld
clear:
        mov cx,bx
        rep stosb
        add di,dx
        dec ah
        jnz clear

        pop di
	ret
ENDP

;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                                PutChar                                  ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Displays a character according to byte input at position col,row        ³
;³ in the colour given by colour                                           ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: Bloody Everything                                              ³
;³ Output: AL=Colour                                                       ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC    _putcharc 

        ARG     Col:word,Row:word,Char:byte,Colour:word

        push bp
        mov bp,sp
	push ds     ;preserve ds
        push si     ;preserve si

	;mov ax,Seg Fonts    ;set ds to font segment
	mov ds,ax
	;mov si,OFFSET Fonts ;set si to start of fonts

        mov ax,vbase
	mov es,ax

	mov ax,[row]
	mov cx,ax
	inc cx
	mov ax,[col]
	mov bx,ax
	call getoffset
        mov dx,di       ;preserve di
        mov di,ax

	xor ah,ah
	mov al,[Char]
	sub ax,32
	mov cl,3
        shl ax,cl

	cld         ;clear direction flag
	add si,ax
	inc si

	mov ax,[colour]
	mov bl,7

ByteLoop:
	mov ah,[byte ptr ds:si]
	mov cx,7
BitLoop:
	shl ah,1
	jnc trans
	mov [es:di],al

Trans:
	inc di
	loop BitLoop

	inc si
	add di,313
	dec bl
	jnz ByteLoop

        mov di,dx
        
        pop si
	pop ds
	pop bp
	ret 
ENDP

;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                                PutString                                ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Displays a string according to pointer input at position col,row        ³
;³ in the colour given by colour                                           ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: Bloody Everything  BP holds offset in string                   ³
;³ Output: AL=Colour           SI addresses charcter maps,DI screen buffer ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC    _putstring 

        ARG     Col:word,Row:word,String:dword,Colour:word

        push bp
        mov bp,sp
	push ds     ;preserve ds
        push si     ;preserve si
        push di     ;preserve di

        mov ax,vbase
        mov ds,ax       ;set ds to data segment
        
        mov ax,[Row]
        inc ax
        mov cx,ax

        mov ax,[Col]
        mov bx,ax
        Call Getoffset      ;set di to screen position
        mov di,ax
        
        mov ax,[colour]
        mov bx,ax           ;save colour    
    
        les bp,[String]     ;set es:bp to point to string
        
        mov dx,ss
        ;mov ax,Seg Fonts
        mov ss,ax           ;set ss to font segment
        
        mov ax,bx
CharLoop:
        xor ah,ah
        mov ah,[byte ptr es:bp] ;fetch next character from string
        inc bp                  ;move to next character

        or ah,ah            ;check for null character 
        jz  Null            ;stop at null character
        
		;mov si,OFFSET Fonts
        xor bh,bh
        mov bl,ah
        sub bl,32
        mov cl,3
        shl bx,cl
        inc bx
        add si,bx           ;adjust si to start of character map
        
        xor bx,bx           ;set bx to zero
        mov ch,7
BytLoop:
        mov ah,[byte ptr ss:si]     ;fetch next byte from ds:si fonts
        mov cl,7
        inc si
BitsLoop:        
        shl ah,1
        jnc Zilch
        mov [byte ptr ds:di+bx],al     ;deposit colour at screen                
Zilch:
        inc bx
        dec cl
        jnz Bitsloop
        add bx,313                      ;adjust down a screen row
        dec ch
        jnz BytLoop         ;continue next byte of character map

        add di,8            ;adjust to next column position for character        
        jmp CharLoop        ;continue with next character of string
Null:       ;end of string
        
        mov ss,dx
        pop di
        pop si
        pop ds
        pop bp
        ret 
ENDP

END     ;end  of program
