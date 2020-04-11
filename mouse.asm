include d:\cppwork\asm\xdriver.inc

	CODESEG

	PUBLIC  _resetmouse,_showcursor,_hidecursor,_mousestatus,_setcursorpos
	PUBLIC  _getbtnpressinfo,_getbtnreleaseinfo,_setboundaries
	PUBLIC  _setgraphicscursor,_getsensitivity,_setsensitivity
	
	

;---------------------------------
;Procedure resetmouse
;Prepares driver for interupt support
;---------------------------------

PROC    _resetmouse
	xor ax,ax
	int 33h
	ret
ENDP


;---------------------------------
;Procedure showcursor
;Tells mouse driver to display mouse cursor
;---------------------------------

PROC    _showcursor
	mov ax,1
	int 33h
	ret
ENDP


;---------------------------------
;Procedure hidecursor
;Tells mouse driver to hide mouse cursor
;---------------------------------

PROC    _hidecursor
	mov ax,2
	int 33h
	ret
ENDP


;---------------------------------
;Procedure mouse status
;Returns mouse's x and y coordinates and button state
;---------------------------------

PROC    _mousestatus

	ARG     x:dword,y:dword,b1:dword,b2:dword

        push bp
        mov bp,sp
        mov gs,di

        mov ax,3
        int 33h     ;request mouse status
        les di,[x]
        mov [es:di],cx  ;store x coord in address of x
        les di,[y]
        mov [es:di],dx  ;store y coord in addres of y
        les di,[b1]
        mov ax,bx    
        and ax,1
        mov [es:di],ax
        les di,[b2]
        shr bx,1
        mov [es:di],bx

        mov di,gs
        pop bp
        ret
ENDP
        
	


;---------------------------------
;Procdure setcursorpos
;Changes mouse cursor position give x and y coords
;---------------------------------

PROC    _setcursorpos

	ARG     x:word,y:word

	push bp
	mov bp,sp

	
	mov bx,[x]
	mov cx,[y]
	mov ax,4
        int 33h

	pop bp
	ret

ENDP



;---------------------------------
;Procedure getbtnpressinfo
;returns number of presses and 
;coordinates of last press since last call
;---------------------------------

PROC    _getbtnpressinfo

    ARG button:word,clicks:dword,xpos:dword,ypos:dword

    push bp
    mov bp,sp

    mov ax,5
    mov bx,[button]
    int 33h

    mov ax,bx       ;move clicks to ax

    les bx,[clicks]
    mov [word ptr es:bx],ax
    les bx,[xpos]
    mov [word ptr es:bx],cx
    les bx,[ypos]
    mov [word ptr es:bx],dx
    
    pop bp
    ret

ENDP



;---------------------------------
;Procedure getbtnreleaseinfo
;returns number of button releases and
;coordinates of last release since
;last call
;---------------------------------



PROC    _getbtnreleaseinfo

    ARG button:word,releases:dword,xpos:dword,ypos:dword

    push bp
    mov bp,sp

    mov ax,6
    mov bx,[button]
    int 33h

    mov ax,bx       ;copy releases to AX 

    les bx,[releases]
    mov [word ptr es:bx],ax
    les bx,[xpos]
    mov [word ptr es:bx],cx
    mov ax,dx
    mov [word ptr es:bx],dx

    pop bp
    ret
ENDP


;-----------------------------------
;Procedure setboundaries
;Sets screen position limits for mouse cursor
;-------------------------------------


PROC    _setboundaries

    ARG minx:word,miny:word,maxx:word,maxy:word

    push bp
    mov bp,sp

    mov cx,[minx]       ;set cx to min xvalue
    mov dx,[maxx]       ;set dx to max xvalue
    mov ax,7

    int 33h

    mov cx,[miny]       ;set cx to min yvalue
    mov dx,[maxy]       ;set dx to max yvalue
    mov ax,8

    int 33h

    pop bp
    ret

ENDP




;-----------------------------------
;Procedure set graphicscursor
;redefines mouse cursor for garphics mode
;-----------------------------------


PROC    _setgraphicscursor

    ARG hspotx:word,hspoty:word

    push bp
    mov bp,sp
    
    mov ax,@data
    mov es,ax
    mov dx,OFFSET screenmask
    mov bx,[hspotx]
    mov cx,[hspoty]
    mov ax,9
    
    int 33h

    pop bp
    ret

ENDP



;--------------------------------------
;Procedure getsensitivity
;returns horizontal and vertical sensitivity
;and doublespeed threshold
;------------------------------------------

PROC    _getsensitivity

    ARG horiz:dword,vert:dword,thresh:dword

    push bp
    mov bp,sp

    mov ax,27
    int 33h

    mov ax,bx

    les bx,[horiz]
    mov [word ptr es:bx],ax
    les bx,[vert]
    mov [word ptr es:bx],cx
    les bx,[thresh]
    mov [word ptr es:bx],dx

    pop bp
    ret

ENDP


;--------------------------------
;Procedure setsensitivity
;Sets vertical and horizontal sensitivity
;and doublespeed threshold value
;--------------------------------


PROC    _setsensitivity

    ARG horiz:word,vert:word,thresh:word

    push bp
    mov bp,sp

    mov bx,[horiz]
    mov cx,[vert]
    mov dx,[thresh]
    mov ax,26
    int 33h

    pop bp
    ret

ENDP

        DATASEG

screenmask  dw  0011111111111111b
            dw  0101111111111111b
            dw  0110111111111111b
            dw  0111011111111111b
            dw  0111101111111111b
            dw  0001011111111111b
            dw  1110101111111111b
            dw  1111001111111111b
            dw  1111111111111111b
            dw  1111111111111111b
            dw  1111111111111111b
            dw  1111111111111111b
            dw  1111111111111111b
            dw  1111111111111111b
            dw  1111111111111111b
            dw  1111111111111111b
cursormask  dw  0000000000000000b
            dw  0100000000000000b
            dw  0110000000000000b
            dw  0111000000000000b
            dw  0111100000000000b
            dw  0001000000000000b
            dw  0000100000000000b
            dw  0000000000000000b
            dw  0000000000000000b
            dw  0000000000000000b
            dw  0000000000000000b
            dw  0000000000000000b
            dw  0000000000000000b
            dw  0000000000000000b
            dw  0000000000000000b
            dw  0000000000000000b

END
