
	IDEAL
	DOSSEG
	MODEL large

	CODESEG

	PUBLIC _getpalreg,_setpalreg,_getallreg
	PUBLIC _greyscale,_setallreg

;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³GetPalReg - 	Function to return the 6 bit RGB values for a specified      ³
;³		VGA DAC register                                             ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC	_getpalreg

	ARG	reg:word,red:dword,grn:dword,blu:dword
        push bp
        mov bp,sp

	call FAR _vrwait
	mov dx,3C7h        ;Tell the video card which register we're starting
	mov ax,[reg]	   ;at by outputing the reg to I/O port 3C7h
	out dx,al 
	xor ah,ah
	
	mov dx,3c9h
        les bx,[red]	   ;Load ES:BX with the address of Blu
	in al,dx	   ;Get the Blu value off the stack	
    	mov [word ptr es:bx],ax ;Store the value
        les bx,[grn]       ;Do the same thing above for Green
	in al,dx
    	mov [word ptr es:bx],ax
	les bx,[blu]       ;And then for Red
	in al,dx
	mov [word ptr es:bx],ax
        pop bp
	ret

ENDP

;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³SetPalReg - 	Set a register in the VGA DAC palette                        ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC	_setpalreg

	ARG	reg:word,red:word,grn:word,blu:word
        push bp
        mov bp,sp

	call FAR _vrwait
	mov dx,3C8h   ;Set the first register to access
	mov ax,[reg]  
	out dx,al
	mov dx,3C9h   
	mov ax,[red]  
	out dx,al     ;Set the red...
	mov ax,[grn]
	out dx,al     ;...green...
	mov ax,[blu]
	out dx,al     ;...and blue values
	
        pop bp
	ret 

ENDP

;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³SetBlockReg -	Sets a block of the VGA DAC registers all at once from a     ³
;³		specified buffer in memory                                   ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC	_setallreg

	ARG	block:dword,first:word,count:word
        push bp
        mov bp,sp
	
	call FAR _vrwait
	les bx,[block]     ;Set DS:SI to location of Palette buffer
	mov dx,3C8h
	mov ax,[first]     
	out dx,al          ;Set first register to access
	mov dx,ax           ;copy first to AX
        shl ax,1           ;Multiply ax by three to get extra offset...
	add ax,dx          ;...to add to SI
	add bx,ax          ;Change offset by ax
	mov ax,[count]
        mov dx,ax           ;copy count to dx
        shl ax,1     	   ;Multiply the count by 3 to get the real count val
	add ax,dx
	mov cx,ax
	mov dx,3C9h	   
	cld
@@1:
	mov al,[byte es:bx]    ;Read the next value
	inc bx
        out dx,al          ;Change the VGA registers
	loop @@1
	
			   ;Put back preserved registers
        pop bp
	ret

ENDP

;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³GetBlockReg -	Fills the buffer specified with values from VGA DAC palette  ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC	_getallreg

	ARG block:dword, first:word,count:word
        push bp
        mov bp,sp
	
	call FAR _vrwait
        les bx,[block]	;Address the palette buffer
	mov dx,3C7h
	mov ax,[first]  ;Set first register to access
	out dx,al
	mov dx,ax       ;copy AX to DX
	shl ax,1	;Multiply register by three to get offset in mem
	add ax,dx
	add bx,ax       ;Add offset to DI
	mov ax,[count]
        mov dx,ax
        shl ax,1       ;Multiply the count by 3
	add ax,dx
	mov cx,ax	;Set the count to the new value
	mov dx,3C9h     
@@1:
	in al,dx        ;Read the VGA register
	mov [byte es:bx],al           ;Store it in the buffer
	inc bx
        loop @@1
	
	pop bp          ;Put back preserved stuff
	ret

ENDP
;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³GreyScale -	Turns a specified part of the DAC palette into shades of grey³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC	_greyscale

	ARG	first:word, count:word
        push bp
        mov bp,sp
	mov ax,[first]
	mov bx,ax
	mov ax,[count]
	mov cx,ax
        mov ax,101Bh
        int 10h
        pop bp
	ret

ENDP
;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³VRWait -	Returns to caller when the video vertical retrace has started³
;³		Prevents flickering when changing palettes, and serves as a  ³
;³		Fairly standard wait time for all computers                  ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC    _vrwait

        mov dx,03DAh
@@1:
        in al,dx
        test al,08h
        jnz @@1
@@2:
        in al,dx
        test al,08h
        jz @@2

        ret
ENDP

    END
