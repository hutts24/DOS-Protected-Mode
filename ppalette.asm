include d:\cppwork\asm\xdriver.inc

	EXTRN PaletteBuffer,PaletteTemp,NewLowPal
	EXTRN NewHighPal,PaletteFlag,VsyncActive

	CODESEG
    
	PUBLIC _getpalreg,_setpalreg,_getallpalreg,_fadepalette
	PUBLIC _greyscale,_setallpalreg,_flushpalette,_cyclepalette

;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³GetPalReg - 	Function to return the 6 bit RGB values for a specified      ³
;³		VGA DAC register (actually buffered in Palettte Buffer               ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC	_getpalreg

	ARG	reg:word,red:dword,grn:dword,blu:dword

	push bp
	mov bp,sp
	mov fs,si
	push ds

	mov ax,@data
	mov es,ax
	mov si,OFFSET PaletteBuffer

	mov ax,[reg]
	add si,ax
	add si,ax
	add si,ax		;get offset to red value of colour in palette buffer

	cld

	lodsb
	les bx,[red]	   ;Load ES:BX with the address of Blue
	mov [byte ptr es:bx],al ;Store the value

	lodsb
	les bx,[grn]       ;Do the same thing above for Green
	mov [byte ptr es:bx],al

	lodsb
	les bx,[blu]       ;And then for Red
	mov [byte ptr es:bx],al

	mov si,fs
	pop ds
	pop bp
	ret

ENDP


;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³SetPalReg - 	Set a register in the VGA DAC palette                        ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC	_setpalreg

	ARG	reg:word,red:byte,grn:byte,blu:byte

	push bp
	mov bp,sp
	mov gs,di

	mov ax,@data
	mov es,ax
	mov di,OFFSET PaletteBuffer

	mov [es:PaletteFlag],0

	cld

	mov ax,[reg]  ;dx remembers register to change

	cmp [es:NewLowPal],ax
	jle setregnotbelow
	mov [es:NewLowPal],ax
setregnotbelow:


	cmp [es:NewHighPal],ax
	jge setregnotabove
	mov [es:NewHighPal],ax
setregnotabove:


	add di,ax
	add di,ax
	add di,ax	  ;add ax * 3 to di

	mov al,[red]
	stosb		  ;Set the red buffer ...
	mov al,[grn]
	stosb		  ;...green...
	mov al,[blu]
	stosb		  ;...and blue values

	mov [es:PaletteFlag],1

	mov di,gs
	pop bp
	ret

ENDP

;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³SetBlockReg -	Sets a block of the VGA DAC registers all at once from a     ³
;³		specified buffer in memory                                   		 ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC	_setallpalreg

	ARG	block:dword,first:word,count:word

	push bp
	mov bp,sp

	push ds
	push di
	push si

	cld

	mov ax,@data
	mov es,ax
	mov ds,ax

	mov [es:PaletteFlag],0

	mov di,OFFSET PaletteBuffer
	lds si,[block]     ;Set ds:si to location of Palette buffer

	mov ax,[first]
	cmp [es:NewLowPal],ax
	jle setallpalnotbelow
	mov [es:NewLowPal],ax

setallpalnotbelow:

	add di,ax          ;copy first to AX
	add di,ax          ;Multiply ax by three to get extra offset...
	add di,ax          ;...to add to bx

	mov cx,[count]
	mov dx,cx           ;copy count to dx
	add dx,ax
	dec dx				;find last colour
	cmp [es:NewHighPal],dx
	jge setallpalnotabove
	mov [es:NewHighPal],dx
setallpalnotabove:

	mov dx,cx
	add cx,dx    	   ;Multiply the count by 3 to get the real count val
	add cx,dx
	mov bx,cx
	mov dx,cx			;cx,bx,dx have total count

	shr cx,2			;number of dwords
	and bx,3h			;left over bytes

	rep movsd
	mov cx,bx
	rep movsb           ;buffer is copied

	mov [es:PaletteFlag],1

	pop si
	pop di
	pop ds

	pop bp
	ret

ENDP

;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³GetBlockReg -	Fills the buffer specified with values from palette buffer   ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC	_getallpalreg

	ARG block:dword, first:word,count:word

	push bp
	mov bp,sp
	mov gs,di

	mov ax,@data
	mov ds,ax
	mov si,OFFSET PaletteBuffer

	les di,[block]	;Address the palette buffer
	mov ax,[first]  ;Set first register to access
	add si,ax
	add si,ax	;Multiply register by three to get offset in mem
	add si,ax
	mov cx,[count]
	mov dx,cx
	add cx,dx
	add cx,dx
	mov dx,cx
	and dx,3
	shr cx,2

	cld

	rep movsd
	mov cx,dx
	rep movsb

	mov di,gs
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

	mov bx,[first]
	mov cx,[count]
	mov ax,101Bh
	int 10h
	pop bp

	ret

ENDP




;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³flushes the palette buffer to update the vga colour registers				 ³
;³		then effecting changes to the vga registers                          ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC	_flushpalette

	mov bx,ds
	mov gs,si

	mov ax,@data
	mov ds,ax
	mov es,ax
	cld
	mov ax,[es:NewLowPal]
	mov cx,[es:NewHighPal]
	mov si,OFFSET PaletteBuffer
	add si,ax
	add si,ax
	add si,ax	;offset to start of changed palette buffer
	sub cx,ax
	inc cx
	mov dx,cx
	add cx,dx
	add cx,dx
	mov dx,3c8h
	out dx,al
	mov dx,3c9h
	rep outsb	;change palette as required

	mov ds,bx
	mov si,gs
	ret

ENDP



;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³fades in/out the VGA palette entries by manipulating the palette buffer by ³
;³		adding the increment values to each colour                           ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC	_fadepalette

	ARG	lower:word,upper:word,redinc:byte,bluinc:byte,greninc:byte

	push bp
	mov bp,sp

	mov ax,@data
	mov es,ax
	mov bx,OFFSET PaletteBuffer

	mov [es:PaletteFlag],0

	mov cx,[upper]
	cmp [es:NewHighPal],cx
	jge notabovefade
	mov [es:NewHighPal],cx

notabovefade:

	mov ax,[lower]
	add bx,ax
	add bx,ax
	add bx,ax
	cmp [es:NewLowPal],ax
	jle notbelowfade
	mov [es:NewLowPal],ax

notbelowfade:

	sub cx,ax
	inc cx
	mov ax,cx
	add cx,ax
	add cx,ax         ;got count and offset in buffer

	mov al,[redinc]
	mov ah,[bluinc]
	mov dl,[greninc]

fadeloop:
	add [byte ptr es:bx],al
	jo badredinc
redok:
	add [byte ptr es:bx+1],dl
	jo badgreeninc
greenok:
	add [byte ptr es:bx+2],ah
	jo badblueinc
blueok:
	add bx,3
	loop fadeloop

	mov [es:PaletteFlag],1
	pop bp
	ret

badredinc:
	jnc redaddoverflow
	mov [byte ptr es:bx],-128
	jmp short redok
redaddoverflow:
	mov [byte ptr es:bx],127
	jmp short redok

badgreeninc:
	jnc greenaddoverflow
	mov [byte ptr es:bx+1],-128
	jmp short greenok
greenaddoverflow:
	mov [byte ptr es:bx+1],127
	jmp short greenok

badblueinc:
	jnc blueaddoverflow
	mov [byte ptr es:bx+2],-128
	jmp short blueok
blueaddoverflow:
	mov [byte ptr es:bx+2],127
	jmp short blueok

ENDP

;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³Rotates the VGA palette entries by manipulating the palette buffer and 	 ³
;³		then effecting changes to the vga registers                          ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC	_cyclepalette

	ARG	lower:word,upper:word,increment:word

	push bp
	mov bp,sp

	push ds
	push si
	push di

	mov ax,@data
	mov ds,ax
	mov es,ax

	mov si,OFFSET PaletteBuffer
	mov di,OFFSET PaletteTemp

	mov [es:PaletteFlag],0

	std

	mov dx,[upper]
	mov ax,[es:NewHighPal]
	cmp [es:NewHighPal],dx
	jge notabove
	mov [es:NewHighPal],dx

notabove:

	mov cx,dx
	add dx,cx
	add dx,cx

	mov bx,[lower]
	mov ax,[ds:NewLowPal]
	cmp [ds:NewLowPal],bx
	jle notbelow
	mov [ds:NewLowPal],bx

notbelow:

	mov ax,bx
	add bx,ax
	add bx,ax

	mov ax,[increment]
	mov cx,ax
	add ax,cx
	add ax,cx

	mov bp,dx
	sub bp,bx
	add bp,3		;bp has total bytes to cycle

	add dx,2			;ds now is offset to last byte to cycle

	test ax,1000000000000000b	;test if signed
	jz cycleforward

	xchg dx,bx		;swap upper with lower
	neg ax
	cld

cycleforward:

	add si,dx       ;start of run
	mov gs,si		;remember start of real buffer
	add di,dx
	mov fs,di       ;remember start of temp storage

	mov cx,ax		;increment count

	rep movsb       ;copy temp storage

	mov di,gs
	mov cx,bp
	sub cx,ax		;get bulk count
	rep movsb		;copy bulk

	mov si,fs		;si starts reading from temp buffer
	mov cx,ax
	rep movsb		;copy temp storage back to new destination

	mov [es:PaletteFlag],1

	pop di
	pop si
	pop ds
	pop bp

	ret
ENDP


END
