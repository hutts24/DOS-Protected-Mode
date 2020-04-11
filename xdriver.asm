%TITLE "VGA Driver - 15/1/94"

;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³       A series of procedures for using VGA mode 13h and C++       ³
;³           Written by Mr Hutts & Cergy Cranger (c)1994             ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

include d:\cppwork\asm\xdriver.inc

;Important local variables values

DATASEG

	ViewPort dw 0,0,319,191
	ActiveOffset dw 0       ;physical offsets within videomemory
	VisualOffset dw 0
	EndSplitScreen dw 0		;start address of video memory after splitscreen
	FreeVideo dw 0	 		;offset of video memory free for bitmap storage

	VsyncActive db 0

	VisualFlag db 0
	NewPelPan db 0
	NewHighByte dw 0
	NewLowByte dw 0

	SplitScreenFlag db 0
	NewSplitByte1 db 0
	NewSplitByte2 db 0
	NewSplitByte3 db 0

	VsyncDelay dw 0
	ClockCounter dw 0
	VsyncSkip db 0
	RetraceSkips db 0

	PaletteFlag db 0
	NewLowPal dw 255 		;lower limit of palette change
	NewHighPal dw 0     	;upper limit of palette change

	PaletteBuffer db 768 dup(?)
	PaletteTemp   db 768 dup(?)



PUBLIC ActiveOffset,VisualOffset,EndSplitScreen
PUBLIC ViewPort,FreeVideo,VsyncActive


;Public declarations for functions - allows their use outside this module
	PUBLIC	_graphicsmode,_returntextmode,_clearvideomem,_clearscreen
	PUBLIC	_setview,_clearviewport	,_setactivepage,_setvisualpage
	PUBLIC  _virtualwidth,_set320x240,_startsplitscreen,_setsplitscreen
	PUBLIC 	_shiftscreen, _copyvideo, _retrace, _startvsync, _stopvsync

	PUBLIC  _setpalreg,_getpalreg,_setallpalreg,_getallpalreg,_cyclepalette
	PUBLIC  _fadepalette,_greyscale,_flushpalette

	PUBLIC _putpixel,_getpixel,_horizline,_vertline
	PUBLIC _drawrect,_solidrect,_line

	PUBLIC  _getimage, _putimage, _clipimagex4, _tileimage
	PUBLIC	_imagesize, _maskimage, _putimagex4, _clipmaskimagex4
	PUBLIC  _freadpcx

	PUBLIC  _getvideo, _putvideo, _clipvideo
	PUBLIC	_videosize, _maskvideo, _setfreevideo, _valloc

CODESEG

;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                               GraphicsMode                              ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes Video Mode to 320x200x256 (13h).                                ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: AX,DX and certain vga card registers                           ³
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
	push si
		push di      ;preserve registers for silly C

		xor ah,ah    ;Ah=00 - BIOS change mode function
	mov al,Mode  ;Move desired video mode to al
	int 10h      ;Do it - changes graphics mode
	mov dx,03c4h ;index register for sequence controller on card

	mov ax,0604h ;turn of bit 3 at sequence controller register 4
	out dx,ax
	mov dx,03d4h ;index register for crtc controller

	mov ax,0014h  ;turn off underline register, index 14
	out dx,ax    ;done, now in xmode!


	mov ax,0e317h ;index register 17 for mode control register
	out dx,ax

	pop di
	pop si

	ret
ENDP



;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                               Retrace	                               ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Wait for vertical retrace to begin		                               ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: al									                           ³
;³ Input: None                                                             ³
;³ Output: bugger all						                               ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

PROC	_retrace

	mov dx,3dah
vrw1:
	in al,dx
	test al,08h
	jz vrw1    		;poll while bit is 0
vrw2:
	in al,dx
	test al,08h
	jnz vrw2        ;poll while bit is 1
	ret             ;now it has begun :)
ENDP


;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                               VirtualWidth                              ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes Video Virtual screen size to MAXX                               ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: AX,DX and certain vga card registers                           ³
;³ Input: None                                                             ³
;³ Output: Bugger All                                                      ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

PROC _virtualwidth

		mov dx,3d4h         ;crtc index
		mov al,13h
		mov ah,MaxX/8
		out dx,ax           ;set virtual width in card register
        ret
ENDP



;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                               set320x240                                ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes Video screen resolution to 320 x 240                            ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: AX,DX and certain vga card registers                           ³
;³ Input: None                                                             ³
;³ Output: Bugger All                                                      ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

PROC _set320x240

		mov dx,3c2h         ;some crtc index,get to it later
		mov al,0e3h
		out dx,al           ;set index register

		mov dx,03d4h

		mov ax,2c11h
		out dx,ax
		mov ax,0d06h
		out dx,ax
		mov ax,3e07h
		out dx,ax
		mov ax,0ea10h
		out dx,ax
		mov ax,0ac11h
		out dx,ax
		mov ax,0df12h
		out dx,ax
		mov ax,0e715h
		out dx,ax
		mov ax,0616h
		out dx,ax

		ret
ENDP



;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                               StartSplitScreen                          ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ starts a split screen						                           ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: AX,DX and certain vga card registers                           ³
;³ Input: None                                                             ³
;³ Output: Bugger All                                                      ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

PROC _startsplitscreen

	mov dx,03dah
	in al,dx

	mov al,10h+20h
	mov dx,03c0h
	out dx,al					;set flipflop?

	inc dx
	in al,dx
	or al,20h					;suppress split screen panning
	dec dx
	out dx,al					;write new mode to register

	mov dx,03dah

vrwaitstartsplit1:                         ;COME BACK TO THIS
	in al,dx
	test al,08h
	jnz vrwaitstartsplit1

vrwaitstartsplit2:                         ;COME BACK TO THIS
	in al,dx
	test al,08h

	jz vrwaitstartsplit2

	cli
	mov ax,SplitStart*2-1
	mov ah,al
	mov al,18h
	mov dx,03d4h
	out dx,ax

	mov ax,SplitStart*2-1
	and ah,1
	shl ah,4
	mov al,07h
	out dx,al
	inc dx
	in al,dx
	and al,not 10h
	or al,ah
	out dx,al

	dec dx
	mov ax,SplitStart*2-1
	and ah,2
	ror ah,3
	mov al,09h
	out dx,al
	inc dx
	in al,dx
	and al,not 40h
	or al,ah
	out dx,al
	sti

	mov [EndSplitScreen],MaxX/4*SplitHeight

	ret

ENDP




;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                               SetSplitScreen      	                   ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ changes split screen	starting line on visible screen                    ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: AX,DX and certain vga card registers                           ³
;³ Input: None                                                             ³
;³ Output: Bugger All                                                      ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

PROC _setsplitscreen

	ARG		startline:word
	push bp
	mov bp,sp	;aaargggghhh

	mov bx,[startline]
	shl bx,1
	dec bx

	mov ah,bl         ;ah first out

	mov cl,bh
	and cl,1
	shl cl,4          ;cl second out

	mov ch,bh
	and ch,2
	ror ch,3          ;ch third out

	cmp [VsyncActive],1
	je setsplitvsync

	cli
	mov al,18h
	mov dx,03d4h
	out dx,ax

	mov ah,cl
	mov al,07h
	out dx,al

	inc dx
	in al,dx
	and al,not 10h
	or al,ah
	out dx,al

	dec dx
	mov ah,ch
	mov al,09h
	out dx,al

	inc dx
	in al,dx
	and al,not 40h
	or al,ah
	out dx,al

	sti

	pop bp
	ret

setsplitvsync:

	mov [SplitScreenFlag],0
	mov [NewSplitByte1],ah
	mov [NewSplitByte2],cl
	mov [NewSplitByte3],ch
	mov [SplitScreenFlag],1

	pop bp
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
PROC	_returntextmode

	xor ah,ah    ;Use BIOS to change mode
	mov al,3     ;Mode is 3 for 80x25
	int 10h      ;Do it
	ret

ENDP




;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                             ClearVideoMem                               ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´ 
;³ Clears the buffer.                                                      ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´ 
;³ Changes: AX,BX,CX,DX,DI,ES                                              ³
;³ Notes: Clears all 4 screens of video memory                             ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC	_clearvideomem 

	mov bx,di                 ;store di in bx
		mov dx,03c4h          ;sequence controller index
		mov ax,0f02h          ;set write plane register to all, index 2
		out dx,ax             ;output to card registers
		mov ax,vbase          ;Put the segment of video buffer into
		mov es,ax             ;ES for use in string instructions
        xor di,di
        mov cx,64000          ;Set count to no. of dwords in buffer
        xor eax,eax                ;Set EAX to 0 - value which goes to memory
		cld
        rep stosd
        mov di,bx
		ret
ENDP



;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                             ClearScreen                                 ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´ 
;³ Clears the buffer.                                                      ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´ 
;³ Changes: AX,BX,CX,DX,DI,ES                                              ³
;³ Notes: Clears active screen                                             ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC	_clearscreen

	mov bx,di                 ;store di in bx
		mov dx,03c4h              ;sequence controller index
		mov ax,0f02h               ;set write plane register to all, index 2
		out dx,ax                 ;output to card registers
		mov di,[ActiveOffset]
		mov ax,vbase              ;Put the segment of video buffer into
		mov es,ax                 ;ES for use in string instructions
		mov cx,MaxX/16*MaxY		        ;Set count to no. of dwords in scree
		xor eax,eax                ;Set ax to 0 - value which goes to memory
		cld
		rep stosd
		mov di,bx
		ret
ENDP



;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                                SetActivePage                            ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ sets active page memory                                                 ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: AX,CX                                                          ³
;³ Output: none                                                            ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC    _setactivepage

	ARG pagenum:word          ;Arguments in C passing style

	push bp                   ;preserve BP
	mov bp,sp                 ;let the compiler get args

	xor ax,ax
	mov cx,[pagenum]
	cmp cx,-1                 ;-1 for splitscreen
	jz split
	mov ax,MaxX/4*MaxY          ;number of bytes per page
	mul cx                   ;calculate offset for desiredpage
	add ax,[EndSplitScreen]
split:
	mov [ActiveOffset],ax       ;change new value

	pop bp
	ret
ENDP




;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                                SetVisualPage                            ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ sets visual page memory                                                 ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: AX,CX                                                          ³
;³ Output: none                                                            ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC    _setvisualpage

	ARG pagenum:word,col:word,row:word      ;Arguments in C passing style

	push bp                   ;preserve BP
	mov bp,sp                 ;let the compiler get args

	mov cx,[pagenum]
	mov ax,MaxX/4*MaxY          ;number of bytes per page
	mul cx                   ;calculate offset for desiredpage

	mov dx,[row]
	shl dx,SHIFT1
	add ax,dx
	shl dx,SHIFT2
	add ax,dx

	mov bx,[col]
	mov cl,bl
	and cl,3h
	shl cl,1

	shr bx,2
	add ax,bx
	add ax,[EndSplitScreen]

	mov [VisualOffset],ax       ;change new value

	mov bx,ax               ;save value in bx
	shr ax,8
	mov ah,al
	mov al,0ch              ;low byte in ax
	mov gs,ax               ;;

	and bx,0ffh
	mov bh,bl
	mov bl,0dh              ;high byte

	cmp [VsyncActive],1
	je setvisvsync

	mov dx,3dah            ;;
vrwait1:                   ;;
	in al,dx               ;;
	test al,01h            ;;
	jnz vrwait1            ;;

	mov dx,3d4h            ;;crtc index port
	mov ax,gs              ;;


	cli                    ;;
	out dx,ax              ;;set low byte
	mov ax,bx              ;;
	out dx,ax              ;;set high byte
	sti                    ;;

	mov dx,03dah           ;;
vrwait2:                   ;;
	in al,dx               ;;
	test al,08h            ;;
	jz vrwait2             ;;

	mov dx,3c0h            ;;
	mov al,13h+20h         ;;
	cli                    ;;
	out dx,al              ;;
	mov al,cl              ;;
	out dx,al              ;;
	sti                    ;;

	pop bp
	ret

setvisvsync:

	mov [NewLowByte],ax
	mov [NewHighByte],bx
	mov [NewPelPan],cl

waitlast:
	cmp [VisualFlag],0
	jne waitlast

	mov [VisualFlag],1


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
;³ Notes: Clears viewport only.                                            ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC	_clearviewport


		mov fs,di
		mov gs,si

		mov dx,3c4h
		mov ax,0f02h
		out dx,ax

		mov bx,[Viewport]       ;fetch start column

		mov cx,[Viewport+4]
		sub cx,bx
		inc cx              ;CX has width of viewport
		shr cx,2            ;CX has width in bytes

		shr bx,2           ;divide left column by 4

		mov dx,[Viewport+2]
		mov ax,dx
		shl ax,SHIFT1            ;multiply by 64
		mov di,ax
		shl ax,SHIFT2
		add di,ax
		add di,bx           ;add column
		add di,[ActiveOffset]

		mov si,[Viewport+6]
		sub si,dx
		inc si              ;BX , or rather, bl has row count

		mov ax,vbase
		mov es,ax           ;set extra segment to temp screen buffer

		cld

		mov dx,MaxX/4
		sub dx,cx           ;get line increment

		mov bh,cl
		shr bh,2			;number of dwords

		mov bl,cl
		and bl,3

		xor eax,eax
viewrowlop:
		mov cl,bh
		rep stosd
		mov cl,bl
		rep stosb
		add di,dx
		dec si
		jnz viewrowlop

		mov di,fs
		mov si,gs
	ret
ENDP



;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                               ShiftScreen                               ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Shifts the contents of the active screen in the directions given        ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: AX,DX and certain vga card registers                           ³
;³ Input: None                                                             ³
;³ Notes: distances are rounded to byte planes                             ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC	_shiftscreen

	ARG     horiz:word,vert:word

	push bp
	mov bp,sp
	push ds
	mov fs,si
	mov gs,di

	mov dx,3c4h
	mov ax,0ff02h
	out dx,ax 		;set to write to all planes

	mov dx,3ceh		;read port
	mov ax,08h		;set all reads to latches
	out dx,ax

	cld				;clear direction flag

	mov di,[ActiveOffset]
	mov si,di

	mov ax,[horiz]
	sar ax,2
	mov bx,ax
	sub si,ax
	neg ax

	test bx,1000000000000000b	;test if signed
	jnz shiftleft

	neg bx
	add di,(MaxX/4)-1	 ;offset to end of line
	add si,(MaxX/4)-1
	add ax,MaxX/2   ;if scanning backwards,will jump down 2 lines
	std				;assuming scanning downwards

shiftleft:
	add bx,MaxX/4	;bx has column count,don't touch it

	mov dx,[vert]

	mov cx,dx
	sal cx,SHIFT1
	mov bp,cx
	sal cx,SHIFT2
	add bp,cx
	sub si,bp

	test dx,1000000000000000b	;test if negative
	jnz shiftup

	neg dx
	add di,(MaxX/4)*(MaxY-1)
	add si,(MaxX/4)*(MaxY-1)

	sub ax,MaxX/2

shiftup:
	add dx,Maxy		;dx has row count

	mov cx,vbase
	mov es,cx
	mov ds,cx		;set es and ds to video segment

shiftloop:
	mov cx,bx		;move how many bytes of video memory
	rep movsb
	add di,ax		;increment by ax bytes to advance to next row
	add si,ax
	dec dx
	jnz shiftloop

	mov dx,3ceh
	mov ax,0ff08h
	out dx,ax

	mov di,gs
	mov si,fs
	pop ds
	pop bp
	ret
ENDP

;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                               CopyVideo                                 ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Copies a section from a screen position to another position (any screen)³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: about everything                                               ³
;³ Input: screen numbers, coordinates screen 3 is split screen             ³
;³ Notes: May copy from one logical screen to another                      ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC	_copyvideo

	ARG     screen1:word,s1x1:word,s1y1:word,s1x2:word,s1y2:word,screen2:word,s2x1:word,s2y1:word

	push bp
	mov bp,sp
	push ds
	mov fs,si
	mov gs,di

	mov dx,3ceh		;read port
	mov ax,08h		;set all reads to latches
	out dx,ax

	mov dx,3c4h
	mov ax,0ff02h
	out dx,ax 		;set to write to all planes

	cld				;clear direction flag

	mov cx,[screen2]
	mov ax,MaxX/4*MaxY          ;number of bytes per page
	mul cx                   ;calculate offset for desiredpage

	mov dx,[s2y1]
	shl dx,SHIFT1
	add ax,dx
	shl dx,SHIFT2
	add ax,dx

	mov bx,[s2x1]
	shr bx,2
	add ax,bx
	add ax,[EndSplitScreen]
	mov di,ax               ;di has destination offset

	mov cx,[screen1]
	mov ax,MaxX/4*MaxY          ;number of bytes per page
	mul cx                   ;calculate offset for desiredpage

	mov dx,[s1y1]
	mov cx,dx               ;save row in CX
		shl dx,SHIFT1
	add ax,dx
	shl dx,SHIFT2
	add ax,dx
	add ax,[EndSplitScreen]

		mov dx,[s1y2]
		sub dx,cx
		inc dx              ;DX has number of rows to copy

		mov cx,[s1x1]
		shr cx,2
		add ax,cx
		mov si,ax           ;si has source index

        mov bx,[s1x2]       ;effectively subtract col1 from col2
        shr bx,2            ;get number of bytes wide to copy
		sub bx,cx
        inc bx

        mov ax,MaxX/4
		sub ax,bx           ;AX now has incremental difference

        mov cx,vbase
        mov es,cx
	mov ds,cx        ;set data and extra seg to video memory

copyvidloop:
	mov cx,bx		;move how many bytes of video memory
	rep movsb
	add di,ax		;increment by ax bytes to advance to next row
	add si,ax
	dec dx                  ;decrement row count
	jnz copyvidloop

	mov dx,3ceh
	mov ax,0ff08h
	out dx,ax

	mov di,gs
	mov si,fs
	pop ds

	pop bp
	ret
ENDP



;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                               Vsync_int                                 ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ This is the vertical retrace synchronizer intterupt blah blah blah	   ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: about everything                                               ³
;³ Input: number of retraces to skip with each frame		               ³
;³ Notes: Must remove it with stopvsync to prevent screwing up system      ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

PROC vsync_int

	pusha	;save all general purpose registers
	push ds ;save data segment

	mov ax,@data
	mov ds,ax ;set data segment

	inc [VsyncSkip]
	mov al,[VsyncSkip]
	cmp al,[RetraceSkips]	;check whether to skip current retrace
	jl stopclock

	cmp [VisualFlag],1
	jne stopclock

	mov dx,3d4h             ;crtc index port

	mov ax,[NewLowByte]
	out dx,ax               ;set low byte
	mov ax,[NewHighByte]
	out dx,ax               ;set high byte

stopclock:
	cli
	mov    al,34h            ;Stop the timer
	out    43h,al         ;Dont want any interrupts
	mov    al,255
	out    40h,al
	out    40h,al
	sti

	cli
	mov    dx,3dah                   ;Wait for vsync
WaitVS:
	in     al,dx
	test   al,08h
	jz     WaitVS

	mov    al,34h            ;Start timer again
	out    43h,al
	mov    ax,[VsyncDelay]
	out    40h,al
	mov    al,ah
	out    40h,al

	mov al,[VsyncSkip]
	cmp al,[RetraceSkips]
	jl novischange

	cmp [VisualFlag],1
	jne novischange

	mov [VsyncSkip],0

	mov dx,3c0h
	mov al,13h+20h
	out dx,al
	mov al,[NewPelPan]
	out dx,al

	mov [VisualFlag],0

novischange:

	cmp [SplitScreenFlag],1
	jne nosplitchange

	mov ah,[NewSplitByte1]
	mov al,18h
	mov dx,03d4h
	out dx,ax

	mov ah,[NewSplitByte2]
	mov al,07h
	out dx,al

	inc dx
	in al,dx
	and al,not 10h
	or al,ah
	out dx,al

	dec dx
	mov ah,[NewSplitByte3]
	mov al,09h
	out dx,al

	inc dx
	in al,dx
	and al,not 40h
	or al,ah
	out dx,al


nosplitchange:

	cmp [PaletteFlag],1
	jne nopalchange

	mov ax,[NewLowPal]
	mov cx,[NewHighPal]
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

	cld			;this helps

palcheck:
	lodsb
	cmp al,0
	jl belowzero
	cmp al,63
	jg above63
skipcheck:
	out dx,al
	loop palcheck	;change palette as required

	mov [PaletteFlag],0
	mov [NewLowPal],255
	mov [NewHighPal],0

nopalchange:

	mov ax,[VsyncDelay]
	add ax,100        	;total time between interrupt
	add [ClockCounter],ax
	jnc short nochain

	pop ds
	popa
	sti
	db	0eah	;hex for jump statement
	OldTimer	dd 0

nochain:
	mov al,20h
	out 20h,al

	pop ds
	popa
	sti
	iret

belowzero:
	xor al,al
	jmp short skipcheck
above63:
	mov al,63
	jmp short skipcheck

ENDP


;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                               StartVsync                                ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Starts the vertical retrace synchronizer interrupt blah blah blah	   ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: about everything                                               ³
;³ Input: number of retraces to skip with each frame		               ³
;³ Notes: Must remove it with stopvsync to prevent screwing up system      ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC	_startvsync

	ARG     skips:word

	push bp
	mov bp,sp

	mov ax,[skips]
	mov [RetraceSkips],al	;set number of animation frames to sync

	mov al,34h            ;Start timer
	out 43h,al
	xor al,al
	out 40h,al
	out 40h,al

	mov dx,3dah
getvsync1:
	in al,dx
	test al,08h
	jnz getvsync1
getvsync2:
	in al,dx
	test al,08h
	jz getvsync2

	xor al,al
	out 43h,al
	in  al,40h
	mov cl,al
	in  al,40h
	mov ch,al            ;get clock ticks in cx

getvsync3:
	in al,dx
	test al,08h
	jnz getvsync3
getvsync4:
	in al,dx
	test al,08h
	jz getvsync4

	xor al,al
	out 43h,al
	in  al,40h
	mov dl,al
	in  al,40h
	mov dh,al            ;get clock ticks in dx

	sub cx,dx			;get clock cycles between vsyncs
	sub cx,100			;add a little extra time
	mov [VsyncDelay],cx

	cli
	mov ax,3500h+08h	;vector for get timer interrupt address
	int 21h
	mov ax,es
	mov dx,SEG OldTimer
	mov es,dx
	mov [word ptr es:OldTimer],bx       ;Store in OldTimerInt
	mov [word ptr es:OldTimer+2],ax

	mov ax,2500h+08h	;vector address for set interrupt
	mov bx,ds
	mov dx,seg vsync_int
	mov ds,dx
	mov dx,offset vsync_int
	int 21h
	mov ds,bx           ;set new timer interrupt

	mov     al,34h                   ;Reprogram timer 0
	out     43h,al
	mov     ax,cx
	out     40h,al
	mov     al,ah
	out     40h,al
	sti

	mov [VsyncActive],1

	pop bp

	ret
ENDP

;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                               StopVsync                                 ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Stops the vertical retrace synchronizer interrupt blah blah blah	   	   ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: about everything                                               ³
;³ Input: number of retraces to skip with each frame		               ³
;³ Notes: Must remove it with stopvsync to prevent screwing up system      ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC _stopvsync

	push ds

	mov ax,SEG OldTimer
	mov es,ax
	mov     dx, [word ptr es:OldTimer]
	mov     ax, [word ptr es:OldTimer+2]

	mov     ds,ax
	mov     ax,2500h+08h       ;Restore the old timer int
	cli
	int     21h
	mov     al,34h                   ;Restore timer 0
	out     43h,al
	mov     al,0
	out     40h,al
	out     40h,al
	sti

	pop ds
	ret
ENDP



;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³GetPalReg - 	Function to return the 6 bit RGB values for a specified      ³
;³		VGA DAC register (actually buffered in Palettte Buffer               ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC	_getpalreg

	ARG	reg:word,red:dword,grn:dword,blu:dword

	push bp
	mov bp,sp
	mov fs,si

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

	mov di,OFFSET PaletteBuffer

	mov [PaletteFlag],0

	cld

	mov ax,[reg]  ;dx remembers register to change

	cmp [NewLowPal],ax
	jle setregnotbelow
	mov [NewLowPal],ax
setregnotbelow:


	cmp [NewHighPal],ax
	jge setregnotabove
	mov [NewHighPal],ax
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

	mov [PaletteFlag],1

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

	mov [PaletteFlag],0

	mov di,OFFSET PaletteBuffer

	mov ax,[first]
	cmp [NewLowPal],ax
	jle setallpalnotbelow
	mov [NewLowPal],ax

setallpalnotbelow:

	add di,ax          ;copy first to AX
	add di,ax          ;Multiply ax by three to get extra offset...
	add di,ax          ;...to add to bx

	mov cx,[count]
	mov dx,cx           ;copy count to dx
	add dx,ax
	dec dx				;find last colour
	cmp [NewHighPal],dx
	jge setallpalnotabove
	mov [NewHighPal],dx
setallpalnotabove:

	mov dx,cx
	add cx,dx    	   ;Multiply the count by 3 to get the real count val
	add cx,dx
	mov bx,cx
	mov dx,cx			;cx,bx,dx have total count

	shr cx,2			;number of dwords
	and bx,3h			;left over bytes

	lds si,[block]     ;Set ds:si to location of Palette buffer

	rep movsd
	mov cx,bx
	rep movsb           ;buffer is copied

	pop si
	pop di
	pop ds

	mov [PaletteFlag],1

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

	mov gs,si

	cld
	mov ax,[NewLowPal]
	mov cx,[NewHighPal]
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

	mov si,gs
	ret

ENDP



;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³fades in/out the VGA palette entries by manipulating the palette buffer by ³
;³		adding the increment values to each colour                           ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC	_fadepalette

	ARG	lower:word,upper:word,redinc:byte,greeninc:byte,blueinc:byte

	push bp
	mov bp,sp

	mov bx,OFFSET PaletteBuffer

	mov [PaletteFlag],0

	mov cx,[upper]
	cmp [NewHighPal],cx
	jge notabovefade
	mov [NewHighPal],cx

notabovefade:

	mov ax,[lower]
	add bx,ax
	add bx,ax
	add bx,ax
	cmp [NewLowPal],ax
	jle notbelowfade
	mov [NewLowPal],ax

notbelowfade:

	sub cx,ax
	inc cx  	;got count and offset in buffer

	mov al,[redinc]
	mov ah,[blueinc]
	mov dl,[greeninc]

fadeloop:
	add [byte ptr ds:bx],al
	add [byte ptr ds:bx+1],dl
	add [byte ptr ds:bx+2],ah
	add bx,3
	loop fadeloop

	mov [PaletteFlag],1
	pop bp
	ret

ENDP

;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³Rotates the VGA palette entries by manipulating the palette buffer and 	 ³
;³		then effecting changes to the vga registers                          ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC	_cyclepalette

	ARG	lower:word,upper:word,increment:word

	push bp
	mov bp,sp

	push si
	push di

	mov ax,@data
	mov es,ax

	mov si,OFFSET PaletteBuffer
	mov di,OFFSET PaletteTemp

	mov [PaletteFlag],0

	std

	mov dx,[upper]
	cmp [NewHighPal],dx
	jge notabove
	mov [NewHighPal],dx

notabove:

	mov cx,dx
	add dx,cx
	add dx,cx

	mov bx,[lower]
	cmp [NewLowPal],bx
	jle notbelow
	mov [NewLowPal],bx

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

	mov [PaletteFlag],1

	pop di
	pop si

	pop bp

	ret
ENDP


;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                                PutPixel                                 ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Writes a pixel at a specified point with a specified colour.            ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: AX,BX,CX,DX,ES                                                 ³
;³ Input: Parameters passed through stack in this order: Colour,Row,Col    ³
;³ Output: None	                                                           ³
;³ Notes: Writes to temporary buffer. Use WriteBuffer to show changes.     ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC	_putpixel 

	ARG Col:word,Row:word,Colour:word ;Arguments for this proc are
                                          ;passed in style usable by C

	push bp                  ;Preserve Stack Base Pointer
	mov bp,sp                ;Move base pointer. TASM does some

	mov bx,[col]
	mov cl,3h               ;logical and column with 3 to determine plane
	and cl,bl
	mov ax,0102h            ;02 is index for write plane register
	shl ah,cl               ;shl 1 by cl for register value for write plane
	mov dx,03c4h             ;sequence controller index register
	out dx,ax               ;set write plane register
	mov ax,[row]

	shr bx,2
	shl ax,SHIFT1           ;multiply row by 16
	add bx,ax
	shl ax,SHIFT2           ;multiply row by 64
	add bx,ax
	add bx,[ActiveOffset]

	mov ax,vbase             ;Move segment address of temp buffer
	mov es,ax                ;To ES for use of ES:BX

	mov ax,[colour]          ;mov colour to set pixel to into AL
	mov [byte ptr es:bx],al  ;change video memory

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


        mov bx,[col]
        mov ah,3h               ;logical and column with 3 to determine plane
        and ah,bl               ;value for read plane register
        mov al,4h               ;index for read register
        mov dx,03ceh             ;sequence controller index register
        out dx,ax               ;set read plane register
        
        mov ax,[row]
        shr bx,2
        shl ax,SHIFT1          ;multiply by 16
        add bx,ax
        shl ax,SHIFT2          ;multiply by 64
        add bx,ax               ;add to multiply by 80 for result
		add bx,[ActiveOffset]   ;add column to produce offset

		mov ax,vbase              ;Set ES to contain segment containing
		mov es,ax                 ;temp buffer

        xor ah,ah                 ;Clear AH because we felt like it
		mov al,[byte ptr es:bx]   ;Put colour at pixel into AL
		pop bp                    ;Put back BP

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

		push di

		cld

		mov bx,[StartCol]           ;stick startcol in BX
		mov cl,3h
		and cl,bl                   ;store startplane value in cl
		shr bx,2                    ;BX has start byte

		mov ax,[Row]
		shl ax,SHIFT1               ;multiply row by 64
		mov di,ax
		shl ax,SHIFT2               ;multiply row by 16
		add di,ax               ;add to multiply by 80 for result
		add di,[ActiveOffset]
		add di,bx               ;add column to produce offset

		mov ax,vbase
		mov es,ax                   ;set extra segment to video memory

		mov ax,0f0eh
		mov dx,[EndCol]
		shl ah,cl               ;ah has start plane mask
		mov cl,dl
		and cl,3h
		shl al,cl
		not al                  ;ah has end plane mask

		shr dx,2                ;divide endcol by 4
		sub dx,bx               ;dx has mid length byte length
		mov cx,dx

		or cx,cx                ;test if line is one byte
		jnz longline
		and ah,al               ;join bit masks into AH
		mov al,02h
		mov dx,03c4h            ;sequence controller index register
		out dx,ax
		mov ax,[Colour]
		mov [byte ptr es:di],al
		jmp short endline
longline:
		mov bh,al               ;copy end mask to bh
		mov al,02h              ;write mask register index
		mov dx,03c4h            ;index register port
		out dx,ax               ;set plane write
		mov ax,[Colour]             ;put our line colour into AL, then
		mov bl,al               ;copy colour to Bl
		stosb
		dec cx
		jz empty
		mov ax,0f02h
		out dx,ax               ;set all planes to write to
		mov ax,bx               ;restore colour from BX to AX
		mov ah,al               ;move colour to ah for moving words
		shl eax,16
		mov al,bl
		mov ah,al               ;fill EAX with colour

		ror ecx,2                ;number of doublewords in length
		rep stosd
		rol ecx,2                ;is there any extra bytes left?
		rep stosb
empty:
		mov ah,bh               ;restore end plane mask to AH
		mov al,02h
		out dx,ax
		mov al,bl               ;restore colour to ax from bx
		mov [byte ptr es:di],al
endline:

		pop di
		pop bp

	ret
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
        
        push di        
        push si
        

        cld

        mov bx,[Left]              ;stick startcol in AX
        mov cl,bl                   ;get startplane
        and cl,3h                   ;store startplane value in cl
        shr bx,2

        mov ax,[Top]
        mov dx,ax                 ;preserve top row in DX
        shl ax,SHIFT1               ;multiply row by 16
        mov di,ax
        shl ax,SHIFT2               ;multiply row by 64
        add di,ax
        add di,bx               
		add di,[ActiveOffset]
        mov si,di
        shl edi,16              ;store offset in upper EDI
        mov di,si

		mov ax,vbase
		mov es,ax                   ;set extra segment to video memory

        mov ax,0f0eh
        shl ah,cl
        mov bh,cl               ;copy plane to bh

        mov cx,[Right]
        mov si,cx                   ;copy cx to si
        and cl,3h                ;cl has end plane, ch has start plane
        shl al,cl               ;al has end horizontal plane mask              
        not al
        mov ch,bh               ;put back plane

        xchg cx,si               ;store plane vals in si, restore right
        shr cx,2
        sub cl,bl               ;CX has middle byte length of line

        neg dx
        add dx,[Bottom]         ;subtract bottom from top for depth
                                
        mov fs,dx              ;store height in FS
        
        shl dx,SHIFT1                ;multiply by 16
        mov bx,dx
        shl dx,SHIFT2                ;multiply by 64
		add bx,dx               ;add to produce offset in bx

        mov gs,cx               ;copy width to GS
        

        or cx,cx                ;test if line is one byte
        jnz longrect    
        and ah,al               ;join bit masks into AH
        mov al,02h    
        mov dx,03c4h            ;sequence controller index register
        out dx,ax
        mov ax,[Colour]
        mov [byte ptr es:di],al     ;top line
        mov [byte ptr es:di+bx],al  ;bottom line
        mov bl,al

        mov ax,0101h
        mov cx,si              ;restore plane values
        shl ah,cl               ;ah has end
        mov cl,ch
        shl al,cl               ;al has start        
        or ah,al               ;join masks in ah
        mov al,02h              ;index for write mask
        out dx,ax
        mov al,bl               ;restore colour to al

        mov bx,fs              ;restore height to bx
        cmp bx,0
		jle endrect
        mov cx,bx               ;move height for loop count to CX        
shortvert:
        add di,MaxX/4              ;advance down a line
        mov [byte ptr es:di],al ;draw both lines :)
		dec cx
		jnz shortvert

		jmp endrect
longrect:
		shl esi,16
		mov si,ax               ;ack, copy masks to si
		mov al,02h              ;write mask register index
		mov dx,03c4h            ;index register port
		out dx,ax               ;set plane write
		mov ax,[Colour]             ;put our line colour into AL, then
		mov [byte ptr es:di+bx],al  ;bottom line
		mov [byte ptr es:di],al
	inc di
		dec cx
		jz horizfinished
		mov ax,0f02h
		out dx,ax               ;set all planes to write to
		mov ax,[colour]               ;restore colour to AX
		mov ah,al               ;move colour to ah for moving words
		shl eax,16
		mov ax,[colour]
		mov ah,al               ;fill EAX with colour
		ror ecx,2                ;number of doublewords in length
		or cx,cx
		jz @@1
horizdwords:
        mov [dword ptr es:di+bx],eax    ;bottom line
        stosd
		dec cx
		jnz horizdwords
@@1:
		rol ecx,2                ;is there any extra bytes left?
		or cx,cx
		jz horizfinished
horizbytes:
		mov [byte ptr es:di+bx],al    ;bottom line
		stosb
		dec cx
		jnz horizbytes
horizfinished:
        mov ax,si               ;restore plane masks to AX
        mov ah,al               ;move end mask to ah
        mov al,02h
        out dx,ax
        mov ax,[colour]               ;restore colour to ax from bx        
        mov [byte ptr es:di],al
        mov [byte ptr es:di+bx],al

        shr edi,16              ;restore si
        mov ax,0101h
        shr esi,16              ;restore plane values
        mov cx,si
        shl al,cl               ;al has end mask
        mov cl,ch
        shl ah,cl               ;ah has start        
		cmp ah,al               ;check if vertical lines lie in same plane
        jz sameplane           ;will work fine without this

        mov si,di               ;copy di to si
        mov bl,al               ;save end mask in bl
        mov al,02h              ;index for write mask
        out dx,ax               ;set plane for first line
		mov ax,[colour]
        mov ah,bl               ;put end mask in ah
        mov bx,fs              ;put height in bx
        cmp bx,0
        jle endrect
        mov cx,bx               ;copy count to cx
        
leftloop:
        add di,MaxX/4
        mov [byte ptr es:di],al
		dec cx
		jnz leftloop
        
        mov cl,al
        mov al,02h
        out dx,ax        
        mov dx,gs
        mov di,si               ;restore value to di
        add di,dx               ;add difference between lines
        mov al,cl               ;put colour back into al
        mov cx,bx               ;restore count to cx
rightloop:
		add di,MaxX/4
        mov [byte ptr es:di],al
		dec cx
		jnz rightloop
        jmp short endrect        

sameplane:
        mov al,02h
		out dx,ax
        mov ax,[colour]
        mov bx,fs              ;restore height to BX
        cmp bx,0
        jle endrect
        mov cx,bx               ;copy to cx for count
        mov dx,gs              ;restore width to DX
        mov bx,dx               ;copy width to bx
        
samevertloop:
        add di,MaxX/4
        mov [byte ptr es:di],al
        mov [byte ptr es:di+bx],al
		dec cx
		jnz samevertloop
        

endrect:
        pop si        
        pop di
        pop bp             
	
	ret
ENDP



;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                                Vertline                                 ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Draw a vertical line on the screen in one colour.                       ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: AX,CX,BX,ES                                                    ³
;³ Input: Using Stack in this order: StartRow,EndRow,Col,Colour            ³
;³ Output: None                                                            ³
;³ Notes: Yet another rouine which writes to the Video Buffer.            ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC	_vertline 

	ARG	Col:word,StartRow:word,EndRow:word,Colour:word

	push bp
	mov bp,sp
            
		mov ax,[col]
        mov bx,ax
        and al,3h               ;logical and column with 3 to determine plane
        mov cl,al
		mov ax,0102h            ;index for write plane register
        shl ah,cl               ;register value for write plane
        mov dx,03c4h             ;sequence controller index register
        out dx,ax             

	
        shr bx,2
		mov dx,bx
        mov ax,[StartRow]           ;Put row into bx and starting col pos
	mov bx,ax                   ;copy startrow to BX
	shl ax,SHIFT1                   ;multiply by 16
	add dx,ax                   ;add column
	shl ax,SHIFT2
	add dx,ax                   ;multiply by 64
	add dx,[ActiveOffset]

	mov ax,vbase                ;position in DI
	mov es,ax                   ;Get ES ready for string instruction


	mov ax,[EndRow]             ;Get the count by subtracting 
	sub ax,bx                   
        inc ax                      ;number of rows
	mov cx,ax
        
        mov ax,[Colour]             ;put our line colour into AL, then
        mov bx,dx
vertline1:
	mov [byte ptr es:bx],al     ;away we go...line is now drawn!
		add bx,MaxX/4
		dec cx
		jnz vertline1

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
		push si

		cld

		mov bx,[left]           ;stick startcol in BX
		mov cl,3h
		and cl,bl                   ;store startplane value in cl
		shr bx,2                    ;BX has start byte

		mov si,[top]
		mov ax,si
		shl ax,SHIFT1               ;multiply row by 64
		mov di,ax
		shl ax,SHIFT2               ;multiply row by 16
		add di,ax               ;add to multiply by 80 for result
		add di,[ActiveOffset]
		add di,bx               ;add column to produce offset

		mov ax,vbase
		mov es,ax                   ;set extra segment to video memory

		mov ax,0f0eh
		mov dx,[right]
		shl ah,cl               ;al has start plane mask
		mov cl,dl
		and cl,3h
		shl al,cl
		not al                  ;ah has end plane mask

		shr dx,2                ;divide endcol by 4
		sub dx,bx               ;dx has mid length byte length
		mov cx,dx

		neg si
		add si,[bottom]
		inc si					;si has depth

		or cx,cx                ;test if line is one byte
		jnz widerect
		mov cx,si				;cx has depth of rectangle
		and ah,al               ;join bit masks into AH
		mov al,02h
		mov dx,03c4h            ;sequence controller index register
		out dx,ax
		mov ax,[Colour]
thinrect:
		mov [byte ptr es:di],al
		add di,MaxX/4			;advance a line
		dec cx
		jnz thinrect
		jmp short endsolidrect
widerect:
		mov bh,al               ;copy end mask to bh
		mov al,02h              ;write mask register index
		mov dx,03c4h            ;index register port
		out dx,ax               ;set plane write
		mov ax,[Colour]             ;put our line colour into AL, then
		mov bl,al               ;copy colour to Bl
		mov fs,di
		mov bp,si				;use bp for count
leftrect:
		mov [byte ptr es:di],al
		add di,MaxX/4
		dec bp
		jnz leftrect

		mov bp,fs
		add bp,cx
		mov di,fs					;store updated DI in FS
		mov fs,bp
		inc di

		dec cx
		jz emptysolidrect

		mov gs,si				;store height in GS
		mov bp,MaxX/4
		sub bp,cx 				;si has line inc

		mov ax,0f02h
		out dx,ax               ;set all planes to write to
		mov al,bl               ;restore colour from BX to AX
		mov ah,al               ;move colour to ah for moving words
		shl eax,16
		mov al,bl
		mov ah,al               ;fill EAX with colour

		mov dh,cl                ;number of doublewords in length
		shr dh,2
		mov dl,cl                ;is there any extra bytes left?
		and dl,3

midrect:
		mov cl,dh
		rep stosd
		mov cl,dl
		rep stosb
		add di,bp
		dec si
		jnz midrect


emptysolidrect:
		mov di,fs
		mov cx,gs
		mov ah,bh               ;restore end plane mask to AH
		mov al,02h
		mov dx,3c4h
		out dx,ax
		mov al,bl               ;restore colour to ax from bx
rightrect:
		mov [byte ptr es:di],al
		add di,MaxX/4
		dec cx
		jnz rightrect

endsolidrect:

		pop si
		pop di
		pop bp

	ret



ENDP



;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                             Line			                               ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Draws a line					                                           ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: AX,CX,DI,ES                                                    ³
;³ Notes: works for all gradients? NEEDS FIXING                                        ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC    _line

		ARG     x1:word,y1:word,x2:word,y2:word,colour:word

		push bp
		mov bp,sp   ;usual 

		push di
		push si

		mov dx,3c4h
		mov al,02h
		out dx,al

		mov ax,[y1]       ;fetch start column
		mov bx,ax

		shl ax,SHIFT1
		mov di,ax
		shl ax,SHIFT2
		add di,ax

		mov ax,[x1]
		mov dx,ax

		shr ax,2
		add di,ax			;complete offset
		add di,[ActiveOffset]

		mov ax,vbase
		mov es,ax           ;set data segment to temp screen buffer

		mov ax,[colour]
		mov ah,al

		mov cl,dl
		and cl,3h
		mov al,11h
		shl al,cl

		mov si,-MaxX/4

		sub bx,[y2]
		jge yok
		neg si
		neg bx              ;get delta y
yok:
		mov [x1],si
		neg dx
		add dx,[x2]         ;get delta x

		cmp dx,bx
		jl deltaysmaller

		mov cx,dx
		inc cx 				;get width of line (in pixels)

		mov si,bx 			;move deltay to SI
		sub si,dx			;get deltay - deltax in SI
		shl si,1			;SI now has 2 * (deltay - deltax)
		mov [x2],si

		shl bx,2			;get 2 * deltay	(inc 1 in BX)

		mov si,bx
		sub si,dx			;SI has decision variable (2 * deltay) - deltax

		;all math precalculations done

		mov dx,3c5h

pixellooplarger:
		out dx,al
		mov [byte ptr es:di],ah

		cmp si,0
		jge short incbothlarger
		add si,bx
		jmp	short pixelloklarger
incbothlarger:
		add si,[x2]
		add di,[x1]
pixelloklarger:
		rol al,1
		adc di,0
		dec cx
		jnz pixellooplarger
		jmp short finishedline


deltaysmaller:
		xchg bx,dx			;bx now has delta x,dx has delta y

		mov cx,dx           ;mov delta y to cx
		inc cx 				;get length of line (in pixels)

		mov si,bx 			;move deltax to SI
		sub si,dx			;get deltax - deltay in SI
		shl si,1			;SI now has 2 * (deltax - deltay)
		mov [x2],si

		shl bx,2			;get 2 * deltax	(inc 1 in BX)

		mov si,bx
		sub si,dx			;SI has decision variable (2 * deltax) - deltay

		;all math precalculations done

		mov dx,3c5h

pixelloopsmaller:
		out dx,al
		mov [byte ptr es:di],ah

		cmp si,0
		jge short incbothsmaller
		add si,bx
		jmp	short pixelloksmaller
incbothsmaller:
		add si,[x2]
		rol al,1
		adc di,0

pixelloksmaller:
		add di,[x1]
		dec cx
		jnz pixelloopsmaller

finishedline:

		pop si
		pop di
		pop bp

		ret         ;do not forget
ENDP



;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                                 GetImage                                ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Gets a binary image from the virtual screen to a buffer                 ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: ???????????                                                    ³
;³ Input: Through stack: TLHC - row,col,pointer to buffer,invisible clear  ³
;³ Notes: copies from active page to pointer.                              ³
;³ ES holds new pointer,DS gets temp screen buffer                         ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC    _getimage 

		ARG     Left:word,Top:word,Right:word,Bottom:word,Image:dword
                
   
	push bp
	mov bp,sp   ;usual 
        
		mov fs,di             ;preserver DI - C uses it
        push si
        push ds

		mov ax,[Left]       ;fetch start column
		mov bx,ax           ;copy Left to BX
        and al,3h           ;get start plane value
		shl eax,24          ;store start plane in upper EAX
        
        mov ax,[Top]        ;starting row
        mov cx,ax           ;copy Top to CX
        shl ax,SHIFT1            ;multiply by 16           
		mov si,ax
        shl ax,SHIFT2            ;multiply by 16
        add si,ax           ;row by 80
		add si,[ActiveOffset]
                
        mov ax,vbase
        mov ds,ax           ;set datasegment to video memory
        
        cld
                        
        les di,[image]      ;set es:di to hold pointer
               
        mov ax,[Right]
        sub ax,bx       ;subtract left from right to give number of cols
        inc ax
        stosw           ;deposit horiz dimension in buffer
        mov ch,al       ;copy value to ch
        shr ax,2        ;divide width by 4
        and ch,3        ;get remainder bytes in ch
        setnz dl
        add al,dl       ;add extra byte field
noremget:
        shr bx,2        ;divide Left by 4
		add si,bx       ;add Left mod 4 to complete offset in si
                    
        mov bh,al           ;bh has column count backup           
        neg ax
        add ax,MaxX/4
		mov gs,ax           ;store line inc in GS
        
		mov ax,[Bottom]
        sub al,cl           ;subtract top from bottom to give number of rows
        inc ax              ;al has row count
        stosw               ;store vertical dimension in buffer

        mov bp,gs           ;BP now has line inc
        
        mov bl,al           ;bl has row backup,bh backsup column

        shr eax,16          ;bring mask to ah

        mov al,04h          ;read index register
        mov dx,3ceh         ;some funny port
        out dx,al           ;set read plane
        mov al,ah           ;move mask from ah to al
        mov ah,ch           ;move remainding bytes to ah from ch

        mov cx,4h           ;prepare cx to count planes
                
		mov dh,bh
        and dh,3h
        shr bh,2
        shl edx,8     
        
planeloopget:
        push si             ;backup si
		shl ecx,24          ;shift plane count to upper CAX
        mov dx,3cfh
		out dx,al
        shr edx,8
        mov dl,bl           ;restore row count from bl to dl
rowloopget:
        mov cl,bh           ;restore dword count from bh to cx        
        rep movsd
        mov cl,dh           ;byte count
		rep movsb
        add si,bp           ;increment to next line
        dec dl
        jnz rowloopget
        
        dec ah
        setz cl             ;set cl to 1 if end of remainders
        add bp,cx             ;increment line inc if cl was set
        sub dh,cl              ;decrement column count if cl set to 1
        jge planeokget
        
        mov dh,3            ;set bytes to 3
		dec bh              ;lop off a dword

planeokget:
        pop si              ;OOOHH a pop - ewww yuck
        inc al              ;shift long planes
        and al,3h           ;test for plane wrap around
        setz cl
		add si,cx              ;increment si backup
nowrapget:
        shr ecx,24          ;restore plane count back to cx
        shl edx,8
		dec cx
		jnz planeloopget

        pop ds
        pop si
		mov di,fs
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
;³ Notes:      Define pallette colour 255 as black ie, 255 is solid black  ³
;³             No transparency                                             ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC    _putimage

		ARG     Left:word,Top:word,Image:dword

	push bp
	mov bp,sp   ;usual 

		push ds
		push di             ;preserver DI - C uses it
		mov fs,si

		mov bx,[Left]       ;fetch start column
		mov dx,[Top]

		mov cl,bl
		and cl,3h
		mov ch,11h
		shl ch,cl           ;ch has first mask

		shr bx,2           ;divide left column by 4

		shl dx,SHIFT1            ;multiply by 64
		mov di,dx
		shl dx,SHIFT2            ;multiply by 16
		add di,dx           ;ax now has offset
		add di,[ActiveOffset]
		add di,bx           ;add column

		cld

		mov ax,vbase
		mov es,ax           ;set data segment to temp screen buffer

		lds si,[image]      ;set ds:si to hold pointer

		lodsw               ;fetch width in pixels
		mov ah,al           ;copy al to ah

		shr al,2            ;divide width by 4 for bytes
		and ah,3            ;remaining bytes
		setnz dl
		add al,dl              ;add extra byte width
norem2:
		mov dx,ax
		xor dh,dh
		neg dx
		add dx,Maxx/4
		mov bp,dx

		mov bh,al           ;bh has column count backup
		mov bl,al
		and bl,3h
		shr bh,2            ;bl has column byte count

		mov cl,[byte ptr ds:si]           ;cl has row backup
		add si,2

		mov al,02h
		mov dx,3c4h
		out dx,al
		mov al,ch           ;restore write plane mask to al

		mov ch,4h
planelop2:
		mov gs,di
		mov dx,3c5h
		out dx,al
		mov dl,cl           ;restore row count from bl to dl
		shl ecx,16
rowlop2:
		mov cl,bh           ;restore column count from bh to cx
		rep movsd
		mov cl,bl
		rep movsb

		add di,bp
		dec dl
		jnz rowlop2

		dec ah
		setz cl
		add bp,cx              ;increment line inc
		sub bl,cl              ;decrement column count
		jge planeok2

		mov bl,3
		dec bh

planeok2:
		mov di,gs
		rol al,1
		adc di,0
		shr ecx,16
		dec ch
		jnz planelop2

		mov si,fs
		pop di
		pop ds
		pop bp
		ret         ;do not forget


ENDP

;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                                 PutImagex4                              ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Puts a binary image to the screen                                       ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: ???????????                                                    ³
;³ Input: Thru stack: TLHC - row,col,image offset                          ³
;³ Notes:      Define pallette colour 255 as black ie, 255 is solid black  ³
;³             No transparency,width must be multiple of 4                 ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC    _putimagex4

		ARG     Left:word,Top:word,Image:dword

	push bp
	mov bp,sp   ;usual 

		push ds
		push di             ;preserver DI - C uses it
		mov fs,si

		mov bx,[Left]       ;fetch start column
		mov dx,[Top]

		mov cl,bl
		and cl,3h
		mov ch,11h
		shl ch,cl            ;store start plane in ch
		shr bx,2           ;divide left column by 4

		shl dx,SHIFT1            ;multiply by 64
		mov di,dx
		shl dx,SHIFT2            ;multiply by 16
		add di,dx           ;ax now has offset
		add di,[ActiveOffset]
		add di,bx           ;add column

		cld

		mov ax,vbase
		mov es,ax           ;set data segment to temp screen buffer

		lds si,[image]      ;set ds:si to hold pointer

		lodsw               ;fetch width in pixels

		shr ax,2            ;divide width by 4 for bytes
		mov bh,al           ;bh has column count backup
		mov bl,al
		and bl,3h
		shr bh,2

		neg ax
		add ax,MaxX/4
		mov bp,ax           ;bp has line inc

		lodsw               ;fetch rows
		mov cl,al           ;cl has row backup

		mov al,02h
		mov ah,ch           ;restore write plane mask to al

		mov ch,04h          ;cl backs rows,ch counts planes


planelop2x4:
		mov gs,di 			;save di in gs
		mov dx,3c4h
		out dx,ax
		mov dl,cl           ;restore row count from cl to ah
		shl ecx,16
rowlop2x4:
		mov cl,bh           ;restore column dword count from bh to cx
		rep movsd
		mov cl,bl           ;restore column byte count from bh to cx
		rep movsb

		add di,bp
		dec dl
		jnz rowlop2x4

		mov di,gs           ;put di back from gs
		rol ah,1
		adc di,0
		shr ecx,16
		dec ch
		jnz planelop2x4

		mov si,fs
		pop di
		pop ds
		pop bp
		ret         ;do not forget


ENDP



;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                                MaskImage                                ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Puts a binary image to the screen                                       ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: ???????????                                                    ³
;³ Input: Thru stack: TLHC - row,col,image offset                          ³
;³ Notes:      Define pallette colour 255 as black ie, 255 is solid black  ³
;³             Normal black 0 (default background colour) is transparent   ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC    _maskimage

		ARG     Left:word,Top:word,Image:dword

	push bp
	mov bp,sp   ;usual 

		push ds
		push di             ;preserver DI - C uses it
        mov fs,si

		mov bx,[Left]       ;fetch start column
		mov dx,[Top]

		mov cl,bl
		and cl,3h
		mov ch,11h
		shl ch,cl          ;ch has initial mask
		shr bx,2           ;divide left column by 4

		shl dx,SHIFT1            ;multiply by 64
		mov di,dx
		shl dx,SHIFT2            ;multiply by 16
		add di,dx           ;ax now has offset
		add di,[ActiveOffset]
		add di,bx           ;add column

		cld

		mov ax,vbase
		mov es,ax           ;set data segment to temp screen buffer

        lds si,[image]      ;set ds:si to hold pointer
               
		lodsw               ;fetch width in pixels
		mov bl,al
        shr ax,2            ;divide width by 4 for bytes
		and bl,3            ;remaining pixels
		setnz dl
		add al,dl           ;add extra byte width

		mov bh,al           ;bh has column count backup

		neg ax
		add ax,MaxX/4
		mov bp,ax           ;bp has line inc

		lodsw               ;fetch rows
		mov cl,al           ;cl has row backup
		mov al,02h          ;bring mask to AH
		mov ah,ch           ;ah has word top control planes

		mov ch,04h          ;ch backs rows
		mov dx,3c4h
planelopmask:
		mov gs,di
		out dx,ax
		shl eax,16           ;vacate ax
		mov ah,cl           ;restore row count from cl to ah
		shl ecx,16
rowlopmask:
		mov cl,bh           ;restore column count from bh to cx
yuckyloop:
		lodsb               ;move colour from image to al
		or al,al            ;test if al is 0, clear
		jz clear
		mov [es:di],al      ;store colour in video memory
clear:
		inc di
		dec cx
		jnz yuckyloop

		add di,bp
		dec ah
		jnz rowlopmask

		shr eax,16               ;reoccupy ax
		dec bl
		setz cl
		add bp,cx              ;increment line inc
		sub bh,cl              ;decrement column count

planeokmask:
		mov di,gs
		rol ah,1
		adc di,0
		shr ecx,16
		dec ch
		jnz planelopmask

		mov si,fs
		pop di
		pop ds
		pop bp
		ret         ;do not forget


ENDP


;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                            ClipMaskImagex4                              ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Puts a binary image to the screen                                       ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: ???????????                                                    ³
;³ Input: Thru stack: TLHC - row,col,image offset                          ³
;³ Notes:      image width needs to be a multiple of 4                     ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC    _clipmaskimagex4

		ARG     Left:word,Top:word,Image:dword

	push bp
	mov bp,sp   ;usual 

		push ds
		push di             ;preserver DI - C uses it
		push si

		mov ax,@data
		mov es,ax           ;set extra segment to data segment

		cld
		lds si,[image]      ;set ds:si to hold pointer

		lodsd               ;fetch image width in AX, height in EAX

		mov bx,[Left]       ;fetch start column
		cmp bx,[es:Viewport+4]
		jg endclipmaskx4
		mov cl,bl
		and cl,3h
		mov ch,11h
		shl ch,cl           ;start plane in ch
		mov gs,cx			;save plane in gs

		xor cx,cx           ;clear cx
		xor edx,edx			;clear dx
		mov di,[es:Viewport] ;move left viewport coord to DX
		cmp di,bx
		jle noleftclipmaskx4
		mov cx,di
		sub cx,bx			;cl has left clipping value
		mov bx,di 			;now left paste coord is same as viewport left
		mov di,cx
		shr cx,2			;get byte clipping width
		and di,3h
		setnz dl
		add cx,dx			;add extra byte if needed
		add si,cx			;adjust rightwards through image
		mov dx,di
		shl edx,16			;left clip leftovers in upper edx
		jmp short norightclipmaskx4

noleftclipmaskx4:

		mov di,bx			;copy left paste coord to DX
		add di,ax			;add total columns to left screen pos
		dec di
		sub di,[es:Viewport+4]
		jle norightclipmaskx4

		mov dx,di
		shr dx,2
		add cx,dx           ;add to clipping bytes
		xor dh,dh
		and di,3h
		mov dx,di
		mov dh,dl

norightclipmaskx4:

		shl edx,8
		shr ax,2			;divide width by 4 for bytes

		mov dx,[Top]
		cmp dx,[es:Viewport+6]
		jg endclipmaskx4

		mov di,[es:Viewport+2] ;move top viewport coord to AX
		cmp di,dx
		jle notopclipmaskx4
		mov ch,[byte ptr es:Viewport+2]
		sub ch,dl			;ch has top clipping value
		mov dx,di 			;now top paste coord is same as viewport top
		mov di,ax			;temp save width in di
		mul ch				;mult clip in ch by width in al
		add si,ax			;jump down from start of image
		mov ax,di			;restore width to ax

notopclipmaskx4:
		mov bp,dx			;temp save top paste coord in BP
		shl dx,SHIFT1            ;multiply by 64
		mov di,dx
		shl dx,SHIFT2            ;multiply by 16
		add di,dx           ;ax now has offset
		add di,[es:ActiveOffset]
		shr bx,2
		add di,bx
		mov dx,bp			;restore top paste coord

testbottomclipmaskx4:

		rol eax,16			;bring image rows to AX
		add dx,ax			;add actual rows to to paste coord
		dec dx				;subtract one to get actual last row
		sub dx,[es:Viewport+6] ;if positive number, image exceeds boundary
		jle nobottomclipmaskx4

		add ch,dl			;add to row clipping total

nobottomclipmaskx4:

		sub al,ch           ;subtract total row clip from image rows
		jle endclipmaskx4

		rol eax,16			;bring back columns to AX
		mov fs,ax			;back width in FS
		mul ch
		mov bp,ax
		rol ebp,16          ;vertical image inc is in BX
		mov ax,fs

		sub al,cl           ;subtract horiz clipping from columns
		jl endclipmaskx4

		mov bh,al           ;bh has column count backup
		
		neg ax
		add ax,MaxX/4
		mov bp,ax          ;bp has line inc, swap width back to AX

		xor ax,ax
		mov al,cl           ;AX has horiz clip inc

		rol eax,16			;bring rows back to AX
		mov cl,al           ;cl has row backup

		mov ax,vbase
		mov es,ax			;set extra segment to video memory segment

		mov al,02h
		mov dx,gs			;get planes from EDX
		mov ah,dh           ;restore write plane mask to al

		mov ch,04h          ;cl backs rows,ch counts planes

planelopclipmaskx4:
		mov gs,di 			;save di in gs
		mov dx,3c4h
		out dx,ax
		mov dl,cl           ;restore row count from cl to ah
		shl ecx,16
		rol eax,16
rowlopclipmaskx4:
		mov cl,bh           ;restore column dword count from bh to cx
		jcxz emptyrow
		yuckyloopclip:
		mov bl,[ds:si]      ;move colour from image to al
		inc si
		or bl,bl            ;test if al is 0, clear
		jz clearclip
		mov [es:di],bl      ;store colour in video memory
clearclip:
		inc di
		dec cx
		jnz yuckyloopclip

emptyrow:
		add di,bp           ;add screen row adjustment
		add si,ax
		dec dl
		jnz rowlopclipmaskx4

		mov di,gs           ;put di back from gs

		shr edx,16			;bring on the clipping vals

		dec dh
		jz adjustleftmask

resumeleftmask:

		inc dl
		cmp dl,4
		jz adjustrightmask

resumerightmask:

		shl edx,16
		rol eax,16

		rol ah,1
		adc di,0

		rol ebp,16
		add si,bp           ;add image plane adjustment
		rol ebp,16

		shr ecx,16
		dec ch
		jnz planelopclipmaskx4

endclipmaskx4:
		pop si
		pop di
		pop ds
		pop bp
		ret         ;do not forget

adjustleftmask:
		dec bp
		dec ax
		dec si
		inc bh
		dec di
		jmp short resumeleftmask

adjustrightmask:
		inc bp
		inc ax
		dec bh
		jmp short resumerightmask

ENDP



;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                                 ClipImagex4                               ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Puts a binary image to the screen                                       ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: ???????????                                                    ³
;³ Input: Thru stack: TLHC - row,col,image offset                          ³
;³ Notes:      Needs Fair Optimization Yet                                 ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC    _clipimagex4

		ARG     Left:word,Top:word,Image:dword

	push bp
	mov bp,sp   ;usual 

		push ds
		push di             ;preserver DI - C uses it
		push si

		mov ax,@data
		mov es,ax           ;set extra segment to data segment

		cld
		lds si,[image]      ;set ds:si to hold pointer

		lodsd               ;fetch image width in AX, height in EAX

		mov bx,[Left]       ;fetch start column
		cmp bx,[es:Viewport+4]
		jg endclipx4
		mov cl,bl
		and cl,3h
		mov ch,11h
		shl ch,cl           ;start plane in ch
		mov gs,cx			;save plane in gs

		xor cx,cx           ;clear cx
		xor edx,edx			;clear dx
		mov di,[es:Viewport] ;move left viewport coord to DX
		cmp di,bx
		jle noleftclipx4
		mov cx,di
		sub cx,bx			;cl has left clipping value
		mov bx,di 			;now left paste coord is same as viewport left
		mov di,cx
		shr cx,2			;get byte clipping width
		and di,3h
		setnz dl
		add cx,dx			;add extra byte if needed
		add si,cx			;adjust rightwards through image
		mov dx,di
		shl edx,16			;left clip leftovers in upper edx
		jmp short norightclipx4

noleftclipx4:

		mov di,bx			;copy left paste coord to DX
		add di,ax			;add total columns to left screen pos
		dec di
		sub di,[es:Viewport+4]
		jle norightclipx4

		mov dx,di
		shr dx,2
		add cx,dx           ;add to clipping bytes
		xor dh,dh
		and di,3h
		mov dx,di
		mov dh,dl

norightclipx4:

		shl edx,8
		shr ax,2			;divide width by 4 for bytes

		mov dx,[Top]
		cmp dx,[es:Viewport+6]
		jg endclipx4

		mov di,[es:Viewport+2] ;move top viewport coord to AX
		cmp di,dx
		jle notopclipx4
		mov ch,[byte ptr es:Viewport+2]
		sub ch,dl			;ch has top clipping value
		mov dx,di 			;now top paste coord is same as viewport top
		mov di,ax			;temp save width in di
		mul ch				;mult clip in ch by width in al
		add si,ax			;jump down from start of image
		mov ax,di			;restore width to ax

notopclipx4:
		mov bp,dx			;temp save top paste coord in BP
		shl dx,SHIFT1            ;multiply by 64
		mov di,dx
		shl dx,SHIFT2            ;multiply by 16
		add di,dx           ;ax now has offset
		add di,[es:ActiveOffset]
		shr bx,2
		add di,bx
		mov dx,bp			;restore top paste coord

testbottomclipx4:

		rol eax,16			;bring image rows to AX
		add dx,ax			;add actual rows to to paste coord
		dec dx				;subtract one to get actual last row
		sub dx,[es:Viewport+6] ;if positive number, image exceeds boundary
		jle nobottomclipx4

		add ch,dl			;add to row clipping total

nobottomclipx4:

		sub al,ch           ;subtract total row clip from image rows
		jle endclipx4

		rol eax,16			;bring back columns to AX
		mov fs,ax			;back width in FS
		mul ch
		mov bp,ax
		rol ebp,16          ;vertical image inc is in BX
		mov ax,fs

		sub al,cl           ;subtract horiz clipping from columns
		jl endclipx4

		mov bh,al           ;bh has column count backup
		mov bl,al
		and bl,3h
		shr bh,2

		neg ax
		add ax,MaxX/4
		mov bp,ax          ;bp has line inc, swap width back to AX

		xor ax,ax
		mov al,cl           ;AX has horiz clip inc

		rol eax,16			;bring rows back to AX
		mov cl,al           ;cl has row backup

		mov ax,vbase
		mov es,ax			;set extra segment to video memory segment

		mov al,02h
		mov dx,gs			;get planes from EDX
		mov ah,dh           ;restore write plane mask to al

		mov ch,04h          ;cl backs rows,ch counts planes

planelopclipx4:
		mov gs,di 			;save di in gs
		mov dx,3c4h
		out dx,ax
		mov dl,cl           ;restore row count from cl to ah
		shl ecx,16
		rol eax,16
rowlopclipx4:
		mov cl,bh           ;restore column dword count from bh to cx
		rep movsd
		mov cl,bl           ;restore column byte count from bl to cx
		rep movsb

		add di,bp           ;add screen row adjustment
		add si,ax
		dec dl
		jnz rowlopclipx4

		mov di,gs           ;put di back from gs

		shr edx,16			;bring on the clipping vals
		dec dh
		jz adjustleft

resumeleft:

		inc dl
		cmp dl,4
		jz adjustright

resumeright:

		shl edx,16
		rol eax,16

		rol ah,1
		adc di,0

		rol ebp,16
		add si,bp           ;add image plane adjustment
		rol ebp,16

		shr ecx,16
		dec ch
		jnz planelopclipx4

endclipx4:
		pop si
		pop di
		pop ds
		pop bp
		ret         ;do not forget

adjustleft:
		dec bp
		dec ax
		dec si
		inc bl
		dec di
		jmp short resumeleft

adjustright:
		inc bp
		inc ax
		dec bl
		jge resumeright

		dec bh
		mov bl,3
		jmp short resumeright

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

        mov cx,[rows]
        mov ax,[cols]
		mul cx
        add ax,4
        
		pop bp

		ret         ;do not forget
ENDP


;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                                tileimage                                ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Fills the screen evenly with tiles from the image pointer parameter     ³
;³ Size defined by 'TILEWIDTH' and TILEHEIGHT                              ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC    _tileimage

		ARG     Image:dword

	push bp
	mov bp,sp   ;usual 

	push di
	push si
	push ds

	cld			;clear direction flag

	mov di,[ActiveOffset]
	mov fs,di

	lds si,[Image]      ;set ds:si to point to Image
	add si,4            ;skip image dimensions

	mov dx,3c4h
	mov al,02h           ;write plane index register
	out dx,al

	mov ax,vbase
	mov es,ax           ;set es to video memory

	mov ax,0401h         ;set to write to plane 0
	mov dx,3c5h         ;prepare dx to change

tileplane:
	mov di,fs
	out dx,al
	mov gs,ax


	mov bh,TileHeight
tileheit:
	mov bl,MaxY/TileHeight

	lodsd
	mov ebp,[dword ptr ds:si]   ;fetch second dword for one tile row
	add si,4

tilerow:
	mov cx,MaxX/TileWidth       ;number of tiles across the screen

tilecolumn:
	stosd                       ;deposit first half of row
	mov [dword ptr es:di],ebp   ;deposit second half
	add di,4
	dec cx
	jnz tilecolumn

	add di,MaxX/4*(TileHeight-1)    ;advance down
	dec bl
	jnz tilerow

	sub di,MaxX/4*(MaxY-1)
	dec bh
	jnz tileheit

	mov ax,gs
	shl al,1
	dec ah
	jnz tileplane

	pop ds
	pop si
	pop di
	pop bp

	ret

ENDP




;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                                freadpcx                                 ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ reads a pcx image from file and coverts to planar layout in buffer  	   ³
;³ uses a temporary buffer.  It is assumes that real width is multiple of 4³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC    _freadpcx

		ARG     buffer:dword,pal:dword,fileid:word

	push bp
	mov bp,sp   ;usual 
	push di
	push si
	push ds

	mov bx,[fileid]	;BX has file handle
	xor cx,cx
	mov dx,4		;CX:DX has seek offset of 4
	mov ax,4201h    ;int id 42, 1 means seek from current pos
	int 21h			;advance file pointer 4 bytes

	lds dx,[buffer]	;set ds:dx to point to buffer for file read
	les di,[buffer]

	mov ah,3fh
	mov cx,8		;read 4 integers
	int 21h			;read the ints

	add dx,4		;jump 4 bytes along buffer
	mov gs,dx		;back up dx offset in gs

	xor cx,cx
	mov dx,116		;CX:DX has seek 112
	mov ax,4201h    ;int id 42, 1 means seek from current pos
	int 21h			;jump to start of image data

	mov ax,[word ptr es:di+4]	;fetch Xmax
	sub ax,[word ptr es:di]	;sub Xmin from Xmax
	inc ax						;inc for true width
	mov [word ptr es:di],ax	;deposit width dimension

	mov cx,[word ptr es:di+6]	;fetch Ymax
	sub cx,[word ptr es:di+2]	;sub Ymin from Ymax
	inc cx						;inc for true width
	mov [word ptr es:di+2],cx	;deposit width dimension

	mul cx			;ax has total number of bytes in image
	mov si,ax		;SI counts bytes to read from the file
	push bp
	mov bp,ax
	shr bp,2		;divide count by 4 for planar length
	xor di,di		;di records current plane
	mov dx,bx
	mov bx,gs		;DS:DX again has pointer to buffer

fetchloop:
	xchg bx,dx      ;dx has buffer offset
	mov ah,3fh		;bloody dos interrupt changes ax each time
	mov cx,1		;cx counts 1 byte
	int 21h			;grab one byte from the file

	xchg bx,dx      ;now bx has buffer offset
	mov al,[byte ptr ds:bx]	;grab first byte
	mov ah,al
	and ah,0c0h
	cmp ah,0c0h
	jne storeloop

	xchg bx,dx      ;dx has buffer offset
	and al,3fh		;remove top 2 bytes to give count
	mov fs,ax			;save count in fs
	mov ah,3fh
	int 21h			;grab foloowing colour byte

	xchg bx,dx
	mov al,[byte ptr ds:bx]
	mov cx,fs		;restore count to cl

storeloop:
	mov [byte ptr ds:bx],al	;store byte in buffer
	add bx,bp	;jump along to next plane
	inc di		;shift to next plane
	and di,3h	;check if wrapped to 0
	jnz notzeroplane

	mov bx,gs	;restore original offset
	inc bx		;increment
	mov gs,bx	;back up again

notzeroplane:
	dec si		;decrement byte count
	dec cl		;decrement loop count
	jnz storeloop

	or si,si
	jnz fetchloop

	pop bp
	mov bx,dx

	cmp [pal],0
	je skippal
    
	mov ah,3fh
	mov cx,1	;count one byte
	lds dx,[pal]
	int 21h

    mov ah,3fh
	mov cx,768
	int 21h		;fetch palette info

	mov bx,dx
palshiftloop:
	shr [byte ptr ds:bx],2
	inc bx
	loop palshiftloop  ;divide all bytes by 4

	jmp short endpcx

skippal:

	xor cx,cx
	mov dx,769
	mov ax,4201h
	int 21h

	;read past colourmap info

endpcx:

	pop ds
	pop si
	pop di
	pop bp
	ret
ENDP


;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                                 SetFreeVideo                            ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ sets up the starting offset for free video storage area                 ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Notes: run once after setting up video display       		   ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC	_setfreevideo		;sets up offset for free video memory

	mov ax,[EndSplitScreen]
	add ax,MaxX*MaxY
	mov [FreeVideo],ax
	ret

ENDP



;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                                 GetVideo                                ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Gets a binary image from the active page to video storage area          ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: ???????????                                                    ³
;³ Input: Through stack: TLHC - row,col,pointer to buffer,invisible clear  ³
;³ Notes: copies from active page to free video Width must be multiple of 4³
;³ Buffer is assigned dimensions and offset in video memory                ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC    _getvideo

		ARG     Left:word,Top:word,Right:word,Bottom:word,Buffer:dword


	push bp
	mov bp,sp   ;usual 

	mov gs,di             ;preserver DI - C uses it
	mov fs,si
	push ds

	mov dx,3c4h
	mov ax,0ff02h
	out dx,ax 		;set to write to all planes

	mov bx,[Left]       ;fetch start column
	shr bx,2			;divide left by 4

	mov ax,[Top]        ;starting row
	mov cx,ax           ;copy Top to CX
	shl ax,SHIFT1            ;multiply by 16
	mov si,ax
	shl ax,SHIFT2            ;multiply by 16
	add si,ax           ;row by 80
	add si,[ActiveOffset]
	add si,bx       ;add Left mod 4 to complete offset in si

	les di,[Buffer]      ;set es:di to hold pointer

	mov ax,[Right]
	shr ax,2
	sub ax,bx       ;subtract left from right to give number of cols
	inc ax
	mov bx,ax           ;bh has column count backup

	cld

	stosw	           ;deposit horiz dimension in buffer

	neg ax
	add ax,MaxX/4

	mov dx,[Bottom]
	sub dx,cx           ;subtract top from bottom to give number of rows
	inc dx              ;al has row count

	mov [word ptr es:di],dx  ;store vertical dimension in buffer
	add di,2

	mov cx,dx
	mov bp,ax

	mov dx,3ceh		;read port
	mov ax,08h		;set all reads to latches
	out dx,ax

	mov dx,cx
	mov ax,bp

	mov cx,vbase
	mov ds,cx

rowloopgetvideo:
	mov cx,bx           ;restore byte count from bh to cx
	rep movsb
	add si,ax           ;increment to next line
	dec dx
	jnz rowloopgetvideo

	mov dx,3ceh         ;set reads back to cpu
	mov ax,0ff08h
	out dx,ax

	pop ds
	mov si,fs
	mov di,gs

	pop bp
	ret         ;do not forget
ENDP



;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                                 Putvideo                                ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Puts a binary image to the screen                                       ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: ???????????                                                    ³
;³ Input: Through stack: TLHC - row,col,image offset                       ³
;³ Notes:      Define pallette colour 255 as black ie, 255 is solid black  ³
;³             No transparency                                             ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC    _putvideo

		ARG     Left:word,Top:word,Buffer:dword

	push bp
	mov bp,sp   ;usual 

	push ds
	mov gs,di             ;preserver DI - C uses it
	mov fs,si

	mov dx,3c4h
	mov ax,0ff02h
	out dx,ax 		;set to write to all planes

	mov bx,[Left]       ;fetch start column

	mov dx,[Top]

	shr bx,2           ;divide left column by 4

	shl dx,SHIFT1            ;multiply by 64
	mov di,dx
	shl dx,SHIFT2            ;multiply by 16
	add di,dx           ;ax now has offset
	add di,[ActiveOffset]
	add di,bx           ;add column

	lds si,[Buffer]      ;set ds:si to hold pointer

	cld
	lodsw		       ;fetch width in bytes

	mov bx,ax           ;bx has column count backup
	neg ax
	add ax,MaxX/4

	mov dx,[word ptr ds:si]      ;dl has row backup

	add si,2

	mov cx,dx
	mov bp,ax

	mov dx,3ceh		;read port
	mov ax,08h		;set all reads to latches
	out dx,ax

	mov dx,cx
	mov ax,bp

	mov cx,vbase
	mov es,cx
	mov ds,cx


rowlopputvideo:

	mov cx,bx           ;restore column count from bh to cx
	rep movsb
	add di,ax
	dec dx
	jnz rowlopputvideo

	mov dx,3ceh         ;set reads back to cpu
	mov ax,0ff08h
	out dx,ax

	mov si,fs
	mov di,gs
	pop ds
	pop bp
	ret         ;do not forget


ENDP


;TO BE COMPLETED!!!!!!!!!!

;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                                Maskvideo                                ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Puts a video memory image to active video srceen                        ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: ???????????                                                    ³
;³ Input: Thru stack: TLHC - row,col,image offset                          ³
;³ Notes:      Define pallette colour 255 as black ie, 255 is solid black  ³
;³             Normal black 0 (default background colour) is transparent   ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC    _maskvideo

		ARG     Left:word,Top:word,Image:dword

	push bp
	mov bp,sp   ;usual 

		push ds
		mov gs,di             ;preserver DI - C uses it
		mov fs,si

		mov bx,[Left]       ;fetch start column

		mov dx,[Top]

		mov cl,bl
		and cl,3h
		mov al,11h
		shl al,cl
		shl eax,24           ;store start plane in upper EAX

		shr bx,2           ;divide left column by 4

		shl dx,SHIFT1            ;multiply by 64
		mov di,dx
		shl dx,SHIFT2            ;multiply by 16
		add di,dx           ;ax now has offset
		add di,[ActiveOffset]
		add di,bx           ;add column

		cld

		mov ax,vbase
		mov es,ax           ;set data segment to temp screen buffer

		lds si,[image]      ;set ds:si to hold pointer

		lodsw               ;fetch width in pixels
		mov cl,al           ;copy al to cl

		shr ax,2            ;divide width by 4 for bytes
		and cl,3            ;remaining bytes
		setnz dl
		add al,dl              ;add extra byte width

		mov bh,al           ;bh has column count backup
		neg ax
		add ax,MaxX/4
		mov bp,ax           ;bp has line inc

		lodsw               ;fetch rows
		mov bl,al           ;bl has row backup
		shr eax,16          ;bring mask to AH

		mov al,02h
		mov dx,3c4h
		out dx,al
		mov al,ah           ;restore write plane mask to al
		mov ah,cl           ;move extra bytes to ah

		mov cx,4h

planelopmaskvideo:
		push di
		shl ecx,24
		mov dx,3c5h
		out dx,al
		mov dl,bl           ;restore row count from bl to dl
		shl eax,8           ;vacate al
rowlopmaskvideo:
		mov cl,bh           ;restore column count from bh to cx
yuckyloopvideo:
		lodsb               ;move colour from image to al
		or al,al            ;test if al is 0, clear
		jz clear
		mov [es:di],al      ;store colour in video memory
clearvideo:
		inc di
		dec cx
		jnz yuckyloopvideo

		add di,bp
		dec dl
		jnz rowlopmaskvideo

		shr eax,8               ;reoccupy al
		dec ah
		setz cl
		add bp,cx              ;increment line inc
		sub bh,cl              ;decrement column count
		jge planeokmaskvideo

planeokmaskvideo:
		pop di
		rol al,1
		adc di,0
		shr ecx,24

		dec cx
		jnz planelopmaskvideo

		mov si,fs
        mov di,gs
        pop ds
        pop bp
        ret         ;do not forget


ENDP


;TO BE COMPLETED !!!!!!!!!!!!!!!!!!!!

;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                                 Clipvideo                               ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ clips a video bitmap within viewport coordinates                        ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: ???????????                                                    ³
;³ Input: Thru stack: TLHC - row,col,image offset                          ³
;³ Notes:      IT DON'T WORK YET                                           ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC    _clipvideo

		ARG     Left:word,Top:word,Image:dword
        
	push bp
	mov bp,sp   ;usual 
        
	push ds
	mov gs,di             ;preserver DI - C uses it
        push si

		mov ax,@data
		mov es,ax

		lds si,[image]
        
        mov bx,[Left]       ;fetch start column
        cmp bx,[es:Viewport+4]  ;is left beyond viewport?
		ja endclip

		mov dx,[Top]
		cmp dx,[es:Viewport+6]  ;is top beyond viewport?
        ja endclip

        xor cx,cx
        mov ax,[es:Viewport]    ;move left coord of viewport to ax        
        cmp bx,ax               ;check if left coord before? view port
        jae leftok
		mov cx,ax               ;copy viewport left to cx
        sub cx,bx               ;get left clip in CL
        mov bx,ax               ;change screen position to viewport coord in BX
        
leftok:
        mov ax,[es:Viewport+2]    
        cmp dx,ax               ;test if top coord is before viewport
		jae topok
		mov ch,al
        sub ch,dl               ;get top clip offset in CH
        mov dx,ax               ;stick top viewport coord in DX
topok:
        mov ax,dx               ;copy top coord back to AX
        shl ax,SHIFT1
		mov di,ax               ;copy top coord to DI
		shl ax,SHIFT2
		add di,ax               ;first part of screen offset
        add di,[es:ActiveOffset]
        
		lodsw                    ;fetch image width into ax
        mov fs,ax               ;backup true width
		sub al,cl               ;check if image width is smaller or equal
        jbe endclip             ;to left clip
        
		shl edx,24              ;shift top coord to upper EDX

		mov dx,ax               ;copy left adjusted width to DX
		add dx,bx               ;add width to left screen coord
		sub dx,[es:Viewport+4]  ;if result greater than zero, image overlaps
        ja rightview           ;get right clip in DL
        xor dx,dx               ;otherwise no clip       
rightview:        
		sub ax,dx               ;reduce width of image by value of right clip
        mov ah,al               ;shift new full adjusted width to ah
		lodsb                   ;fetch image height into al
		inc si                  ;increment SI
		sub al,ch               ;check if image height is smaller or equal
        jbe endclip             ;to top clip
        
        rol edx,8               ;shift top to DL, and right clip to DH
		add dl,al               ;test if image overlaps bottom viewport limit
        sub dl,[byte ptr es:Viewport+6]  ;get bottom clip in DL        
		ja bottomview
        xor dl,dl               ;AH has adj width, AL has adj height
bottomview:                        ;DH has right clip,DL has bottom clip
		sub al,dl               ;reduce height of image
		shl eax,16              ;shift dimensions to upper EAX
        shl ecx,16              ;shift clips to upper ECX

        mov cl,bl
		and cl,3h
        mov ah,11h
        shl ah,cl
		rol eax,16           ;store start plane in upper EAX,restore dimnsns

        shr bx,2           ;divide left column by 4
		add di,bx           ;BX is finally FREE
        
		shr ecx,16          ;restore clips to CX,ch - top,cl - left
		mov bl,cl
		shr bl,2            ;bytes for left clip
		add si,bx           ;adjust image pointer by column
		mov bx,fs           ;bring true image pixel width to BX
		xchg ax,bx          ;stick true width in AX,dimensions in BX
        shr ax,2            ;divide width by 2
        xchg cl,ch          ;swap top clip to CL,left clip to CH
        mul cl        
		add si,ax           ;adjust image pointer down by rows
		shr dh,2
		add dh,ch           ;DH now has image row inc
		mov ax,fs           ;copy true width back to AX
        shr ax,2
        add cl,dl
		mul cl              ;yucky mul gives image plane inc
        rol ebx,16
        mov bx,ax           ;store plane inc in bx
		rol ebx,16          ;stick plane inc in upper EBX
        ;yippee
        ror eax,16          ;bring plane to ah
        mov al,dh        ;store row inc in [top]
        ror eax,16          ;stick vals back, AX is FREE

        mov ax,vbase
        mov es,ax

		cld
        shr bh,2            ;divide width to get bytes                        
		xor ah,ah
        mov al,bh           ;bh has column count backup           
        neg ax
        add ax,MaxX/4
		mov bp,ax           ;bp has line inc

							;bl has row backup
		shr eax,16          ;bring mask to AH
		mov ch,al           ;temp store al in ch
        mov al,02h
        mov dx,3c4h
		out dx,al
        mov al,ah           ;restore write plane mask to al
        mov ah,cl           ;move extra bytes to ah
        shl eax,16          ;stick plane back in upper eax
		mov al,ch           ;put row inc back in al
        mov cx,4h
        mov dh,bh
        and dh,3h
        shr bh,2
        shl edx,8        
        ror eax,16          ;bring plane back
planelopclip:
		push di
		shl ecx,24
		mov dx,3c5h
		out dx,al
		shr edx,8
		mov dl,bl           ;restore row count from bl to dl
		ror eax,16
rowlopclip:

		mov cl,bh           ;restore column count from bh to cx
		rep movsd
		mov cl,dh
		rep movsb


		add si,ax           ;advance to next row
		add di,bp
		dec dl

		jnz rowlopclip
		ror eax,16
		dec ah
		setz cl
		sub dh,cl              ;decrement column count
		add bp,cx              ;increment line inc
planeokclip:
		pop di
		rol al,1
		adc di,0
		shr ecx,24
		shl edx,8
		ror ebx,16
		add si,bx
		ror ebx,16
		dec cx
		jnz planelopclip

endclip:
		pop si
		mov di,gs
		pop ds
		pop bp
		ret         ;do not forget


ENDP



;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³  Notes: for Procedure videosize                                         ³
;³  calculates the size of a buffer needed to store an image               ³
;³  given columns and rows                                                 ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC    _videosize

		ARG     Cols:word,Rows:word

	push bp
	mov bp,sp

	mov cx,[rows]
	mov ax,[cols]
	mul cx
	shr ax,2	;divide by 4
	add ax,4

	pop bp

	ret         ;do not forget
ENDP



;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³  Notes: for Procedure valloc                                            ³
;³  allocates a buffer of video memory needed to store an image            ³
;³  given columns and rows                                                 ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC    _valloc

		ARG     Buffersize:word

	push bp
	mov bp,sp

	mov dx,vbase
	mov ax,[FreeVideo]
	mov cx,[Buffersize]
	add [FreeVideo],cx

	pop bp

	ret         ;do not forget
ENDP


END ;end of it all


