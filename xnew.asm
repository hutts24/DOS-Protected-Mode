;%TITLE "VGA Driver - 15/1/94"

;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³       A series of procedures for using VGA mode 13h and C++       ³
;³           Written by Mr Hutts & Cergy Cranger (c)1994             ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

Mode = 13h
MaxX = 320
MaxY = 192
SplitStart = 192
SplitHeight = 48
vBase = 1bh
Shift1 = 4
Shift2 = 2

.386P

Stack SEGMENT BYTE STACK USE32 'STACK'
       db  8192 dup (?)
Stack ENDS

DriverDataSeg SEGMENT BYTE USE32 'data'

	ViewPort dw 0,0,319,191
	ActiveOffset dw 0       ;physical offsets within videomemory
	VisualOffset dw 0
	EndSplitScreen dw 0             ;start address of video memory after splitscreen
	FreeVideo dw 0                  ;offset of video memory free for bitmap storage

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
	NewLowPal dw 255                ;lower limit of palette change
	NewHighPal dw 0         ;upper limit of palette change

DriverDataSeg ENDS


TheCode SEGMENT BYTE PUBLIC USE32 'code'

	ASSUME cs:TheCode,ds:DriverDataSeg



    jmp START
    

;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                               GraphicsMode                              ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes Video Mode to 320x200x256 (13h).                                ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: AX,DX and certain vga card registers                           ³
;³ Input: None                                                             ³
;³ Output: exit code 255 if vga not present                                ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

_graphicsmode PROC

	mov ax,1A00h ; Test for presence of MCGA
	int 10h      ; using Video BIOS int
	cmp al,1Ah   ; If al=1A then we have MCGA!
	jz OK        ;Jump to OK if test successful
	mov ax,4CFFh ;Otherwise terminate program and
	int 21h      ;Exit to DOS
OK:
	;push si
	;push di      ;preserve registers for silly C


	;pop di
	;pop si

	ret
_graphicsmode ENDP



;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                               Retrace                                       ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Wait for vertical retrace to begin                                          ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: al                                                                                             ³
;³ Input: None                                                             ³
;³ Output: bugger all                                                                          ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

_retrace PROC

	mov dx,3dah
vrw1:
	in al,dx
	test al,08h
	jz vrw1                 ;poll while bit is 0
vrw2:
	in al,dx
	test al,08h
	jnz vrw2        ;poll while bit is 1
	ret             ;now it has begun :)
_retrace ENDP


;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                               VirtualWidth                              ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes Video Virtual screen size to MAXX                               ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: AX,DX and certain vga card registers                           ³
;³ Input: None                                                             ³
;³ Output: Bugger All                                                      ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

_virtualwidth PROC

		mov dx,3d4h         ;crtc index
		mov al,13h
		mov ah,MaxX/8
		out dx,ax           ;set virtual width in card register
	ret
_virtualwidth ENDP



;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                               set320x240                                ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes Video screen resolution to 320 x 240                            ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: AX,DX and certain vga card registers                           ³
;³ Input: None                                                             ³
;³ Output: Bugger All                                                      ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

_set320x240 PROC

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
_set320x240 ENDP



;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                               StartSplitScreen                          ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ starts a split screen                                                                           ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: AX,DX and certain vga card registers                           ³
;³ Input: None                                                             ³
;³ Output: Bugger All                                                      ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

_startsplitscreen PROC

	mov dx,03dah
	in al,dx

	mov al,10h+20h
	mov dx,03c0h
	out dx,al                                       ;set flipflop?

	inc dx
	in al,dx
	or al,20h                                       ;suppress split screen panning
	dec dx
	out dx,al                                       ;write new mode to register

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

	mov [ds:EndSplitScreen],MaxX/4*SplitHeight

	ret

_startsplitscreen ENDP




;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                               SetSplitScreen                            ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ changes split screen starting line on visible screen                    ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: AX,DX and certain vga card registers                           ³
;³ Input: None                                                             ³
;³ Output: Bugger All                                                      ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

_setsplitscreen PROC

	push bp
	mov bp,sp       ;aaargggghhh

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

_setsplitscreen ENDP




;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                                 TextMode                                ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes Video Mode to 80x25 Text (03h).                                 ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: AX                                                             ³
;³ Input: None                                                             ³
;³ Output: None                                                            ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
_returntextmode PROC

	xor ah,ah    ;Use BIOS to change mode
	mov al,3     ;Mode is 3 for 80x25
	int 10h      ;Do it
	ret

_returntextmode ENDP




;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                             ClearVideoMem                               ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´ 
;³ Clears the buffer.                                                      ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´ 
;³ Changes: AX,BX,CX,DX,DI,ES                                              ³
;³ Notes: Clears all 4 screens of video memory                             ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
_clearvideomem PROC

	mov ebx,edi                 ;store di in bx
	mov dx,03c4h          ;sequence controller index
	mov ax,0f02h          ;set write plane register to all, index 2
	out dx,ax             ;output to card registers
	mov ax,vbase          ;Put the segment of video buffer into
	mov es,ax             ;ES for use in string instructions
	xor edi,edi
	mov ecx,64000          ;Set count to no. of dwords in buffer
	xor eax,eax                ;Set EAX to 0 - value which goes to memory
	cld
	rep stosd
	mov edi,ebx
	ret
_clearvideomem ENDP



;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                             ClearScreen                                 ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´ 
;³ Clears the buffer.                                                      ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´ 
;³ Changes: AX,BX,CX,DX,DI,ES                                              ³
;³ Notes: Clears active screen                                             ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
_clearscreen PROC

	mov bx,di                 ;store di in bx
		mov dx,03c4h              ;sequence controller index
		mov ax,0f02h               ;set write plane register to all, index 2
		out dx,ax                 ;output to card registers
		mov di,[ActiveOffset]
		mov ax,vbase              ;Put the segment of video buffer into
		mov es,ax                 ;ES for use in string instructions
		mov cx,MaxX/16*MaxY                     ;Set count to no. of dwords in scree
		xor eax,eax                ;Set ax to 0 - value which goes to memory
		cld
		rep stosd
		mov di,bx
		ret
_clearscreen ENDP



;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                                SetActivePage                            ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ sets active page memory                                                 ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: AX,CX                                                          ³
;³ Output: none                                                            ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
_setactivepage PROC

     

	push bp                   ;preserve BP
	mov bp,sp                 ;let the compiler get args

	xor ax,ax
      
	cmp cx,-1                 ;-1 for splitscreen
	jz split
	mov ax,MaxX/4*MaxY          ;number of bytes per page
	mul cx                   ;calculate offset for desiredpage
	add ax,[EndSplitScreen]
split:
	mov [ActiveOffset],ax       ;change new value

	pop bp
	ret
_setactivepage ENDP




;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                                SetVisualPage                            ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ sets visual page memory                                                 ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: AX,CX                                                          ³
;³ Output: none                                                            ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
_setvisualpage PROC

	push bp                   ;preserve BP
	mov bp,sp                 ;let the compiler get args

	mov cx,[bp+6]
	mov ax,MaxX/4*MaxY          ;number of bytes per page
	mul cx                   ;calculate offset for desiredpage

	mov dx,[bp+2]
	shl dx,SHIFT1
	add ax,dx
	shl dx,SHIFT2
	add ax,dx

	mov bx,[bp+4]
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
_setvisualpage ENDP










;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                                PutPixel                                 ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Writes a pixel at a specified point with a specified colour.            ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: AX,BX,CX,DX,ES                                                 ³
;³ Input: Parameters passed through stack in this order: Colour,Row,Col    ³
;³ Output: None                                                            ³
;³ Notes: Writes to temporary buffer. Use WriteBuffer to show changes.     ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
_putpixel PROC

	;ARG Col:word,Row:word,Colour:word ;Arguments for this proc are
					  ;passed in style usable by C

	push bp                  ;Preserve Stack Base Pointer
	mov bp,sp                ;Move base pointer. TASM does some
	
	xor ebx,ebx
	mov bx,[bp+6]
	mov cl,3h               ;logical and column with 3 to determine plane
	and cl,bl
	mov ax,0102h            ;02 is index for write plane register
	shl ah,cl               ;shl 1 by cl for register value for write plane
	mov dx,03c4h             ;sequence controller index register
	out dx,ax               ;set write plane register
	mov ax,[bp+4]

	shr bx,2
	shl ax,SHIFT1           ;multiply row by 16
	add bx,ax
	shl ax,SHIFT2           ;multiply row by 64
	add bx,ax
	add bx,[ActiveOffset]

	mov ax,vbase             ;Move segment address of temp buffer
	mov es,ax                ;To ES for use of ES:BX

	mov ax,[bp+2]          ;mov colour to set pixel to into AL
	mov es:[ebx],al  ;change video memory

	pop bp                   ;put back bp

	ret                     ;remove 6 bytes from stack (variables)
_putpixel ENDP


	
START:

    call _graphicsmode

    ;call _clearvideomem
	     
    mov ax,4c00h
    int 21h




TheCode ENDS ;end of it all

END
