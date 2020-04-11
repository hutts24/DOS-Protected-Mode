%TITLE "VGA Driver - 15/1/94"

;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³       A series of procedures for using VGA mode 13h and C++       ³
;³           Written by Mr Hutts & Cergy Cranger (c)1994             ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

include xdriver.inc
include protmode.inc

ProtCode SEGMENT PARA PUBLIC USE32 'CODE'
    ASSUME cs:ProtCode,ds:ProtCode

;Important local variables values

	ViewPort dw 0,0,319,191
	ActiveOffset dd 0       ;physical offsets within videomemory
	VisualOffset dd 0
	EndSplitScreen dd 0a0000h	;start address of video memory after splitscreen
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
	PUBLIC	graphicsmode,returntextmode,clearvideomem,clearscreen
	PUBLIC	setview, clearviewport, setactivepage, setvisualpage
	PUBLIC  virtualwidth, set320x240, startsplitscreen, setsplitscreen
	PUBLIC 	shiftscreen, _copyvideo, retrace, _startvsync, _stopvsync

;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                               GraphicsMode                              ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes Video Mode to 320x200x256 (13h).                                ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: AX,DX and certain vga card registers                           ³
;³ Input: None                                                             ³
;³ Output: exit code 255 if vga not present                                ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

graphicsmode PROC

	pushad      ;don't trust bios
    
        mov ax,1A00h ; Test for presence of MCGA
	mov bp,10h      ; using Video BIOS int
	int 30h
        cmp al,1Ah   ; If al=1A then we have MCGA!
	jnz Exit

	mov ax,Mode  ;Move desired video mode to al
	int 30h      ;Do it - changes graphics mode
	mov dx,03c4h ;index register for sequence controller on card

	mov ax,0604h ;turn of bit 3 at sequence controller register 4
	out dx,ax
	mov dx,03d4h ;index register for crtc controller

	mov ax,0014h  ;turn off underline register, index 14
	out dx,ax    ;done, now in xmode!


	mov ax,0e317h ;index register 17 for mode control register
	out dx,ax

	popad

	ret
graphicsmode ENDP



;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                               Retrace	                           ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Wait for vertical retrace to begin		                           ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: al								   ³
;³ Input: None                                                             ³
;³ Output: bugger all						           ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

PROC	retrace
        push eax
        push edx

	mov dx,3dah
vrw1:
	in al,dx
	test al,08h
	jz vrw1    		;poll while bit is 0
vrw2:
	in al,dx
	test al,08h
	jnz vrw2        ;poll while bit is 1

        pop edx
        pop eax
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

virtualwidth PROC
        
        push eax
        push edx
        
        mov dx,3d4h         ;crtc index
	mov al,13h
	mov ah,MaxX/8
	out dx,ax           ;set virtual width in card register
        
        pop edx
        pop eax
        ret
virtualwidth ENDP



;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                               set320x240                                ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes Video screen resolution to 320 x 240                            ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: AX,DX and certain vga card registers                           ³
;³ Input: None                                                             ³
;³ Output: Bugger All                                                      ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

set320x240 PROC

        push eax
        push edx

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

        pop edx
        pop eax
	ret
set320x240 ENDP



;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                               StartSplitScreen                          ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ starts a split screen						                           ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: AX,DX and certain vga card registers                           ³
;³ Input: None                                                             ³
;³ Output: Bugger All                                                      ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

startsplitscreen PROC

        push eax
        push edx	

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

	mov [EndSplitScreen],0a0000h+MaxX/4*SplitHeight

        pop edx
        pop eax
	ret

startsplitscreen ENDP




;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                               SetSplitScreen      	                   ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ changes split screen	starting line on visible screen                    ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: AX,DX and certain vga card registers                           ³
;³ Input: startline in AX                                                  ³
;³ Output: Bugger All                                                      ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

setsplitscreen PROC     ;startline in AX register

        push eax
        push ebx
        push ecx
        push edx

	mov bx,ax
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

	pop edx
        pop ecx
        pop ebx
        pop eax
    	ret

setsplitvsync:

	mov [SplitScreenFlag],0
	mov [NewSplitByte1],ah
	mov [NewSplitByte2],cl
	mov [NewSplitByte3],ch
	mov [SplitScreenFlag],1

	pop edx
        pop ecx
        pop ebx
        pop eax
	ret

setsplitscreen ENDP




;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                                 TextMode                                ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes Video Mode to 80x25 Text (03h).                                 ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: AX                                                             ³
;³ Input: None                                                             ³
;³ Output: None                                                            ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
returntextmode PROC

	push eax
        push ebp

        xor eax,eax     ;Use BIOS to change mode
	mov al,3        ;Mode is 3 for 80x25
	mov ebp,10h  
	int 30h         ;do it

        pop ebp
        pop eax
        ret

returntextmode ENDP




;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                             ClearVideoMem                               ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´ 
;³ Clears the buffer.                                                      ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´ 
;³ Changes: AX,BX,CX,DX,DI,ES                                              ³
;³ Notes: Clears all 4 screens of video memory                             ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
clearvideomem PROC

	push eax
        push ecx
        push edx
        push edi

	mov dx,03c4h          ;sequence controller index
	mov ax,0f02h          ;set write plane register to all, index 2
	out dx,ax             ;output to card registers
	mov edi,0a0000h
        mov ecx,64000         ;Set count to no. of dwords in buffer
        xor eax,eax           ;Set EAX to 0 - value which goes to memory
	cld
        rep stosd
        	
        pop edi
        pop edx
        pop ecx
        pop eax
        ret
clearvideomem ENDP



;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                             ClearScreen                                 ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´ 
;³ Clears the buffer.                                                      ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´ 
;³ Changes: AX,BX,CX,DX,DI,ES                                              ³
;³ Notes: Clears active screen                                             ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
clearscreen PROC

    push eax
    push ecx
    push edx
    push edi

    mov dx,03c4h              ;sequence controller index
    mov ax,0f02h               ;set write plane register to all, index 2
    out dx,ax                 ;output to card registers
    mov edi,[ActiveOffset]
    mov ecx,MaxX/16*MaxY       ;Set count to no. of dwords in scree
    xor eax,eax                ;Set ax to 0 - value which goes to memory
    cld
    rep stosd

    pop edi
    pop edx
    pop ecx
    pop eax
    ret
 
clearscreen ENDP



;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                                SetActivePage                            ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ sets active page memory                                                 ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: AX,CX                                                          ³
;³ Output: none                                                            ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
setactivepage PROC      ;Page Number in cx

        push eax
        push ecx
        
        mov eax,0a0000h
	cmp cx,-1                 ;-1 for splitscreen
	jz split
	mov eax,MaxX/4*MaxY          ;number of bytes per page
	mul cx                   ;calculate offset for desiredpage
	add eax,[EndSplitScreen]
split:
	mov [ActiveOffset],eax       ;change new value

        pop ecx
        pop eax
	ret
setactivepage ENDP




;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                                SetVisualPage                            ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ sets visual page memory                                                 ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: AX,CX                                                          ³
;³ Output: none                                                            ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC    setvisualpage   ;Page Number in CX,Row in DX,Column in BX

        push eax
        push ebx
        push ecx
        push edx

	mov eax,MaxX/4*MaxY          ;number of bytes per page
	mul cx                   ;calculate offset for desiredpage

	shl dx,SHIFT1
	add ax,dx
	shl dx,SHIFT2
	add ax,dx

	mov cl,bl
	and cl,3h
	shl cl,1

	shr bx,2
	add ax,bx
	add eax,[EndSplitScreen]

	mov [VisualOffset],eax       ;change new value

	mov bx,ax               ;save value in bx
	shr ax,8
	mov ah,al
	mov al,0ch              ;low byte in ax
	push eax               ;;

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
	pop eax              ;;


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

	pop edx
        pop ecx
        pop ebx
        pop eax
	ret

setvisvsync:

	mov [NewLowByte],ax
	mov [NewHighByte],bx
	mov [NewPelPan],cl

waitlast:
	cmp [VisualFlag],0
	jne waitlast

	mov [VisualFlag],1

        pop edx
        pop ecx
        pop ebx
        pop eax
	ret
ENDP






;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³  Notes: for Procedure SetView                                           ³
;³  Sets the co-ordinates of the active clipping viewport (images only)    ³
;³  Changes local variables defined in data segment                        ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC    setview     ;Left in AX,Top in BX,Right in CX,Bottom in DX

	mov [Viewport],ax
	mov [Viewport+2],bx
	mov [Viewport+4],cx
	mov [Viewport+6],dx
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
PROC	clearviewport

        push eax
        push ebx
        push ecx
        push edx
        push edi
        push esi

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
	movzx edi,ax
	shl ax,SHIFT2
	add di,ax
	add di,bx           ;add column
	add edi,[ActiveOffset]

	mov si,[Viewport+6]
	sub si,dx
	inc si              ;BX , or rather, bl has row count

	cld

	mov edx,MaxX/4
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
	add edi,edx
	dec si
	jnz viewrowlop

        pop esi
        pop edi
        pop edx
        pop ecx
        pop ebx
        pop eax
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
PROC	shiftscreen ;horiz in BX,vertical in CX

        pushad

	mov dx,3c4h
	mov ax,0ff02h
	out dx,ax 		;set to write to all planes

	mov dx,3ceh		;read port
	mov ax,08h		;set all reads to latches
	out dx,ax

	cld				;clear direction flag

	mov edi,[ActiveOffset]
	mov esi,edi

	movsx eax,bx
	sar eax,2
	mov ebx,eax
	sub esi,eax
	neg eax

	test ebx,10000000000000000000000000000000b	;test if signed
	jnz shiftleft

	neg ebx
	add edi,(MaxX/4)-1	 ;offset to end of line
	add esi,(MaxX/4)-1
	add eax,MaxX/2   ;if scanning backwards,will jump down 2 lines
	std				;assuming scanning downwards

shiftleft:
	add ebx,MaxX/4	;bx has column count,don't touch it

	mov dx,cx
	sal cx,SHIFT1
	movzx ebp,cx
	sal cx,SHIFT2
	add bp,cx
	sub esi,ebp

	test dx,1000000000000000b	;test if negative
	jnz shiftup

	neg dx
	add edi,(MaxX/4)*(MaxY-1)
	add esi,(MaxX/4)*(MaxY-1)

	sub eax,MaxX/2

shiftup:
	add dx,Maxy		;dx has row count

	mov cx,GlobalSelector
        mov ds,cx		;set es and ds to global segment

shiftloop:
	mov ecx,ebx		;move how many bytes of video memory
	rep movsb
	add edi,eax		;increment by ax bytes to advance to next row
	add esi,eax
	dec dx
	jnz shiftloop

	mov dx,3ceh
	mov ax,0ff08h
	out dx,ax

        popad
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
PROC	_copyvideo  ;AL:screen1,BX:s1x1,CX:s1y1,BP:s1x2,DX:s1y2,AH:screen2,SI:s2x,DI:s2y

	ARG     screen1:word,s1x1:word,s1y1:word,s1x2:word,s1y2:word,screen2:word,s2x1:word,s2y1:word

        pushad
                
	mov dx,3ceh		;read port
	mov ax,08h		;set all reads to latches
	out dx,ax

	mov dx,3c4h
	mov ax,0ff02h
	out dx,ax 		;set to write to all planes

	cld				;clear direction flag

	mov eax,MaxX/4*MaxY          ;number of bytes per page
	movzx cx,ch    
        mul cx                   ;calculate offset for desiredpage

	shl di,SHIFT1
	add ax,di
	shl di,SHIFT2
	add ax,di

	shr si,2
	add ax,si
	add eax,[EndSplitScreen]
	mov edi,eax               ;di has destination offset

	movzx cx,[sp]
	mov eax,MaxX/4*MaxY          ;number of bytes per page
	mul cx                   ;calculate offset for desiredpage

        mov dx,[sp+4]
	mov cx,dx               ;save row in CX
	shl dx,SHIFT1
	add ax,dx
	shl dx,SHIFT2
	add ax,dx
	
	mov dx,[sp+8]
	sub dx,cx
	inc dx              ;DX has number of rows to copy

	shr bx,2
	add ax,bx
	add eax,[EndSplitScreen]
        mov esi,eax           ;si has source index

        shr bp,2            ;get number of bytes wide to copy
	sub bp,cx
        inc bp

        mov ax,MaxX/4
	sub ax,bp           ;AX now has incremental difference

copyvidloop:
	mov cx,bp		;move how many bytes of video memory
	rep movsb
	add di,ax		;increment by ax bytes to advance to next row
	add si,ax
	dec dx                  ;decrement row count
	jnz copyvidloop

	mov dx,3ceh
	mov ax,0ff08h
	out dx,ax

	popad
	ret
ENDP



;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                               Vsync_int                                 ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ This is the vertical retrace synchronizer interrupt blah blah blah	   ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: about everything                                               ³
;³ Input: number of retraces to skip with each frame	                   ³
;³ Notes: Must remove it with stopvsync to prevent screwing up system      ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

PROC vsync_int

	pushad	;save all general purpose registers
	push ds ;save data segment

	mov ax,ProtCode
        mov ds,ax
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
	OldTimer dd ?
        dw ProtCode 

nochain:
	mov al,20h
	out 20h,al

	pop ds
	popad
	sti
	iretd

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
;³ Input: number of retraces to skip with each frame		           ³
;³ Notes: Must remove it with stopvsync to prevent screwing up system      ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC	_startvsync AX:Retraces to skip

        push eax
        push ebx
        push ecx
        push edx

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
	mov eax,20h	    ;vector for get timer interrupt address
	call GetVector
	mov [word ptr es:OldTimer],eax       ;Store in OldTimerInt
	
	mov eax,Offset Vsync_int	;vector address for set interrupt
	mov ebx,20h
	call SetVector           ;set new timer interrupt

	mov     al,34h                   ;Reprogram timer 0
	out     43h,al
	mov     ax,cx
	out     40h,al
	mov     al,ah
	out     40h,al
	sti

	mov [VsyncActive],1

	ret
ENDP

;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                               StopVsync                                 ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Stops the vertical retrace synchronizer interrupt blah blah blah	   ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: about everything                                               ³
;³ Input: number of retraces to skip with each frame		           ³
;³ Notes: Must remove it with stopvsync to prevent screwing up system      ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC _stopvsync

	push eax
        push ebx

        mov eax,[OldTimer]
	mov ebx,20h       ;Restore the old timer int
	call SetVector
        
        cli
	int     21h
	mov     al,34h                   ;Restore timer 0
	out     43h,al
	mov     al,0
	out     40h,al
	out     40h,al
	sti

        pop ebx
        pop eax

	ret
ENDP

ProtCode ENDS

END