include d:\cppwork\asm\xdriver.inc

	CODESEG

	EXTRN	ActiveOffset, ViewPort, EndSplitScreen, FreeVideo

	PUBLIC  _getvideo, _putvideo, _clipvideo
	PUBLIC	_videosize, _maskvideo, _setfreevideo, _valloc

;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                                 SetFreeVideo                            ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ sets up the starting offset for free video storage area                 ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Notes: run once after setting up video display       		   ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC	_setfreevideo		;sets up offset for free video memory

	mov ax,@data
	mov es,ax

	mov ax,[es:EndSplitScreen]
	add ax,MaxX*MaxY
	mov [es:FreeVideo],ax
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

	mov ax,@data
	mov es,ax

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
	add si,[es:ActiveOffset]
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

rowloopget:
	mov cx,bx           ;restore byte count from bh to cx
	rep movsb
	add si,ax           ;increment to next line
	dec dx
	jnz rowloopget

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

	mov ax,@data
	mov es,ax

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
	add di,[es:ActiveOffset]
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


rowlop2:

	mov cx,bx           ;restore column count from bh to cx
	rep movsb
	add di,ax
	dec dx
	jnz rowlop2

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

		mov ax,@data
		mov es,ax

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
		add di,[es:ActiveOffset]
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

planelopmask:
		push di
		shl ecx,24
		mov dx,3c5h
		out dx,al
		mov dl,bl           ;restore row count from bl to dl
		shl eax,8           ;vacate al
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
		dec dl
		jnz rowlopmask

		shr eax,8               ;reoccupy al
		dec ah
		setz cl
		add bp,cx              ;increment line inc
		sub bh,cl              ;decrement column count
		jge planeokmask

planeokmask:
		pop di
		rol al,1
		adc di,0
		shr ecx,24

		dec cx
		jnz planelopmask

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

	mov ax,@data
	mov es,ax

	mov dx,vbase
	mov ax,[es:FreeVideo]
	mov cx,[Buffersize]
	add [es:FreeVideo],cx

	pop bp

	ret         ;do not forget
ENDP




END
