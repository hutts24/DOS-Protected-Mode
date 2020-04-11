include d:\cppwork\asm\xdriver.inc

	CODESEG

	EXTRN	ActiveOffset, ViewPort

	PUBLIC  _getimage, _putimage, _clipimagex4, _tileimage
	PUBLIC	_imagesize, _maskimage, _putimagex4, _clipmaskimagex4
	PUBLIC  _freadpcx

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

		mov ax,@data
		mov es,ax

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
		add si,[es:ActiveOffset]
                
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

		mov ax,@data
		mov es,ax

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
		add di,[es:ActiveOffset]
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

		mov ax,@data
		mov es,ax

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
		add di,[es:ActiveOffset]
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

		mov ax,@data
		mov es,ax

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
		add di,[es:ActiveOffset]
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

	mov ax,@data
	mov es,ax

	mov di,[es:ActiveOffset]
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
shiftloop:
	shr [byte ptr ds:bx],2
	inc bx
	loop shiftloop  ;divide all bytes by 4

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




END
