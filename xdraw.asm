include d:\cppwork\asm\xdriver.inc

	CODESEG

	EXTRN ActiveOffset

	PUBLIC _putpixel,_getpixel,_horizline,_vertline
	PUBLIC _drawrect,_solidrect,_line



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

	mov ax,@data
	mov es,ax

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
	add bx,[es:ActiveOffset]

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

	mov ax,@data
	mov es,ax

        
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
		add bx,[es:ActiveOffset]   ;add column to produce offset

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

		mov ax,@data
		mov es,ax

		mov bx,[StartCol]           ;stick startcol in BX
		mov cl,3h
		and cl,bl                   ;store startplane value in cl
		shr bx,2                    ;BX has start byte

		mov ax,[Row]
		shl ax,SHIFT1               ;multiply row by 64
		mov di,ax
		shl ax,SHIFT2               ;multiply row by 16
		add di,ax               ;add to multiply by 80 for result
		add di,[es:ActiveOffset]
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

		mov ax,@data
		mov es,ax

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
		add di,[es:ActiveOffset]
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
            
	mov ax,@data
	mov es,ax

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
	add dx,[es:ActiveOffset]

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

		mov ax,@data
		mov es,ax
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
		add di,[es:ActiveOffset]
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

		mov ax,@data
		mov es,ax

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
		add di,[es:ActiveOffset]

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


END
