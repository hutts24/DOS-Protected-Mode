include d:\cppwork\asm\xdriver.inc

;;;;;;;;;;;;;; Font - Belwe Bold...or close to it anyway ;;;;;;;;;;;;;


DATASEG

LABEL   Fonts8x8   byte

Space   db  00000000b
		db  00000000b
		db  00000000b
		db  00000000b
		db  00000000b
		db  00000000b
		db  00000000b
		db  00000000b

Exclmk  db  00000000b
		db  00011000b
		db  00111100b
		db  00111100b
		db  00011000b
		db  00011000b
		db  00000000b
		db  00011000b

Dblqt   db  00000000b
		db  00010100b
		db  00010100b
		db  00101000b
		db  00000000b
		db  00000000b
		db  00000000b
		db  00000000b

Hash    db  00000000b
		db  00100100b
		db  01111110b
		db  00100100b
		db  00100100b
		db  01111110b
		db  00100100b
		db  00000000b

Dollar  db  00000000b
		db  00011000b
		db  00111110b
		db  01011000b
		db  00111100b
		db  00011010b
		db  01111100b
		db  00011000b

Percent db  00000000b
		db  01100010b
		db  01100100b
		db  00001000b
		db  00010000b
		db  00100110b
		db  01000110b
		db  10000000b

Amprsnd db  00000000b
		db  00110000b
&       db  01001000b
		db  00110000b
		db  00110010b
		db  01001100b
		db  10001100b
		db  01110010b

CloseQt db  00000000b
		db  00011000b
		db  00011000b
		db  00110000b
		db  00000000b
		db  00000000b
		db  00000000b
		db  00000000b

LftBrkt db  00000000b
		db  00011100b
		db  00100000b
		db  01000000b
		db  01000000b
		db  01000000b
		db  00100000b
		db  00011100b

RhtBrkt db  00000000b
		db  00111000b
		db  00000100b
		db  00000010b
		db  00000010b
		db  00000010b
		db  00000100b
		db  00111000b

Astersk db  00000000b
		db  01000100b
		db  00101000b
		db  11111110b
		db  00101000b
		db  01000100b
		db  00000000b
		db  00000000b

Plus    db  00000000b
		db  00000000b
		db  00010000b
		db  00010000b
		db  01111100b
		db  00010000b
		db  00010000b
		db  00000000b

Comma   db  00000000b
		db  00000000b
		db  00000000b
		db  00000000b
		db  00000000b
		db  00011000b
		db  00011000b
		db  00110000b

Minus   db  00000000b
		db  00000000b
		db  00000000b
		db  00000000b
		db  01111100b
		db  00000000b
		db  00000000b
		db  00000000b

Period  db  00000000b
		db  00000000b
		db  00000000b
		db  00000000b
		db  00000000b
		db  00000000b
		db  00011000b
		db  00011000b

FwdSlsh db  00000000b
		db  00000010b
		db  00000110b
		db  00001100b
		db  00011000b
		db  00110000b
		db  01100000b
		db  11000000b

Zero    db  00000000b
		db  00111000b
		db  01100100b
		db  10100010b
		db  10100010b
		db  10100010b
		db  01100100b
		db  00111000b

One     db  00000000b
		db  00001100b
		db  00110100b
		db  00010100b
		db  00010100b
		db  00010100b
		db  00010100b
		db  00111110b

Two     db  00000000b
		db  00111100b
		db  01000010b
		db  00001100b
		db  00011000b
		db  00110010b
		db  01100010b
		db  01111110b

Three   db  00000000b
		db  00111100b
		db  01001010b
		db  00001010b
		db  00011010b
		db  00001010b
		db  01001010b
		db  00111100b

Four    db  00000000b
		db  00010100b
		db  00101100b
		db  01001100b
		db  11111110b
		db  00001100b
		db  00001100b
		db  00011110b

Five    db  00000000b
		db  00011110b
		db  00100000b
		db  01000000b
		db  01111100b
		db  00001010b
		db  10001010b
		db  01111100b

Six     db  00000000b
		db  00111100b
		db  01100010b
		db  10100000b
		db  10111100b
		db  10100010b
		db  10100010b
		db  01111100b

Seven   db  00000000b
		db  11111110b
		db  00001010b
		db  00010100b
		db  00101000b
		db  01010000b
		db  10100000b
		db  11110000b

Eight   db  00000000b
		db  00111100b
		db  01100010b
		db  01100010b
		db  00111100b
		db  01011110b
		db  01000110b
		db  00111100b

Nine    db  00000000b
		db  00111100b
		db  01001010b
		db  01001010b
		db  00111010b
		db  00001010b
		db  00001010b
		db  00111100b

Colon   db  00000000b
		db  00000000b
		db  00011000b
		db  00011000b
		db  00000000b
		db  00011000b
		db  00011000b
		db  00000000b

SemColn db  00000000b
		db  00000000b
		db  00011000b
		db  00011000b
		db  00000000b
		db  00011000b
		db  00011000b
		db  00110000b

Lesthan db  00000000b
		db  00001100b
		db  00011000b
		db  00110000b
		db  01100000b
		db  00110000b
		db  00011000b
		db  00001100b

Equals  db  00000000b
		db  00000000b
		db  00000000b
		db  01111110b
		db  00000000b
		db  01111110b
		db  00000000b
		db  00000000b

Grtthan db  00000000b
		db  01100000b
		db  00110000b
		db  00011000b
		db  00001100b
		db  00011000b
		db  00110000b
		db  01100000b

Questn  db  00000000b
		db  00011100b
		db  00100010b
		db  00000110b
		db  00011000b
		db  00000000b
		db  00011000b
		db  00011000b

Atsign  db  00000000b
		db  01111100b
		db  10000010b
		db  10011010b
		db  10101010b
		db  10111110b
		db  10000000b
		db  01111100b

A       db  10101010b
		db  01010101b
		db  10101010b
		db  01010101b
		db  10101010b
		db  01010101b
		db  10101010b
		db  01010101b

B       db  00000000b
		db  11111000b
		db  01101100b
		db  01101100b
		db  01111100b
		db  01100110b
		db  01100110b
		db  11111100b

C       db  00000000b
		db  01111100b
		db  10000010b
		db  10000100b
		db  11000000b
		db  11100000b
		db  11111110b
		db  01111100b

D       db  00000000b
		db  11111100b
		db  01100110b
		db  01100110b
		db  01100110b
		db  01100110b
		db  01100110b
		db  11111100b

E       db  00000000b
		db  11111110b
		db  01100010b
		db  01100000b
		db  01111110b
		db  01100000b
		db  01100010b
		db  11111110b

F       db  00000000b
		db  11111110b
		db  01100010b
		db  01100000b
		db  01111110b
		db  01100000b
		db  01100000b
		db  11110000b

G       db  00000000b
		db  01111100b
		db  10000010b
		db  10000000b
		db  10011110b
		db  11001100b
		db  11101100b
		db  01111010b

H       db  00000000b
		db  11101110b
		db  11000110b
		db  11000110b
		db  11111110b
		db  11000110b
		db  11000110b
		db  11101110b

I       db  00000000b
		db  00111110b
		db  00011100b
		db  00011100b
		db  00011100b
		db  00011100b
		db  00011100b
		db  00111110b

J       db  00000000b
		db  00111110b
		db  00011100b
		db  00011100b
		db  00011100b
		db  00011110b
		db  00011100b
		db  00110000b

K       db  00000000b
		db  11100010b
		db  01000110b
		db  01001000b
		db  01110000b
		db  01011000b
		db  01001100b
		db  11100110b

L       db  00000000b
		db  11110000b
		db  01100000b
		db  01100000b
		db  01100000b
		db  01100000b
		db  01100010b
		db  11111100b

M       db  00000000b
		db  11000010b
		db  11100110b
		db  11011010b
		db  10010010b
		db  10000010b
		db  10000010b
		db  11000110b

N       db  00000000b
		db  11000110b
		db  11100010b
		db  11010010b
		db  10010110b
		db  10001110b
		db  10000110b
		db  11001110b

O       db  00000000b
		db  01111100b
		db  10001110b
		db  10000110b
		db  10000010b
		db  11000010b
		db  11100010b
		db  01111100b

P       db  00000000b
		db  11111100b
		db  01100110b
		db  01100110b
		db  01111100b
		db  01100000b
		db  01100000b
		db  11110000b

Q       db  00000000b
		db  01111100b
		db  10001110b
		db  10000110b
		db  11000010b
		db  11100010b
		db  01111100b
		db  00000110b

R       db  00000000b
		db  11111100b
		db  01100010b
		db  01100010b
		db  01111100b
		db  01101100b
		db  01100110b
		db  11110110b

S       db  00000000b
		db  00111110b
		db  01000010b
		db  00110000b
		db  00011000b
		db  00001100b
		db  01000010b
		db  00111100b

T       db  00000000b
		db  11111110b
		db  10011010b
		db  00011000b
		db  00011000b
		db  00011000b
		db  00011000b
		db  00111100b

U       db  00000000b
		db  11101110b
		db  01100110b
		db  01100110b
		db  01100110b
		db  01100110b
		db  01100110b
		db  00111100b

V       db  00000000b
		db  11101110b
		db  01100010b
		db  01100010b
		db  01100100b
		db  00110100b
		db  00011000b
		db  00011000b

W       db  00000000b
		db  11010110b
		db  10010010b
		db  10010010b
		db  10010010b
		db  10010010b
		db  10111010b
		db  11101110b

X       db  00000000b
		db  10000010b
		db  11000110b
		db  01101000b
		db  00110000b
		db  00111000b
		db  01001100b
		db  11000110b

Y       db  00000000b
		db  11100110b
		db  01000010b
		db  00100100b
		db  00011000b
		db  00010000b
		db  00100000b
		db  01111100b

Z       db  00000000b
		db  11111110b
		db  10000110b
		db  00001100b
		db  00011000b
		db  00110000b
		db  01100010b
		db  11111110b

LSqrBkt db  00000000b
		db  01111110b
		db  01100000b
		db  01100000b
		db  01100000b
		db  01100000b
		db  01100000b
		db  01111110b

BkSlsh  db  00000000b
		db  10000000b
		db  11000000b
		db  01100000b
		db  00110000b
		db  00011000b
		db  00001100b
		db  00000110b

RSqrBkt db  00000000b
		db  01111110b
		db  00000110b
		db  00000110b
		db  00000110b
		db  00000110b
		db  00000110b
		db  01111110b

Carat   db  00000000b
		db  00011000b
		db  00100100b
		db  01000010b
		db  00000000b
		db  00000000b
		db  00000000b
		db  00000000b

UndrScr db  00000000b
		db  00000000b
		db  00000000b
		db  00000000b
		db  00000000b
		db  00000000b
		db  00000000b
		db  11111110b

OpenQt  db  00000000b
		db  00011000b
		db  00011000b
		db  00001100b
		db  00000000b
		db  00000000b
		db  00000000b
		db  00000000b

a_      db  00000000b
		db  00000000b
		db  00000000b
		db  01111000b
		db  00001100b
		db  01111100b
		db  10001100b
		db  01111110b

b_      db  00000000b
		db  00100000b
		db  11100000b
		db  01100000b
		db  01111100b
		db  01100110b
		db  01100110b
		db  01111000b

c_      db  00000000b
		db  00000000b
		db  00000000b
		db  01111010b
		db  10001100b
		db  11000000b
		db  11100010b
		db  01111100b

d_      db  00000000b
		db  00000010b
		db  00000100b
		db  00110100b
		db  01001100b
		db  10000100b
		db  11001100b
		db  01110110b

e_      db  00000000b
		db  00000000b
		db  00000000b
		db  00111000b
		db  01001100b
		db  10011000b
		db  10100010b
		db  01111100b

f_      db  00000000b
		db  00011100b
		db  00110010b
		db  00011000b
		db  00111100b
		db  00011000b
		db  00011000b
		db  00111100b

g_      db  00000000b
		db  00000000b
		db  00000110b
		db  00111100b
		db  01100110b
		db  00111100b
		db  10000010b
		db  01111100b

h_      db  00000000b
		db  00100000b
		db  01100000b
		db  01100000b
		db  01111100b
		db  01100010b
		db  01100010b
		db  11110110b

i_      db  00000000b
		db  00000000b
		db  00011000b
		db  00011000b
		db  00000100b
		db  00111000b
		db  00011000b
		db  00111100b

j_      db  00000000b
		db  00011000b
		db  00011000b
		db  00000100b
		db  00111000b
		db  00011000b
		db  00011100b
		db  00111000b

k_      db  00000000b
		db  01000000b
		db  11000000b
		db  11001100b
		db  11011010b
		db  11100100b
		db  11011000b
		db  11000110b

l_      db  00000000b
		db  00001000b
		db  00011000b
		db  00011000b
		db  00011000b
		db  00011000b
		db  00011000b
		db  00111100b

m_      db  00000000b
		db  00000000b
		db  00000000b
		db  00000000b
		db  10100100b
		db  11011010b
		db  10011010b
		db  10011010b

n_      db  00000000b
		db  00000000b
		db  00000000b
		db  01011000b
		db  11100110b
		db  01000110b
		db  01000110b
		db  11101110b

o_      db  00000000b
		db  00000000b
		db  00000000b
		db  01111100b
		db  10001110b
		db  11000110b
		db  11100010b
		db  01111100b

p_      db  00000000b
		db  00000000b
		db  01000000b
		db  11111100b
		db  11001110b
		db  11100010b
		db  11011100b
		db  11000000b

q_      db  00000000b
		db  00000000b
		db  00000000b
		db  01111010b
		db  11000100b
		db  01111100b
		db  00000100b
		db  00001110b

r_      db  00000000b
		db  00000000b
		db  00000000b
		db  10011110b
		db  01111100b
		db  01100000b
		db  01100000b
		db  11110000b

s_      db  00000000b
		db  00000000b
		db  00000000b
		db  00111100b
		db  01100010b
		db  00111000b
		db  10001100b
		db  01111000b

t_      db  00000000b
		db  00000000b
		db  00001000b
		db  00011000b
		db  00111100b
		db  00011000b
		db  00011000b
		db  00111100b

u_      db  00000000b
		db  00000000b
		db  00000000b
		db  00100010b
		db  01100110b
		db  01100110b
		db  01100110b
		db  00111100b

v_      db  00000000b
		db  00000000b
		db  00000000b
		db  00001000b
		db  11000110b
		db  01100100b
		db  01100100b
		db  00111000b

w_      db  00000000b
		db  00000000b
		db  00000000b
		db  10000100b
		db  10100010b
		db  10010010b
		db  10101010b
		db  01000100b

x_      db  00000000b
		db  00000000b
		db  00000000b
		db  11000110b
		db  01101000b
		db  00111000b
		db  00101100b
		db  11000110b

y_      db  00000000b
		db  00000000b
		db  00000000b
		db  00100110b
		db  01110110b
		db  00011100b
		db  00001100b
		db  00111000b

z_      db  00000000b
		db  00000000b
		db  00000000b
		db  01111100b
		db  10011000b
		db  00110000b
		db  01100010b
		db  11111100b ;91 characters

LABEL	Fonts8x12	byte

LABEL   Fonts16x16   word

spc16   dw  1010101010101010b
		dw  0101010101010101b
		dw  1010101010101010b
		dw  0101010101010101b
		dw  1010101010101010b
		dw  0101010101010101b
		dw  1010101010101010b
		dw  0101010101010101b
		dw  1010101010101010b
		dw  0101010101010101b
		dw  1010101010101010b
		dw  0101010101010101b
		dw  1010101010101010b
		dw  0101010101010101b
		dw  1010101010101010b
		dw  0101010101010101b



LABEL	FOnts12x16	word



		CODESEG				;directive

		PUBLIC _putstring8x8,_putchar8x8,_putstring16x16,_putchar16x16
		PUBLIC _putstring8x12,_putchar8x12
		EXTRN ActiveOffset

;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                                PutChar8x8                               ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Displays a character according to byte input at position col,row        ³
;³ in the colour given by colour                                           ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: Bloody Everything                                              ³
;³ Output: AL=Colour                                                       ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC    _putchar8x8

		ARG     Col:word,Row:word,Char:byte,Colour:word

		push bp
		mov bp,sp
		push si     ;preserve si
		mov gs,di     ;preserve di

		mov ax,@data    ;set fs to font segment
		mov fs,ax
		mov si,OFFSET Fonts8x8 ;set si to start of 8x8 fonts

		mov ax,@data
		mov es,ax

		mov bx,[col]

		mov dx,3c4h         ;write plane index
		mov al,02
		out dx,al           ;set to change write index

		mov al,11h
		mov cl,bl
		and cl,3h
		shl al,cl           ;get start screen write plane

		mov dx,[row]       ;copy row to DX
		shl dx,SHIFT1
		mov di,dx       ;copy row to DI
		shl dx,SHIFT2        ;multiply by 64
		add di,dx 	;multiply by 16
		shr bx,2
		add di,bx
		add di,[es:ActiveOffset]   ;complete offset

		mov bx,vbase
		mov es,bx

		xor dh,dh
		mov dl,[Char]
		sub dx,32d
		shl dx,3


		add si,dx
		mov ah,[byte ptr colour]        ;ah has colour, al has plane
		mov dx,3c5h     ;prepare DX for out instructions
		mov bp,4        ;to count 4 planes

putcharplane8x8:
		out dx,al       ;change write plane
		mov bx,[word ptr fs:si]         ;get word from font data
		mov cx,8                        ;cx counts for 8 sets of two shifts
BitLoop8x8:
		shl bx,1
		jnc trans18x8
		mov [es:di],ah                  ;deposit first byte

Trans18x8:
		inc di
		shl bx,1
		jnc trans28x8
		mov [es:di],ah                  ;deposit second byte
trans28x8:
		add di,MaxX/4-1                 ;advance to next row

		dec cx
		jnz BitLoop8x8                    ;complete a plane

		add si,2
		rol al,1
		adc di,0            ;advance offset if necessarry for next plane
		sub di,MaxX*2
		dec bp
		jnz putcharplane8x8

		pop si
		mov di,gs
	pop bp
	ret
ENDP

;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                                PutString8x8                             ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Displays a string according to pointer input at position col,row        ³
;³ in the colour given by colour                                           ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: Bloody Everything  BP holds offset in string                   ³
;³ Output: AL=Colour           SI addresses charcter maps,DI screen buffer ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC    _putstring8x8

	ARG     Col:word,Row:word,String:dword,Colour:word

	push bp
	mov bp,sp

	push si     ;preserve si
	push di     ;preserve di
	push ds     ;preserve ds

	cld			;clear direction flag

	mov ax,@data
	mov fs,ax

	mov ax,@data
	mov es,ax

	mov bx,[col]

	mov dx,3c4h         ;write plane index
	mov al,02
	out dx,al           ;set to change write index

	mov al,11h
	mov cl,bl
	and cl,3h
	shl al,cl           ;get start screen write plane

	mov dx,[row]       ;copy row to DX
	shl dx,SHIFT1
	mov di,dx       ;copy col to DI
	shl dx,SHIFT2        ;multiply by 64
	add di,dx 	;multiply by 16
	shr bx,2
	add di,bx
	add di,[es:ActiveOffset]   ;complete offset

	mov dx,3c5h     ;prepare DX for out instructions
	mov ah,[byte ptr colour]        ;ah has colour, al has plane
	shl eax,16

	mov ax,vbase
	mov es,ax

	mov cx,-1

	lds si,[String]
	mov gs,si               ;copy si
getlength8x8:
	lodsb			;fetch character
	inc cx			;add one to count
	or al,al
	jnz getlength8x8	;jump if null character found
	or cx,cx
	jz nullstring8x8	;quit if string is empty
	mov si,gs       ;restore si

	mov gs,cx       ;use this lousy segment register to backup char count

	mov ax,4        ;to count 4 planes

putstringplane8x8:
	ror eax,16      ;store plane count in upper EAX
	mov cx,gs		;restore character count
	push si         ;backup start of string
	push di         ;backup screen start position
putstringchar8x8:

	mov bp,OFFSET Fonts8x8				;set fs:bp for 8x8 font data
	xor bh,bh
	mov bl,[byte ptr ds:si]         ;grab character
	inc si
	sub bx,32d
	shl bx,3

	add bp,bx                       ;adjust pointer to

	out dx,al       ;change write plane

	mov bx,[word ptr fs:bp]         ;get word from font data
	mov ch,8                        ;cl counts for 8 sets of two shifts
StringBitLoop8x8:
	shl bx,1
	jnc Stringtrans18x8
	mov [es:di],ah                  ;deposit first byte

StringTrans18x8:
	inc di
	shl bx,1
	jnc Stringtrans28x8
	mov [es:di],ah                  ;deposit second byte
Stringtrans28x8:
	add di,MaxX/4-1                       ;advance to next row

	dec ch
	jnz StringBitLoop8x8                    ;complete a plane

	sub di,MaxX*2-2	        ;bring di back up to start of next character
	dec cl
	jnz putstringchar8x8

	pop di
	pop si

	rol al,1
	adc di,0            ;advance offset if necessarry for next plane

	ror eax,16           ;bring back plane count
	dec ax
	jnz putstringplane8x8

nullstring8x8:
	pop ds
	pop di
	pop si
		pop bp
	ret


ENDP


;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                                PutChar8x12                              ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Displays a character according to byte input at position col,row        ³
;³ in the colour given by colour                                           ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: Bloody Everything                                              ³
;³ Output: AL=Colour                                                       ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC    _putchar8x12

		ARG     Col:word,Row:word,Char:byte,Colour:word

		push bp
		mov bp,sp
		push si     ;preserve si
		mov gs,di     ;preserve di

		mov ax,@data    ;set fs to font segment
		mov fs,ax
		mov si,OFFSET Fonts8x12 ;set si to start of 8x8 fonts

		mov ax,@data
		mov es,ax

		mov bx,[col]

		mov dx,3c4h         ;write plane index
		mov al,02
		out dx,al           ;set to change write index

		mov al,11h
		mov cl,bl
		and cl,3h
		shl al,cl           ;get start screen write plane

		mov dx,[row]       ;copy row to DX
		shl dx,SHIFT1
		mov di,dx       ;copy row to DI
		shl dx,SHIFT2        ;multiply by 64
		add di,dx 	;multiply by 16
		shr bx,2
		add di,bx
		add di,[es:ActiveOffset]   ;complete offset

		mov bx,vbase
		mov es,bx

		xor dh,dh
		mov dl,[Char]
		sub dx,32d
		shl dx,2
		add si,dx
		shl dx,1
		add si,dx
		mov ah,[byte ptr colour]        ;ah has colour, al has plane
		mov dx,3c5h     ;prepare DX for out instructions
		mov bp,4        ;to count 4 planes

putcharplane8x12:
		out dx,al       ;change write plane
		mov bx,[word ptr fs:si]         ;get word from font data
		mov cx,12                       ;cx counts for 12 sets of two shifts
BitLoop8x12:
		shl bx,1
		jnc trans18x12
		mov [es:di],ah                  ;deposit first byte

Trans18x12:
		inc di
		shl bx,1
		jnc trans28x12
		mov [es:di],ah                  ;deposit second byte
trans28x12:
		add di,MaxX/4-1                 ;advance to next row

		dec cx
		jnz BitLoop8x12                    ;complete a plane

		add si,2
		rol al,1
		adc di,0            ;advance offset if necessarry for next plane
		sub di,MaxX*3
		dec bp
		jnz putcharplane8x8

		pop si
		mov di,gs
	pop bp
	ret
ENDP

;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                                PutString8x12                            ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Displays a string according to pointer input at position col,row        ³
;³ in the colour given by colour                                           ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: Bloody Everything  BP holds offset in string                   ³
;³ Output: AL=Colour           SI addresses charcter maps,DI screen buffer ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC    _putstring8x12

	ARG     Col:word,Row:word,String:dword,Colour:word

	push bp
	mov bp,sp

	push si     ;preserve si
	push di     ;preserve di
	push ds     ;preserve ds

	cld			;clear direction flag

	mov ax,@data
	mov fs,ax

	mov ax,@data
	mov es,ax

	mov bx,[col]

	mov dx,3c4h         ;write plane index
	mov al,02
	out dx,al           ;set to change write index

	mov al,11h
	mov cl,bl
	and cl,3h
	shl al,cl           ;get start screen write plane

	mov dx,[row]       ;copy row to DX
	shl dx,SHIFT1
	mov di,dx       ;copy col to DI
	shl dx,SHIFT2        ;multiply by 64
	add di,dx 	;multiply by 16
	shr bx,2
	add di,bx
	add di,[es:ActiveOffset]   ;complete offset

	mov dx,3c5h     ;prepare DX for out instructions
	mov ah,[byte ptr colour]        ;ah has colour, al has plane
	shl eax,16

	mov ax,vbase
	mov es,ax

	mov cx,-1

	lds si,[String]
	mov gs,si               ;copy si
getlength8x12:
	lodsb			;fetch character
	inc cx			;add one to count
	or al,al
	jnz getlength8x8	;jump if null character found
	or cx,cx
	jz nullstring8x8	;quit if string is empty
	mov si,gs       ;restore si

	mov gs,cx       ;use this lousy segment register to backup char count

	mov ax,4        ;to count 4 planes

putstringplane8x12:
	ror eax,16      ;store plane count in upper EAX
	mov cx,gs		;restore character count
	push si         ;backup start of string
	push di         ;backup screen start position
putstringchar8x12:

	mov bp,OFFSET Fonts8x8				;set fs:bp for 8x8 font data
	xor bh,bh
	mov bl,[byte ptr ds:si]         ;grab character
	inc si
	sub bx,32d
	shl bx,2
	add bp,bx                       ;adjust pointer to
	shl bx,1
	add bp,bx
	out dx,al       ;change write plane

	mov bx,[word ptr fs:bp]         ;get word from font data
	mov ch,12                        ;cl counts for 12 sets of two shifts
StringBitLoop8x12:
	shl bx,1
	jnc Stringtrans18x8
	mov [es:di],ah                  ;deposit first byte

StringTrans18x12:
	inc di
	shl bx,1
	jnc Stringtrans28x8
	mov [es:di],ah                  ;deposit second byte
Stringtrans28x12:
	add di,MaxX/4-1                       ;advance to next row

	dec ch
	jnz StringBitLoop8x8                    ;complete a plane

	sub di,MaxX*3-2	        ;bring di back up to start of next character
	dec cl
	jnz putstringchar8x8

	pop di
	pop si

	rol al,1
	adc di,0            ;advance offset if necessarry for next plane

	ror eax,16           ;bring back plane count
	dec ax
	jnz putstringplane8x8

nullstring8x12:
	pop ds
	pop di
	pop si
		pop bp
	ret


ENDP

;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                                PutChar16x16                             ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Displays a character according to byte input at position col,row        ³
;³ in the colour given by colour                                           ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: Bloody Everything                                              ³
;³ Output: AL=Colour                                                       ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC    _putchar16x16

		ARG     Col:word,Row:word,Char:byte,Colour:word

		push bp
		mov bp,sp
		push si     ;preserve si
		mov gs,di     ;preserve di

		mov ax,@data     ;set fs to font segment
		mov fs,ax
		mov si,OFFSET Fonts16x16 ;set si to start of fonts

		mov ax,@data
		mov es,ax

		mov bx,[col]

		mov dx,3c4h         ;write plane index
		mov al,02
		out dx,al           ;set to change write index

		mov al,11h
		mov cl,bl
		and cl,3h
		shl al,cl           ;get start screen write plane

		mov dx,[row]       ;copy row to DX
		shl dx,SHIFT1
		mov di,dx       ;copy row to DI
		shl dx,SHIFT2        ;multiply by 64
		add di,dx 	;multiply by 16
		shr bx,2
		add di,bx
		add di,[es:ActiveOffset]   ;complete offset

		mov bx,vbase
		mov es,bx

		xor dh,dh
		mov dl,[Char]
		sub dx,32d
		shl dx,5


		add si,dx
		mov ah,[byte ptr colour]        ;ah has colour, al has plane
		mov dx,3c5h     ;prepare DX for out instructions

		mov ch,4		;ch counts 4 planes
putcharplane16x16:
		out dx,al       ;change write plane
		mov ebx,[dword ptr fs:si]         ;get dword from font data
		mov cl,16
BitLoop16x16:
		shl ebx,1
		jnc trans116x16
		mov [es:di],ah                  ;deposit first byte
Trans116x16:
		inc di
		shl ebx,1
		jnc trans216x16
		mov [es:di],ah                  ;deposit second byte
trans216x16:
		inc di
		shl ebx,1
		jnc trans316x16
		mov [es:di],ah                  ;deposit first byte
Trans316x16:
		inc di
		shl ebx,1
		jnc trans416x16
		mov [es:di],ah                  ;deposit second byte
trans416x16:
		add di,MaxX/4-3                 ;advance to next row

		cmp cl,9
		jz nextdword16x16

dwordcheck16x16:
		dec cl
		jnz BitLoop16x16                    ;complete a plane

		add si,8
		rol al,1
		adc di,0            ;advance offset if necessarry for next plane
		sub di,MaxX*4
		dec ch
		jnz putcharplane16x16

	pop si
	mov di,gs
	pop bp
	ret

nextdword16x16:
		mov ebx,[dword ptr fs:si+4]
		jmp short dwordcheck16x16
ENDP

;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                                PutString16x16                           ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Displays a string according to pointer input at position col,row        ³
;³ in the colour given by colour                                           ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: Bloody Everything  BP holds offset in string                   ³
;³ Output: AL=Colour           SI addresses charcter maps,DI screen buffer ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC    _putstring16x16

	ARG     Col:word,Row:word,String:dword,Colour:word

	push bp
	mov bp,sp

	push si     ;preserve si
	push di     ;preserve di
	push ds     ;preserve ds

				;set ds:si to string
	mov ax,@data
	mov fs,ax

	mov ax,@data
	mov es,ax

	mov bx,[col]

	mov dx,3c4h         ;write plane index
	mov al,02
	out dx,al           ;set to change write index

	mov al,11h
	mov cl,bl
	and cl,3h
	shl al,cl           ;get start screen write plane

	mov dx,[row]       ;copy row to DX
	shl dx,SHIFT1
	mov di,dx       ;copy col to DI
	shl dx,SHIFT2        ;multiply by 64
	add di,dx 	;multiply by 16
	shr bx,2
	add di,bx
	add di,[es:ActiveOffset]   ;complete offset

	mov dx,3c5h     ;prepare DX for out instructions
	mov ah,[byte ptr colour]        ;ah has colour, al has plane
	shl eax,16

	mov ax,vbase
	mov es,ax

	mov cx,-1

	lds si,[String]
	mov gs,si               ;copy si
getlength16x16:
	lodsb			;fetch character
	inc cx			;add one to count
	or al,al
	jnz getlength16x16	;jump if null character found
	or cx,cx
	jz nullstring16x16	;quit if string is empty
	mov si,gs       ;restore si

	mov gs,cx       ;use this lousy segment register to backup char count

	mov ax,4        ;to count 4 planes

putstringplane16x16:
	ror eax,16      ;store plane count in upper EAX
	mov cx,gs		;restore character count
	push si         ;backup start of string
	push di         ;backup screen start position
putstringchar16x16:

	mov bp,OFFSET Fonts16x16				;set fs:bp for 8x8 font data
	xor bh,bh
	mov bl,[byte ptr ds:si]         ;grab character
	inc si
	sub bx,32d
	shl bx,5

	add bp,bx                       ;adjust pointer to

	out dx,al       ;change write plane
	mov ebx,[dword ptr fs:bp]         ;get dword from font data
	mov ch,16                   ;sixteen set of shifts
stringBitLoop16x16:
	shl ebx,1
	jnc stringtrans116x16
	mov [es:di],ah                  ;deposit first byte
stringTrans116x16:
	inc di
	shl ebx,1
	jnc stringtrans216x16
	mov [es:di],ah                  ;deposit second byte
stringtrans216x16:
	inc di
	shl ebx,1
	jnc stringtrans316x16
	mov [es:di],ah                  ;deposit first byte
stringTrans316x16:
	inc di
	shl ebx,1
	jnc stringtrans416x16
	mov [es:di],ah                  ;deposit second byte
stringtrans416x16:
	add di,MaxX/4-3                 ;advance to next row

	cmp ch,9
	jz stringnextdword16x16

stringdwordcheck16x16:
	dec ch
	jnz stringBitLoop16x16                    ;complete a plane

	sub di,MaxX*4-4
	dec cl
	jnz putstringchar16x16

	pop di
	pop si

	rol al,1
	adc di,0            ;advance offset if necessarry for next plane

	ror eax,16           ;bring back plane count
	dec ax
	jnz putstringplane16x16

nullstring16x16:
	pop ds
	pop di
	pop si
	pop bp
	ret

stringnextdword16x16:
		mov ebx,[dword ptr fs:bp+4]
		jmp short stringdwordcheck16x16
ENDP



;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                                FreadFont8x8                             ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Loads a 8x8 pixel font into the 8x8 font buffer given the file name and ³
;³ offset.  File name is an null terminated string referenced by dword ptr ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: Bloody Everything                                              ³
;³ Output: AL=Colour                                                       ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC    _freadfont8x8

		ARG     fileid:word

		push bp
		mov bp,sp
		push ds

		mov ax,@data
		mov ds,ax
		mov dx,OFFSET Fonts8x8 ;set ds:dx to point to font data buffer
		mov bx,[fileid]
		mov cx,728	;set to read 728 bytes
		mov ah,3fh	;interrupt to read from file

		int 21h

		pop ds
		pop bp
		ret
ENDP


;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                                FreadFont8x12                            ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Loads a 8x12 pixel font into the 8x12 font buffer given the file id and ³
;³ offset.  File name is an null terminated string referenced by dword ptr ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: Bloody Everything                                              ³
;³ Output: AL=Colour                                                       ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC    _freadfont8x12

		ARG     fileid:word

		push bp
		mov bp,sp
		push ds

		mov ax,@data
		mov ds,ax
		mov dx,OFFSET Fonts8x12 ;set ds:dx to point to font data buffer
		mov bx,[fileid]
		mov cx,1092	;set to read 1092 bytes
		mov ah,3fh	;interrupt to read from file

		int 21h

		pop ds
		pop bp
		ret
ENDP


;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                                FreadFont12x16                           ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Loads a 12x16 pixel font into the font buffer given the file name and   ³
;³ offset.  File name is an null terminated string referenced by dword ptr ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: Bloody Everything                                              ³
;³ Output: AL=Colour                                                       ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC    _freadfont12x16

		ARG     fileid:word

		push bp
		mov bp,sp
		push ds

		mov ax,@data
		mov ds,ax
		mov dx,OFFSET Fonts12x16 ;set ds:dx to point to font data buffer
		mov bx,[fileid]
		mov cx,2912	;set to read 2912 bytes
		mov ah,3fh	;interrupt to read from file

		int 21h

		pop ds
		pop bp
		ret
ENDP


;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                                FreadFont16x16                           ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Loads a 8x8 pixel font into the 8x8 font buffer given the file name and ³
;³ offset.  File name is an null terminated string referenced by dword ptr ³
;ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;³ Changes: Bloody Everything                                              ³
;³ Output: AL=Colour                                                       ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC    _freadfont16x16

		ARG     fileid:word

		push bp
		mov bp,sp
		push ds

		mov ax,@data
		mov ds,ax
		mov dx,OFFSET Fonts16x16 ;set ds:dx to point to font data buffer
		mov bx,[fileid]
		mov cx,2912	;set to read 2912 bytes
		mov ah,3fh	;interrupt to read from file

		int 21h

		pop ds
		pop bp
		ret
ENDP


END
