include pxdriver.inc
include ppalette.inc
 
;################A protected mode interface (I hope) by Hutts################

	.386P   ;Enable all 386 Protected Mode Instructions

;############# Segments should be compiled in order encountered #############

ProtStackSize = 400h
RealStackSize = 100h

MinHighMem = 2000

V86ExitInt = 0fdh

;========================== Real Code Segment ==============================
RealCode SEGMENT PARA PUBLIC USE16 'CODE'

ASSUME cs:RealCode,ds:ProtCode

cli     ;disable interupts and forget about them for now

mov ax,ProtCode
mov ds,ax

mov eax,cr0     ;get machine status word in ax
mov cx,16h
test eax,1
jnz ExitError

mov cx,12h              ;index for xms error
mov ax,4300h
int 2fh                 ;check for XMS manager
cmp al,80h              ;80h means installed xms driver
jnz ExitError
mov ax,4310h
int 2fh                 ;grab xms service address
mov word ptr [XMSService],bx
mov word ptr [XMSService+2],es
            
mov ah,03h
call [XMSService]   ;enable a20 address line

mov ah,08h
mov cx,13h
call [XMSService]
cmp ax,MinHighMem
jl Quit                 ;not enough ext memory
movzx edi,ax
shl edi,10              ;get number of bytes free in edi
mov [FreeHighMem],edi
mov dx,ax               ;dx has number of k to allocate
mov ah,09h              ;allocate memory function
mov cx,14h
call [XMSService]       ;get handle in DX
mov [XMSHandle],dx
or al,al
jz Quit
mov ah,0ch              ;lock block function
call [XMSService]       ;get linear base of block in dx:bx & lock
shrd esi,edx,16         ;put dx in upper esi
mov si,bx       

mov eax,ProtCode
shl eax,4               ;linear address of prot code segment

sub esi,eax             ;esi offset from offical prot data 
mov [HighMemBase],esi   ;store base of high memory

mov ebx,Stak
shl ebx,4       ;linear address of stack segment
add ebx,OFFSET RealStackSpace + RealStackSize
mov edx,ebx
add ebx,ProtStackSize   ;ebx has linear topsi]   ;fetch second dword for one tile row
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

ENDYúXÿÿÿÿýXþXÿÿ€Y;************************************************************************/
;*	Copyright (C) 1986-1988 Phar Lap Software, Inc.			*/
;*	Unpublished - rights reserved under the Copyright Laws of the	*/
;*	United States.  Use, duplication, or disclosure by the 		*/
;*	Government is subject to restrictions as set forth in 		*/
;*	subparagraph (c)(1)(ii) of the Rights in Technical Data and 	*/
;*	Computer Software clause at 252.227-7013.			*/
;*	Phar Lap Software, Inc., 60 Aberdeen Ave., Cambridge, MA 02138	*/
;************************************************************************/
;
; HELLO.ASM - Hello world program for 386 protected mode
;
; This program is the [in]famous "Hello world" program.  It illustrates
; making MS-DOS system calls from 386 protected mode.
;

	assume	cs:_text,ds:_data

_text	segment	para public use32 'code'

	public	_start_

_start_	proc	near

	mov	ah,09h			; Output the message.
	mov	edx,offset hellomsg	;
	int	21h			;
        
        call _graphicsmode

   ection

db 0eah
dw OFFSET Quit,RealCode   ;return to real mode

Quit:

mov ax,ProtCode
mov ds,ax
mov es,ax
mov fs,ax
mov gs,ax
mov ax,RealCode
mov ss,ax
mov sp,OFFSET RealStackSpace+RealStackSize

lidt fword ptr [RealIDTRegisterValue]

mov ah,04h
call [XMSService]   ;disable a20
mov ah,0dh
mov dx,[XMSHandle]  
call [XMSService]   ;unlock xms chunk
mov ah,0ah
call [XMSService]   ;free xms chunk

cmp cx,-1       ;cx negative 1 if no error
jz NoError

ExitError:

mov ax,3
int 10h
mov ax,14
mul cx
add ax,OFFSET ExitMessages
mov dx,ax
mov ah,9h
int 21h         ;display message

NoError:

sti     ;enable interrupts

mov ax,4c00h
int 21os

;---------------------------Call Interrupt in V86 Mode----------- V86RealInt
V86Return:
mov [gs:V86ds],ds
mov [gs:V86es],es
int V86ExitInt      ;exception 13 traps int instruction with v86 exit code
ENDP V86RealInt
;---------------------------------------------------------------------------
RealC---------
RealC---------
RealCode ENDS
;========================= End Of Real Code Segment ========================



;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>



;========================== Prot Code Segment ==============================
ProtCode SEGMENT PARA PUBLIC USE32 'CODE'

PUBLIC Malloc,Free,GetVector,SetVector,Exit

PUBLIC FreeLowMem,FreeHighMem

PUBLIC GlobalSelector,RealCodeSelector,ProtCodeSelector,RealDataSelector
PUBLIC ProtDataSelector

ASSUME cs:ProtCode,ds:ProtCode

;---------------------------------------------------------------------------
ExitMessages LABEL byte
db 'Exception 0h',7,'$','Exception 1h',7,'$','Exception 2h',7,'$'
db 'Exception 3h',7,'$','Exception 4h',7,'$','Exception 5h',7,'$'
db 'Exception 6h',7,'$','Exception 7h',7,'$','Exception 8h',7,'$'
db 'Exception 9h',7,'$','Exception Ah',7,'$','Exception Bh',7,'$'
db 'Exception Ch',7,'$','Exception Dh',7,'$','Exception Eh',7,'$'
db 'Exception Fh',7,'$','Exception 10',7,'$','Unexpected!!',7,'$'
db 'No XMSDriver',7,'$','Not Enough H',7,'$','XallocFalied',7,'$'
db 'A20 error   ',7,'$','VCPI sux!!!!',7,'$'

hello db 'Hello World!',7,'$'

XMSService dd ?  ;address of XMS driver
XMSHandle dw ?      ;handle of big xms chunk

HighMemBase dd  ?   ;base pointer to high memory
LowMemBase dd   ?   ;base pointer to low memory
FreeLowMem  dd  ?   ;amount of free low memory
FreeHighMem dd  ?   ;amount of free high memory

oirqm21         db      ?               ; old low IRQ mask
oirqma1         db      ?               ; old high IRQ mask

V86eds LABEL dword
V86ds dw ?
V86ueds dw ?

V86ees LABEL dword
V86es dw ?
V86uees dw ?

V86EFlags LABEL dword
V86Flags dw ?
V86UEFlags dw ?

TSSesp0 dd  use32 'stack'

	db	8192 dup (?)

_stack	ends

	end _start_
row);
extern	void setfreevideo(void);
extern	void* valloc(unsigned buffersize);


r€6€€€€ô,€r€¼/€€Š6€€€€k,€r€f€Yb€ò€<€Ñ€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€dummy selector
GlobalDescriptor db 0ffh,0ffh,0,0,0,10010010b,11001111b,0
RealCodeDescriptor db 0ffh,0ffh,?,?,?,10011010b,00000000b,0
ProtCodeDescriptor db 0ffh,0ffh,?,?,?,10011010b,11001111b,0
RealDataDescriptor db 0ffh,0ffh,?,?,?,10010010b,00000000b,0
ProtDataDescriptor db 0ffh,0ffh,?,?,?,10010010b,11001111b,0
TaskDescriptor db 0ffh,0ffh,?,?,?,10001001b,00000000b,0  

GlobalSelector dw 8h
RealCodeSelector dw 10h
ProtCodeSelector dw 18h                  ;64 bit code selector value
RealDataSelector dw 20h                  ;selector for real data in pmode
ProtDataSelector dw 28h                  ;64 bit data selector value
TaskSelector dw 30h                      ;selector for real task state segment

InterruptDescriptorTable LABEL    ;Interrupt Descriptor Table
Exception0h         dw DivideZeroFault, ?,8e00h,0
Exception1h         dw SingleStepTrap,  ?,8e00h,0
Exception2h         dw NMIAbort,        ?,8e00h,0
Exception3h         dw BreakPointTrap,  ?,8e00h,0
Exception4h         dw OverFlowTrap,    ?,8e00h,0
Exception5h         dw BoundRangeFault, ?,8e00h,0
Exception6h         dw InvOpCodeFault,  ?,8e00h,0
Exception7h         dw NoCoProFault,    ?,8e00h,0
Exception8h         dw DoubleFaultAbort,?,8e00h,0
Exception9h         dw CoProSegAbort,   ?,8e00h,0
ExceptionAh         dw InvalidTSSFault, ?,8e00h,0
ExceptionBh         dw SegNotPresFault, ?,8e00h,0
ExceptionCh         dw StackExceptFault,?,8e00h,0
ExceptionDh         dw GeneralProtFault,?,8e00h,0
ExcpetionEh         dw PageFault,       ?,8e00h,0
ExceptionFh         dw CoProErrorFault, ?,8e00h,0
Exception10h        dw Arrangement,     ?,8e00h,0
Unexpected          dw 15 DUP (UnexpectedException, ?,8e00h,0)
IRQ0h               dw IrqService0,     ?,8e00h,0
IRQ1h               dw IrqService1,     ?,8e00h,0
IRQ2h               dw IrqService2,     ?,8e00h,0
IRQ3h               dw IrqService3,     ?,8e00h,0
IRQ4h               dw IrqService4,     ?,8e00h,0
IRQ5h               dw IrqService5,     ?,8e00h,0
IRQ6h               dw IrqService6,     ?,8e00h,0
IRQ7h               dw IrqService7,     ?,8e00h,0
IRQ8h               dw IrqService8,     ?,8e00h,0
IRQ9h               dw IrqService9,     ?,8e00h,0
IRQAh               dw IrqServiceA,     ?,8e00h,0
IRQBh               dw IrqServiceB,     ?,8e00h,0
IRQCh               dw IrqServiceC,     ?,8e00h,0
IRQDh               dw IrqServiceD,     ?,8e00h,0
IRQEh               dw IrqServiceE,     ?,8e00h,0
IRQFh               dw IrqServiceF,     ?,8e00h,0
RealIntService      dw CallRealInt,     ?,8e00h,0
;---------------------------------------------------------------------------

ProtStart:

movzx eax,cs:[ProtDataSelector]
mov ds,ax
mov es,ax
mov fs,ax
mov gs,ax
mov ss,ax
mov esp,ebx     ;set protected mode stack pointer

mov edx,OFFSET InterruptDescriptorTable+2
mov ecx,49      ;49 interrupt entries
BuildIDT:
mov [edx],cs
add edx,8
loop BuildIDT

lidt fword ptr [ProtIDTRegisterValue]

mov byte ptr [esi],1     ;setup intial free mem pointer
mov dword ptr [esi+1],edi  ;set inital size of free mem
mov dword ptr [esi+5],0
mov dword ptr [esi+9],0

mov dword ptr [ebx+4],ebx               ;set tss privelege 0 stack pointer
mov dword ptr [ebx+8],eax               ;set tss privelege 0 stack segment
mov edi,ebx
add edi,12
mov ecx,22
xor eax,eax
rep stosd       ;store 22 zero dwords in TSS
mov dword ptr [edi],104 shl 16
add edi,4
mov ecx,2000h
rep stosb       ;fill in IO permission bitmap with zeros

mov [LowMemBase],edi
sub ebp,edi
inc ebp         ;get size of free low memory
mov [FreeLowMem],ebp
mov byte ptr [edi],1    ;setup inital free mem pointer
mov dword ptr [edi+1],ebp
mov dword ptr [edi+5],0
mov dword ptr [edi+9],0 ;low memory next and far pointers set

mov ax,[GlobalSelector]
mov es,ax

mov ax,[TaskSelector]
ltr ax          ;load task register to point to this task har har

pushfd
pop eax
or ah,30h       ;set IO privelege level to 3
and ah,not 40h  ;clear nested task flag
push eax
popfd

in al,21h
mov [oirqm21],al
or al,3
out 21h,al
in al,0a1h
mov [oirqma1],al
mov bx,2820h
call set8529vektorz
sti

jmp main        ;ready to rip

Main:

mov ebp,21h
mov ah,9
mov v86ds,ProtCode
mov dx,OFFSET hello
int 30h

call GraphicsMode
call set320x240
mov ax,2
call StartVsync
mov dx,255

mov eax,2048000
call malloc

push eax

mov edi,eax

mov dx,1
l:
mov edi,0a0000h
mov al,dl
mov ah,al
shl eax,8
mov al,ah
shl eax,8
mov al,ah
mov ecx,4800
rep stosd
dec dx
jnz l

mov ax,1
mov dx,63

l2:
mov bl,dl
mov cl,dl
call SetPalReg 
call retrace
dec dx
jnz l2                          ³
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

		mov ax,vbase
		mov es,ax           ;set data segment to temp screen buffer

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
;³ Puts a binary immov [esp+16],ax

pop ebx
pop eax
pop ebp

iretd               ;jump to v86 interrupt

Exception13:
mov cx,13
jmp Exit

PageFault:
mov cx,14
jmp Exit

CoProErrorFault:
mov cx,15
jmp Exit

Arrangement:
mov cx,16
jmp Exit

UnexpectedException:
mov cx,17
jmp Exit

;==============================IRQ Handlers================================

IrqService0:
push ebp
mov ebp,8
jmp IrqService

IrqService1:
push ebp
mov ebp,9
jmp IrqService

IrqService2:
push ebp
mov ebp,0ah
jmp IrqService

IrqService3:
push ebp
mov ebp,0bh
jmp IrqService

IrqService4:
push ebp
mov ebp,0ch
jmp IrqService

IrqService5:
push ebp
mov ebp,0dh
jmp IrqService

IrqService6:
push ebp
mov ebp,0eh
jmp IrqService

IrqService7:
push ebp
mov ebp,0fh
jmp IrqService

IrqService8:
push ebp
mov ebp,8
jmp IrqService

IrqService9:
push ebp
mov ebp,70h
jmp IrqService

IrqServiceA:
push ebp
mov ebp,71h
jmp IrqService

IrqServiceB:
push ebp
mov ebp,72h
jmp IrqService

IrqServiceC:
push ebp
mov ebp,73h
jmp IrqService

IrqServiceD:
push ebp
mov ebp,74h
jmp IrqService

IrqServiceE:
push ebp
mov ebp,75h
jmp IrqService

IrqServiceF:
push ebp
mov ebp,76h

IrqService:
test byte ptr [esp+14],2;test if irq was in V86 mode
jz ReflectIrq           ;irq in pmode, reflect to v86 below
push eax
push ebx
movzx eax,cs:[GlobalSelector]
mov ds,ax
jmp SoftwareInterrupt   ;resume interrupt in v86
;=========================== V86 Interrupt ================================

CallRealInt PROC        ;ebp has interrupt number to call

;fake stack after interrupt in v86 mode to return

push ebp

ReflectIrq:

push ds
push es
push fs
push gs                 ;save the seggies, they are lost, but gennies survive!

mov [esp-40],eax
mov [esp-44],ebx

mov eax,ProtCode
push eax                ;push v86 gs padded to two words
push eax                ;push v86 fs padded to two words
push [ss:V86eds]        ;push v86 ds padded to two words
push [ss:V86ees]        ;push v86 es padded to two words
mov eax,Stak
push eax                ;push v86 stack segment padded to two words
mov eax,OFFSET RealStackSpace+RealStackSize-6
push eax                ;push v86 stack pointer

pushfd
pop eax
or eax,20000h           ;set VM flag in flags
push eax                ;leave flags on stack

movzx ebx,[GlobalSelector]
mov ds,bx
shld bx,bp,18

push dword ptr [ebx+2];push target segment on stack
push dword ptr [ebx]  ;push target offset on stack

mov ebp,[ss:StakBase]
mov [ebp-2],ax
mov word ptr [ebp-4],RealCode
mov word ptr [ebp-6],OFFSET V86Return ;stick return address on V86 stack

mov ebp,[ss:TSSesp0]    ;get address of TSS esp0 in ebp
lea eax,[esp+36]
mov [ebp],eax           ;save eax stack pointer as p0 esp in TSS

mov eax,[esp-4]         ;restore eax, ebp is a gonna
mov ebx,[esp-8]

iretd                   ;jump to real code and set processor in V86 mode

ResumeFromV86:

pop eax
add esp,40

pop gs
pop fs
pop es
pop ds

pop ebp

iretd
CallRealInt ENDP

;=======================Interrupt Management================================
GetVector PROC      ;returns 32 bit int vector pointer relative to code segment
                    ;interrupt vector index in EBX register, assumes ds->data
push ebx
shl ebx,3
mov ax,word ptr [InterruptDescriptorTable+ebx+6]
shr eax,16
mov ax,word ptr [InterruptDescriptorTable+ebx]
pop ebx
ret
GetVector ENDP


SetVector PROC      ;installs new int vector pointer relatve to code segment
                    ;interrupt id in bl, address pointer in EAX
push eax
push ebx
movzx ebx,bl
shl ebx,3
mov word ptr [InterruptDescriptorTable+ebx],ax
shr eax,16
mov word ptr [InterruptDescriptorTable+ebx+6],ax
pop ebx
pop eax
ret
SetVector ENDP

;======================Reprogram Interrupt Controller=======================

set8529vektorz PROC                     ; Set new IRQ vektor numbers
        mov al,11h                      ;  BL - low vektor base #
        out 20h,al                      ;  BH - high vektor base #
        jmp short $+2
        mov al,bl
        out 21h,al
        jmp short $+2
        mov al,4h
        out 21h,al
        jmp short $+2
        mov al,1h
        out 21h,al
        jmp short $+2
        mov al,11h
        out 0a0h,al
        jmp short $+2
        mov al,bh
        out 0a1h,al
        jmp short $+2
        mov al,2h
        out 0a1h,al
        jmp short $+2
        mov al,1h
        out 0a1h,al
        ret
set8529vektorz ENDP

;=========================Memory Management Functions=======================
Malloc PROC     ;allocates HIGH memory, amount in EAX register

push ebx
push ecx
push edx

mov ebx,[HighMemBase]   ;address of first block record

TestBlock:

test byte ptr [ebx],1   ;check if free
jz NextBlock 
mov ecx,[ebx+1]
cmp ecx,eax             ;check if size is large enough
jl NextBlock

mov byte ptr [ebx],0    ;set block to used
mov [ebx+1],eax         ;set new size of block
sub ecx,eax             ;get spare memory left
sub ecx,13              ;check if enough for new record and block
jle NoSpareMem

add eax,ebx             ;get new address of new block record
mov edx,[ebx+5]         ;get old block next pointer
or edx,edx
jz NoNextBlock
mov [edx+9],eax         ;set prev pointer of next block to new block

NoNextBlock:

mov [ebx+5],eax         ;set pointer from old block to new block
mov byte ptr [eax],1    ;set new block to free status
mov [eax+1],ecx         ;set size of new block
mov [eax+5],edx         ;set next pointer to next pointer from old block
mov [eax+9],ebx         ;set prev pointer to old block

NoSpareMem:

lea eax,[ebx+13]        ;get final return address in EAX
pop edx
pop ecx
pop ebx
ret

NextBlock:

mov ebx,[ebx+5]         ;get address of next block
or ebx,ebx
jnz TestBlock

xor eax,eax             ;return NULL pointer
pop edx
pop ecx
pop ebx

ret
Malloc ENDP


Free PROC               ;Frees a block of memory, pointer in EAX

push eax
push ebx
push ecx
push edx
push esi
push edi

sub eax,13
mov byte ptr [eax],1    ;mark block as free
mov ecx,[eax+1]         ;fetch size of block to be freed
mov ebx,[eax+5]         ;fetch next block pointer
mov edx,[eax+9]         ;fetch prev block pointer
movzx esi,byte ptr [ebx];fetch next block status
movzx edi,byte ptr [edx];fetch prev block status

or esi,ebx              ;test if next pointer exists, and block is free
jz CheckMergePrev
or edi,edx              ;test if prev pointer exists and block is free
jnz MergeBoth

MergeNext:

add ecx,[ebx+1]         ;add size of next block
add ecx,13
mov edx,[ebx+5]         ;get next pointer of next block
mov [eax+5],edx
mov [edx+9],eax         ;set new prev pointer of following block
jmp NoMerge

CheckMergePrev:
or edi,edx
jnz NoMerge

MergePrev:

add ecx,13
add [edx+1],ecx         ;set new block size
mov [ebx+9],edx         ;set prev pointer of next block to prev block
mov [edx+5],ebx         ;set next pointer of prev block to next block
jmp NoMerge

MergeBoth:

add ecx,26
add ecx,[ebx+1]         ;add size of next block
add [edx+1],ecx         ;get new size of prev block
mov ebx,[ebx+5]         ;get address of following block
mov [ebx+9],edx         ;set new prev pointer
mov [edx+5],ebx

NoMerge:

pop edi
pop esi
pop edx
pop ecx
pop ebx
pop eax
ret

Free ENDP


MallocLow PROC     ;allocates LOW memory, amount in EAX register

push ebx
push ecx
push edx

mov ebx,[LowMemBase]   ;address of first block record
jmp TestBlock           ;jump into high malloc routine, code is same
MallocLow ENDP

ProtCode ENDS
;========================= End Of Prot Code Segment ========================





;============================ Stack Segment ================================
Stak SEGMENT PARA STACK USE32 'STACK'
;------------------------ Real Mode Stack Defined Here --------------------- 
RealStackSpace db RealStackSize DUP (?)
;---------------------------------------------------------------------------
Stak ENDS
;========================= End Of Stack Segment ============================

END

