; PMODE v1.29a raw, DPMI, VCPI, & XMS compliant protected mode header.
; By Tran (a.k.a. Thomas Pytel) of Renaissance.

        .386p

LOWMIN          = 0             ; minimum free low memory (in K)
EXTMIN          = 0             ; minimum free extended memory (in K)
STAKLEN         = 100h          ; stack size in para (keep below or = 1000h)
STAKSAFE        = 20h           ; safe stack space in para

code16  segment para public use16
code16  ends
code32  segment para public use32
code32  ends
codeend segment para stack use32 'stack'
codeend ends

;²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
; Real mode and 16bit code
;²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
code16  segment para public use16
        assume cs:code16, ds:code16
        org 0

;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
; 16 bit common system data
;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
errmsg0         db      '386 or better not detected!!!',7,'$'
errmsg1         db      'Not enough low memory!!!',7,'$'
errmsg2         db      'This system is already in V86 mode!!!',7,'$'
errmsg3         db      'Not enough extended memory!!!',7,'$'
errmsg4         db      'Couldn''t enable A20 gate!!!',7,'$'
errmsg5         db      'Extended memory allocation failure. (weird eh???)',7,'$'

nullint         db      0cfh            ; IRET instruction
exitrout        dw      exit            ; exit routine, modified if XMS, VCPI

;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
; 16 bit common system code
;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
;ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
intreal:                                ; temporary int to real mode
        pushf
;-----------------------------------------------------------------------------
callreal:                               ; temporary far call to real mode
        push cs
        push offset icreald
        mov fs,cs:v86r_fs
        mov gs,cs:v86r_gs
        mov eax,cs:v86r_eax
        mov ecx,cs:v86r_ecx
        mov edx,cs:v86r_edx
        mov ebx,cs:v86r_ebx
        mov esi,cs:v86r_esi
        mov edi,cs:v86r_edi
        mov ebp,cs:v86r_ebp
        push cs:tempaddx
icrealif        db      ?       ; CLI or STI
        retf
icreald:
        cli
        pushf
        pop cs:v86r_flags
        mov cs:v86r_eax,eax
        mov cs:v86r_ecx,ecx
        mov cs:v86r_edx,edx
        mov cs:v86r_ebx,ebx
        mov cs:v86r_esi,esi
        mov cs:v86r_edi,edi
        mov cs:v86r_ebp,ebp
        mov cs:v86r_ds,ds
        mov cs:v86r_es,es
        mov cs:v86r_fs,fs
        mov cs:v86r_gs,gs
icrealret       label word              ; return to pmode method, modifiable
        jmp short d_icrealret
;-----------------------------------------------------------------------------
v_icrealret:                            ; VCPI return to pmode
        mov eax,offset cp_int3_d        ; jump to end of pmode INT32/33
        jmp v_switchtopmode
;-----------------------------------------------------------------------------
d_icrealret:                            ; DPMI return to pmode
        mov ebx,cs:dp_savedstakoff
        mov dx,cs:dp_savedstaksel
        mov cx,dx
        mov ax,cs:_seldata
        mov si,cs:_selcode
        mov edi,offset dp_int3_d
        jmp cs:d_switchaddx
;ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
chek_VCPI:                              ; Chek for VCPI
        mov dx,offset v_emmname
        mov ax,3d00h
        int 21h
        jc short chekVCPIa
        mov bx,ax
        mov ax,4400h
        int 21h
        jc short chekVCPIa
        test dh,80h
        jz short chekVCPIa
        mov ax,4407h
        int 21h
        mov dl,al
        mov ah,3Eh
        int 21h
        cmp dl,0FFh
        jne short chekVCPIa
        mov bx,1
        mov ah,43h
        int 67h
        or ah,ah
        jnz short chekVCPIa
        mov v_emshandle,dx
        mov ax,0de00h
        int 67h
        or ah,ah
        clc
        jz short chekVCPId
        mov ah,45h
        int 67h
chekVCPIa:
        stc
chekVCPId:
        ret
;ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
chek_processor:                         ; Detect if current processor 386
        pushf
        xor ah,ah
        push ax
        popf
        pushf
        pop ax
        and ah,0f0h
        cmp ah,0f0h
        je short detectno386
        mov ah,0f0h
        push ax
        popf
        pushf
        pop ax
        and ah,0f0h
        jz short detectno386
        popf
        ret
detectno386:
        mov dx,offset errmsg0
        jmp short exit16err
;ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
chek_V86:                               ; Chek if already in V86 mode
        smsw ax
        test al,1
        mov dx,offset errmsg2
        jnz short exit16err
        ret
;ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
pregetlomem:                            ; Get low memory or abort
        add eax,ds:_lomembase
        mov ebx,ds:_lomemtop
        cmp eax,ebx
        ja short pregetlomema
        mov ecx,eax
        xchg eax,ds:_lomembase
        sub ebx,ecx
        cmp ebx,LOWMIN*1024
        jb short pregetlomema
        ret
pregetlomema:
        mov dx,offset errmsg1
;ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
exit16err:                              ; Exit program with message
        mov ah,9
        int 21h
        jmp exitrout
;-----------------------------------------------------------------------------
exit:                                   ; Guess what???
        mov ah,4ch
        int 21h
;ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
start16:                                ; Program begins here
        cli
        cld
        push cs
        pop ds

        call chek_processor             ; is it at least a 386¨

        mov ax,es                       ; set up a bunch of pointers
        movzx eax,ax
        shl eax,4
        mov ds:_pspa,eax
        mov eax,code16
        shl eax,4
        mov ds:_code16a,eax
        or dword ptr ds:gdt32code16[2],eax
        or dword ptr ds:gdt32data16[2],eax
        mov ebx,code32
        shl ebx,4
        mov ds:_code32a,ebx
        or dword ptr ds:gdt32code32[2],ebx
        or dword ptr ds:gdt32data32[2],ebx
        add dword ptr ds:c_gdt32addx[2],ebx
        mov eax,codeend
        shl eax,4
        sub eax,ebx
        mov ds:_lomembase,eax
        mov ds:realstackbase,eax
        movzx eax,word ptr es:[2]
        shl eax,4
        sub eax,ebx
        mov ds:_lomemtop,eax

        mov eax,STAKLEN*16              ; get stack memory
        call pregetlomem

        push es                         ; save PSP seg (DPMI chek kills ES)
        pop fs

        mov ax,1687h                    ; chek for DPMI
        int 2fh
        or ax,ax
        jz d_start

        call chek_VCPI                  ; chek for VCPI
        jnc v_start

        call chek_V86                   ; chek for V86 mode

        mov ax,4300h                    ; chek for XMS
        int 2fh
        cmp al,80h
        je x_start

        jmp c_start                     ; custom system start
;ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
; BL=low vektor base, BH=high vektor base
set8259vektorz:                         ; set new IRQ vektor numbers
        in al,0a1h
        mov ch,al
        in al,21h
        mov cl,al
        mov al,11h
        out 20h,al
        out 0a0h,al
        jmp short $+2
        jmp short $+2
        jmp short $+2
        mov al,bl
        out 21h,al
        mov al,bh
        out 0a1h,al
        jmp short $+2
        jmp short $+2
        jmp short $+2
        mov al,4
        out 21h,al
        mov al,2
        out 0a1h,al
        jmp short $+2
        jmp short $+2
        jmp short $+2
        mov al,1
        out 21h,al
        out 0a1h,al
        mov al,cl
        out 21h,al
        mov al,ch
        out 0a1h,al
        ret
;ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
enableA20:                              ; hardware enable gate A20
        xor ax,ax
        mov fs,ax
        dec ax
        mov gs,ax
        call testA20
        je short enableA20done
        in al,92h                       ; PS/2 A20 enable
        or al,2
        jmp short $+2
        jmp short $+2
        jmp short $+2
        out 92h,al
        call testA20
        je short enableA20done
        call enableA20o1                ; AT A20 enable
        jnz short enableA20wait
        mov al,0d1h
        out 64h,al
        call enableA20o1
        jnz short enableA20wait
        mov al,0dfh
        out 60h,al
        push offset enableA20wait
enableA20o1:
        mov ecx,20000h
enableA20o1l:
        jmp short $+2
        jmp short $+2
        jmp short $+2
        in al,64h
        test al,2
        loopnz enableA20o1l
enableA20done:
        ret
;-----------------------------------------------------------------------------
enableA20wait:                          ; wait for A20
        mov cx,800h
enableA20waitl0:
        call testA20
        je enableA20done
        in al,40h
        in al,40h
        mov ah,al
enableA20waitl1:
        in al,40h
        in al,40h
        cmp al,ah
        je enableA20waitl1
        loop enableA20waitl0
        mov dx,offset errmsg4
        jmp exit16err
;-----------------------------------------------------------------------------
testA20:                                ; Test for enabled A20
        mov al,fs:[0]
        mov ah,al
        not al
        mov gs:[10h],al
        cmp ah,fs:[0]
        mov fs:[0],ah
        ret
;ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
; BL=low PIC val, BH=high PIC val, DI->int slot table
setintslots:                            ; set int nums in table to PIC vals
        mov cl,8
setintslotsl0:
        mov [di],bl
        inc di
        inc bl
        dec cl
        jnz setintslotsl0
        mov cl,8
setintslotsl1:
        mov [di],bh
        inc di
        inc bh
        dec cl
        jnz setintslotsl1
        ret
;ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
; DI=return to addx
setpmoderegs:                           ; set pmode reg values
        mov ax,28h
        mov ds,ax
        mov al,18h
        mov gs,ax
        mov al,10h
        mov es,ax
        mov fs,ax
        mov ss,ax
        mov esp,STAKLEN*16
        add esp,ds:realstackbase
        jmp di

;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
; 16 bit DPMI system data
;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
d_errmsg0       db      'DPMI host is not 32bit!!!',7,'$'
d_errmsg1       db      'Ran out of DPMI descriptors!!!',7,'$'
d_errmsg2       db      'Couldn''t modify DPMI descriptors as needed!!!',7,'$'

d_enterpmode    dw      ?,?             ; DPMI switch to pmode addx
d_pspsel        dw      ?               ; stupid PSP selector
d_oldenvsegsel  dw      ?               ; stupid selector we dont want

d_oint32vect    dd      ?               ; old int 32h vektor

d_switchaddx    dd      ?               ; switch to pmode addx
d_saveaddx      dd      ?               ; save/restore state addx
d_savedstakoff  dw      ?               ; current saved stack offset
d_savedstakseg  dw      ?               ; current saved stack segment

d_nintoff       dw      dp_irq0,dp_irq1,dp_irq2,dp_irq3,dp_irq4,dp_irq5,dp_irq6,dp_irq7
                dw      dp_irq8,dp_irq9,dp_irqa,dp_irqb,dp_irqc,dp_irqd,dp_irqe,dp_irqf
                dw      dp_int33,dp_int32

;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
; 16 bit DPMI system code
;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
;ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
d_int32:                                ; DPMI/VCPI real INT32: EDX=off
        pushad
        push ds es fs gs
        cli
        mov ax,cs
        mov ds,ax
        mov ds:tempaddx,edx
        mov al,[esp+44]
        shr al,1
        and al,1
        add al,0fah
        mov ds:dp_callpmodeif,al
        push d_savedstakoff
        push d_savedstakseg
        movzx ebx,ds:nextmodestack
        lea eax,[ebx-STAKSAFE*16]
        mov ds:nextmodestack,ax
        add ebx,ds:realstackbase
        sub sp,ds:dp_savelen
        mov d_savedstakoff,sp
        mov d_savedstakseg,ss
d_int32m0:                              ; modified under VCPI
VCPIINT32M0=(v_int32-3)-$               ;
        mov ax,ss
        mov es,ax
        mov di,sp
        xor al,al
        call d_saveaddx
        mov ax,ds:_seldata
        mov cx,ax
        mov dx,ax
        mov si,ds:_selcode
        mov edi,offset dp_callpmode
        jmp d_switchaddx
d_int32d:
        mov di,sp
        mov al,1
        call d_saveaddx
        add sp,ds:dp_savelen
;-----------------------------------------------------------------------------
d_int32d2:                              ; done from DPMI or VCPI
        pop d_savedstakseg
        pop d_savedstakoff
        add ds:nextmodestack,STAKSAFE*16
        mov bx,ds:v86r_flags
        mov ax,[esp+44]
        and ax,not 8d5h
        and bx,8d5h
        or ax,bx
        mov [esp+44],ax
        pop gs fs es ds
        popad
        iret
;ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
d_retreal:                              ; Return to real mode
        mov eax,d_oint32vect            ; restore real mode INT32
        mov gs:[32h*4],eax
        mov ax,205h                     ; restore all int vektorz needed
        mov edi,17
d_retreall0:
        mov bl,ds:intslotnum[di]
        lea ebp,[edi*2+edi]
        mov edx,dword ptr ds:dp_ointbuf[ebp*2]
        mov cx,word ptr ds:dp_ointbuf[ebp*2+4]
        int 31h
        sub di,1
        jnc d_retreall0
        jmp short d_exit
;ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
d_exit16err:                            ; DPMI Exit with error message
        mov ds:v86r_ds,code16
        mov ds:v86r_ah,9
        mov ax,300h
        mov bx,21h
        xor cx,cx
        mov edi,offset ds:v86r_edi
        push ds
        pop es
        int 31h
;ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
d_exit:                                 ; DPMI exit to real mode
        mov es,d_pspsel                 ; restore env selector
        mov ax,d_oldenvsegsel
        mov es:[2ch],ax
        jmp exit
;ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
d_start:                                ; Start in a DPMI system
        or ds:_sysbyte0,3               ; set system type DPMI byte

        test bl,1                       ; must be 32bit DPMI
        mov dx,offset d_errmsg0
        jz exit16err

        mov d_enterpmode[0],di          ; store enter addx
        mov d_enterpmode[2],es
        push word ptr fs:[2ch]          ; preserve old env seg

        movzx eax,si                    ; get mem for DPMI blok
        shl eax,4
        call pregetlomem
        shr eax,4
        add ax,code32
        mov es,ax

        mov ax,1                        ; switch to pmode
        call dword ptr d_enterpmode
        cli                             ; I don't trust DPMI
        mov dx,offset d_errmsg1
        jc exit16err
        mov ds:v86r_dx,dx               ; prepare for abort maybe
        pop ax                          ; swap old env seg with selector
        xchg ax,es:[2ch]
        mov d_oldenvsegsel,ax
        mov d_pspsel,es                 ; store stupid selectors
        mov ds:data16sel,ds
        mov ds:code16sel,cs
        mov ds:code16off,offset d_retreal       ; set return to real mode addx
        mov ds:_setirqvect,offset dp_setirq     ; modify some stuff
        mov ds:_getirqvect,offset dp_getirq

        push ds                         ; no more need for PSP
        pop es
        mov ax,3                        ; get selector increment value
        int 31h
        mov bx,ax
        xor ax,ax                       ; get 3 needed descriptors
        mov cx,3
        int 31h
        jc d_exit16err

        mov si,ax                       ; set up descriptors
        mov ds:_selcode,ax
        lea ecx,[eax+ebx]
        mov ds:_seldata,cx
        lea ebp,[ecx+ebx]
        mov ds:_selzero,bp
        mov ds:v86r_dx,offset d_errmsg2
        mov ax,0ch                      ; set descriptors from GDT
        mov bx,si
        mov edi,offset ds:gdt32code32
        or byte ptr [edi+5],60h
        int 31h
        jc d_exit16err
        mov bx,cx
        mov edi,offset ds:gdt32data32
        or byte ptr [edi+5],60h
        int 31h
        jc d_exit16err
        mov bx,bp
        mov edi,offset ds:gdt32zero32
        or byte ptr [edi+5],60h
        int 31h
        jc d_exit16err

        mov es,cx                       ; ES, FS, and GS what they should be
        mov fs,cx
        mov gs,bp

        mov edi,ds:_lomembase           ; chek and get extended memory
 ss with currently ASSUMEd segment registers
**Error** xnew.asm(947) Can't address with currently ASSUMEd segment registers
**Error** xnew.asm(952) Can't address with currently ASSUMEd segment registers
**Error** xnew.asm(963) Can't address with currently ASSUMEd segment registers
**Error** xnew.asm(976) Can't address with currently ASSUMEd segment registers
**Error** xnew.asm(979) Can't address with currently ASSUMEd segment registers
**Error** xnew.asm(980) Can't address with currently ASSUMEd segment registers
**Error** xnew.asm(1007) Can't address with currently ASSUMEd segment registers
**Error** xnew.asm(1008) Can't address with currently ASSUMEd segment registers
**Error** xnew.asm(1009) Can't address with currently ASSUMEd segment registers
**Error** xnew.asm(1013) Can't address with currently ASSUMEd segment registers
**Error** xnew.asm(1015) Can't address with currently ASSUMEd segment registers
**Error** xnew.asm(1060) Can't address with currently ASSUMEd segment registers
**Error** xne ptr d_saveaddx[0],cx
        mov word ptr d_saveaddx[2],bx
        mov ax,306h                     ; get switch mode addxs
        int 31h
        mov dword ptr ds:dp_switchaddx[0],edi
        mov word ptr ds:dp_switchaddx[4],si
        mov word ptr d_switchaddx[0],cx
        mov word ptr d_switchaddx[2],bx

        mov ax,400h                     ; set IRQ handlers to PIC values
        int 31h
        mov di,offset ds:intslotnum
        xchg dl,dh
        mov bx,dx
        call setintslots

        mov ah,2                        ; backup and set all int vektorz
        mov si,ds:_selcode
        mov edi,17
d_startl0:
        mov bl,ds:intslotnum[di]
        mov al,4
        int 31h
        lea ebp,[edi*2+edi]
        mov dword ptr ds:dp_ointbuf[ebp*2],edx
        mov word ptr ds:dp_ointbuf[ebp*2+4],cx
        mov al,5
        movzx edx,d_nintoff[edi*2]
        mov cx,si
        int 31h
        sub di,1
        jnc d_startl0

        mov eax,gs:[32h*4]              ; set real mode int32 handler
        mov d_oint32vect,eax
        db 65h,66h,0c7h,6       ; MOV DWORD PTR GS:[32h*4],code16:d_int32
        dw 32h*4,d_int32,code16 ;
        push es                         ; set up needed regs & go on to 32bit
        pop ss
        add esp,ds:realstackbase
;-----------------------------------------------------------------------------
c_to32bit:                              ; shared by custom code
        push es
        pop ds
        push dword ptr cs:_selcode
        push offset p_start
        db 66h,0cbh             ; 32bit RETF

;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
; 16 bit XMS system data
;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
x_calladdx      dd      ?               ; XMS driver addx
x_handle        dw      ?               ; XMS handle of extended memory

;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
; 16 bit XMS system code
;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
;ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
x_exit:                                 ; XMS exit (clean up allocation)
        mov dx,x_handle
        mov ah,0dh
        call x_calladdx
        mov ah,0ah
        call x_calladdx
        jmp exit
;ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
x_exiterr5:                             ; exit with error message 5
        mov dx,offset errmsg5
        jmp exit16err
;ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
x_start:                                ; start in an XMS system
        or ds:_sysbyte0,1               ; set system type XMS byte

        mov ax,4310h                    ; get XMS driver addx
        int 2fh
        mov word ptr x_calladdx[0],bx
        mov word ptr x_calladdx[2],es

        mov ah,3                        ; XMS enable A20
        call x_calladdx
        or ax,ax
        mov dx,offset errmsg4
        jz exit16err

        mov ah,8                        ; chek and get extended memory
        call x_calladdx
        cmp ax,EXTMIN+64
        mov dx,offset errmsg3
        jb exit16err
        lea edx,[eax-64]
        movzx ecx,dx
        shl ecx,10
        mov ah,9
        call x_calladdx
        or ax,ax
        jz x_exiterr5
        mov x_handle,dx
        mov exitrout,offset x_exit
        mov ah,0ch
        call x_calladdx
        or ax,ax
        jz x_exiterr5
        shrd eax,edx,16
        mov ax,bx
        sub eax,ds:_code32a
        mov ds:_himembase,eax
        add eax,ecx
        mov ds:_himemtop,eax

        jmp c_startf0                   ; go on to custom start

;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
; 16bit VCPI system data
;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
v_errmsg0       db      'Incompatible VCPI PIC mappings!!!',7,'$'

v_emmname       db      'EMMXXXX0',0    ; EMS device name
v_emshandle     dw      ?               ; one page allocated to turn on

v_oint32vect    dd      ?               ; old real mode INT32 vektor

v_pagedirseg    dw      ?               ; seg of page directory
v_pagebase      dw      0               ; first page of himem (*4)+1000h
v_pagetop       dw      0               ; top page of himem (*4)+1000h

v_ss_cr3        dd      ?               ; new CR3 for pmode (physical)
v_ss_gdtaddxptr dw      c_gdt32addx,0   ; ptr to GDT data for pmode
v_ss_idtaddxptr dw      c_idt32addx,0   ; ptr to IDT data for pmode
v_ss_ldtsel     dw      0               ; don't need no stinkin LDTs
v_ss_trsel      dw      30h             ; task state segment selector
v_ss_dest       dd      ?               ; start in pmode EIP
                dw      20h             ; start in pmode CS

;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
; 16bit VCPI system code
;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
;ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
v_retreal:                              ; VCPI return to real mode
        mov ebx,esp
        sub ebx,ds:realstackbase
        mov eax,code16
        push eax
        push eax
        push eax
        push eax
        push eax
        push ebx
        pushfd
        push eax
        dw 6866h,v_retreal2,0   ; 32bit PUSH offset v_retreal2
        mov ax,gs
        mov ds,ax
        mov ax,0de0ch
        call cs:vp_vcpientryaddx
;ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
v_retreal2:                             ; in real (V86) mode from VCPI
        mov bx,c_oldpicvectors
        call set8259vektorz
        movzx cx,bh
        xor bh,bh
        mov ax,0de0bh
        int 67h
        xor ax,ax                       ; restore INT 32 vektor
        mov es,ax
        mov eax,v_oint32vect
        mov es:[32h*4],eax
        jmp c_retreal2
;ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
v_int32:                                ; VCPI real INT32: EDX=off
        mov eax,offset vp_callpmode
;ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
; EAX=offset to jump to in code32
v_switchtopmode:                        ; VCPI switch to pmode
        mov cs:v_ss_dest,eax
        mov esi,offset v_ss_cr3
        add esi,cs:_code16a
        mov ax,0de0ch
        int 67h
;ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
v_exit2:                                ; VCPI exit (clean up pages)
        mov es,v_pagedirseg
        mov si,v_pagebase
        mov cx,v_pagetop
        sub cx,si
        jz short v_exit
v_exit2l0:
        mov edx,es:[si]
        and dx,0f000h
        mov ax,0de05h
        int 67h
        add si,4
        sub cx,4
        jnz v_exit2l0
;ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
v_exit:                                 ; VCPI exit (clean up EMS page)
        mov ah,45h
        mov dx,v_emshandle
        int 67h
        jmp exit
;ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
v_exiterr1:                             ; VCPI not enough low mem exit
        mov dx,offset errmsg1
        jmp exit16err
;ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
v_start:                                ; start continues from VCPI
        or ds:_sysbyte0,2               ; set system type VCPI byte
        mov exitrout,offset v_exit      ; set EMS cleanup exit
        mov ds:code16off,offset v_retreal       ; set VCPI return to real mode

        mov ax,0de0ah                   ; get PIC mappings
        int 67h
        mov bh,cl
        mov c_oldpicvectors,bx
        mov dx,offset v_errmsg0         ; chek for compatible PIC mapping
        cmp bl,bh
        je exit16err
        cmp bl,30h
        je exit16err
        cmp bh,30h
        je exit16err
        mov ax,2ch                      ; compatible, get highest needed num
        cmp al,bl
        ja short v_startf1
        mov al,bl
v_startf1:
        cmp al,bh
        ja short v_startf2
        mov al,bh
v_startf2:
        add al,7
        mov c_numofintvects,al
        lea eax,[eax*8+7]               ; set limit of IDT
        mov c_idt32addx,ax
        mov di,offset ds:intslotnum     ; set int slots needed
        call setintslots

v_startf0:
        movzx eax,c_idt32addx           ; get IDT size -1
        add eax,68h+1
        call pregetlomem                ; allocate TSS and IDT
        mov ebp,ds:_code32a             ; TSS location in mem to GDT
        lea ebx,[eax+ebp]
        or dword ptr ds:gdt32task[2],ebx
        add ebx,68h                     ; set up IDT addx
        mov dword ptr c_idt32addx[2],ebx
        mov eax,ds:_code16a             ; adjust mode switch structure
        add dword ptr v_ss_gdtaddxptr,eax
        add dword ptr v_ss_idtaddxptr,eax

        mov exitrout,offset v_exit2     ; set VCPI cleanup exit

        mov eax,ds:_lomembase           ; align lomem base on a page
        mov ebx,ds:_code32a
        add ebx,eax
        lea ecx,[ebx+0fffh]
        and ecx,0fffff000h
        sub ebx,ecx
        sub eax,ebx
        mov ds:_lomembase,eax
        mov ebp,ds:_lomemtop            ; get available low memory
        sub ebp,eax
        sub ebp,LOWMIN*1024             ; die if not enough
        jc v_exiterr1
        cmp ebp,8192
        jb v_exiterr1

        shld eax,ecx,28                 ; get segment and clear all pages
        mov v_pagedirseg,ax
        mov es,ax
        xor di,di
        mov cx,2048
        xor eax,eax
        rep stos dword ptr es:[di]
        mov di,1000h                    ; get VCPI pmode interface
        mov si,offset ds:gdt32vcpi
        mov ax,0de01h
        int 67h
        mov dword ptr ds:vp_vcpientryaddx,ebx

        mov v_pagebase,di               ; set up and go through allocation
        mov v_pagetop,di
        movzx eax,di
        sub eax,1000h
        shl eax,10
        mov ebp,ds:_code32a
        sub eax,ebp
        mov ds:_himembase,eax
        mov ebx,8192
v_startl2:
        mov ax,0de04h
        int 67h
        or ah,ah
        jnz short v_startl2d
        test di,0fffh
        jnz short v_startf4
        add ebx,4096
        cmp ebx,ebp
        ja v_exiterr1
v_startf4:
        and dx,0f000h
        or dl,7
        mov es:[di],edx
        add di,4
        jnc v_startl2
v_startl2d:
        mov v_pagetop,di
        lea si,[di-1000h]
        movzx eax,si
        shl eax,10
        sub eax,ebp             ; EBP=_code32a
        mov ds:_himemtop,eax
        sub di,v_pagebase
        cmp di,EXTMIN
        mov dx,offset errmsg3
        jb exit16err
        add ds:_lomembase,ebx

        movzx ebx,v_pagedirseg          ; set up physical addresses
        shr ebx,8
        mov eax,es:[ebx*4+1000h]
        mov v_ss_cr3,eax
        xor di,di
v_startl3:
        inc ebx
        mov eax,es:[ebx*4+1000h]
        and ax,0f000h
        or al,7
        stos dword ptr es:[di]
        sub si,1000h
        ja v_startl3

        xor ax,ax                       ; set VCPI real mode INT32
        mov es,ax
        mov eax,es:[32h*4]
        mov v_oint32vect,eax
        db 26h,66h,0c7h,6       ; MOV DWORD PTR ES:[32h*4],code16:d_int32
        dw 32h*4,d_int32,code16 ;
        mov di,offset ds:cp_int3_m0     ; modify INT32/33 custom pmode handler
        mov byte ptr [di],0e9h
        mov dword ptr [di+1],VCPIINT3_M0
        mov di,offset d_int32m0         ; modify INT32 DPMI real handler
        mov byte ptr [di],0e9h
        mov word ptr [di+1],VCPIINT32M0
        mov di,offset ds:dp_callpmodem1 ; modify INT32/33 DPMI pmode call
        mov byte ptr [di],0e9h
        mov dword ptr [di+1],VCPIINT32M1

        mov eax,offset v_startf3        ; offset to jump to in pmode
        jmp v_switchtopmode             ; duh?
;-----------------------------------------------------------------------------
v_startf3:                              ; in 16bit pmode
        mov di,offset v_startf5         ; set up segregs and stack
        jmp setpmoderegs
v_startf5:
        mov word ptr v_ss_dest[4],8     ; only enter 32bit pmode from now on
        mov ecx,offset c_idt32handler   ; int handler table
        mov dx,0db87h                   ; XCHG BX,BX for ret to pmode thing
        mov edi,dword ptr c_idt32addx[2]; get ptr to IDT
        sub edi,ds:_code32a
        jmp c_startf2                   ; continue in custom start

;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
; 16 bit custom system data
;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
c_idt16addx     dw      3ffh, 0,0       ; default real mode IDT addx&limit
c_idt32addx     dw      19fh, ?,?       ; 32bit IDT addx&limit
c_gdt32addx     dw      04fh, gdt32,0   ; 32bit GDT addx&limit

c_oldpicvectors dw      7008h           ; old PIC vals, maybe modified in VCPI
c_numofintvects db      33h             ; number of int vects needed -1, ditto

c_idt32handler  dw      cp_irq0,cp_irq1,cp_irq2,cp_irq3,cp_irq4,cp_irq5
                dw      cp_irq6,cp_irq7,cp_irq8,cp_irq9,cp_irqa,cp_irqb
                dw      cp_irqc,cp_irqd,cp_irqe,cp_irqf
                dw      cp_int33,cp_int32,cp_int31
                dw      cp_exc0,cp_exc1,cp_exc2,cp_exc3,cp_exc4,cp_exc5
                dw      cp_exc6,cp_exc7,cp_exc8,cp_exc9,cp_exca,cp_excb
                dw      cp_excc,cp_excd,cp_exce

;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
; 16 bit custom system code
;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
;ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
c_irqreal:                              ; custom real mode IRQ from pmode
        pushf
        push cs
        push offset icrealret
        jmp cs:tempaddx
;ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
c_retreal:                              ; custom return to real mode
        mov ax,ds
        mov es,ax
        mov fs,ax
        mov gs,ax
        mov ss,ax
        lidt fword ptr c_idt16addx
        mov eax,cr0
        and al,0feh
        mov cr0,eax
        db 0eah
        dw c_retreal2,code16
;ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
c_retreal2:                             ; in real mode from custom or VCPI
        mov ax,code16
        mov ds,ax
        mov es,ax
        mov fs,ax
        mov gs,ax
        sub ax,10h
        mov ss,ax
        mov esp,100h
        push exitrout
        mov bx,c_oldpicvectors          ; restore int controllers
        jmp set8259vektorz
;ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
c_start:                                ; start in custom system
        call enableA20                  ; enable that stupid A20 thingy

        mov ah,88h                      ; chek and get extended mem
        int 15h
        cmp ax,EXTMIN
        mov dx,offset errmsg3
        jb exit16err
        movzx eax,ax
        shl eax,10
        mov ebx,100000h
        sub ebx,ds:_code32a
        mov ds:_himembase,ebx
        add eax,ebx
        mov ds:_himemtop,eax
;ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
c_startf0:                              ; start continues from custom or XMS
        mov eax,2208h                   ; allocate TSS, IO bitmap, and IDT
        call pregetlomem
        push eax
        mov bx,2820h                    ; new base vektorz for int controllers
        call set8259vektorz
        pop ebx
        lgdt fword ptr c_gdt32addx      ; switch to pmode
        mov eax,cr0
        or al,1
        mov cr0,eax
        db 0eah
        dw c_startf1,20h
;-----------------------------------------------------------------------------
c_startf1:                              ; in 16bit pmode
        mov di,offset c_startf3         ; set up segregs and stack
        jmp setpmoderegs
c_startf3:
        lea eax,[ebx+4]                 ; addx of ESP0 in TSS
        mov ds:cp_tssesp0ptr,eax
        mov ebp,ds:_code32a             ; TSS location in mem to GDT
        lea eax,[ebx+ebp]
        or dword ptr ds:gdt32task[2],eax
        add eax,2068h                   ; set up IDT addx
        mov ecx,offset c_idt32addx
        mov dword ptr [ecx+2],eax
        lidt fword ptr [ecx]
        mov ax,30h                      ; load task register
        ltr ax

        mov dword ptr es:[ebx+8],10h    ; set up TSS stuff (EBX -> TSS)
        mov edi,104
        mov es:[ebx+102],di
        mov word ptr es:[ebx+100],0
        add edi,ebx                     ; fill IO bitmap with 0
        xor eax,eax
        mov ecx,800h
        rep stos dword ptr es:[edi]
        mov ecx,offset c_idt32handler   ; int handler table
        mov dx,0ffcdh                   ; INT FF for ret to pmode thing
;ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
; ECX->int vect table, EDI->IDT, DX=word for icrealret
c_startf2:                              ; start continues from custom or VCPI
        mov icrealret,dx                ; modify pmode return from real thingy
        mov ds:cp_idt32ptr,edi          ; set up IDT entries
        movzx esi,c_numofintvects
c_startl0:
        mov dword ptr es:[edi+esi*8],80000h+offset cp_excf
        mov dword ptr es:[edi+esi*8+4],8e00h
        sub si,1
        jnc c_startl0
        mov esi,33
c_startl1:
        movzx ebp,ds:intslotnum[si]
        mov ax,[ecx+esi*2]
        mov es:[edi+ebp*8],ax
        sub si,1
        jnc c_startl1

        pushfd                          ; set eflags: NT=0, IOPL=3
        pop eax
        and ah,0bfh
        or ah,30h
        push eax
        popfd

        jmp c_to32bit                   ; jump to jump to 32bit code

code16  ends

;²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
; 32bit pmode code
;²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
code32  segment para public use32
        assume cs:code32, ds:code32
        org 0

extrn   _main:near

public  _exit, _ret, _getmem, _getlomem, _gethimem, _lomemsize, _himemsize
public  _getirqmask, _setirqmask

public  v86r_eax, v86r_ebx, v86r_ecx, v86r_edx, v86r_esi, v86r_edi, v86r_ebp
public  v86r_ax, v86r_bx, v86r_cx, v86r_dx, v86r_si, v86r_di, v86r_bp
public  v86r_al, v86r_ah, v86r_bl, v86r_bh, v86r_cl, v86r_ch, v86r_dl, v86r_dh
public  v86r_ds, v86r_es, v86r_fs, v86r_gs
public  _selcode, _seldata, _selzero, _lomembase, _lomemtop, _himembase
public  _himemtop, _pspa, _code16a, _code32a, _getirqvect, _setirqvect
public  _sysbyte0

;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
; 32 bit common system data
;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
                dd      ?               ; scratch dword for VCPI tss esp0
_lomembase      dd      ?               ; low mem base for allocation
_lomemtop       dd      ?               ; top of low mem
_himembase      dd      0               ; high mem base for allocation
_himemtop       dd      0               ; top of high mem
_pspa           dd      ?               ; offset of start of PSP from 0
_code16a        dd      ?               ; offset of start of 16bit code from 0
_code32a        dd      ?               ; offset of start of 32bit code from 0
_selcode        dw      8               ; code segment selector
_seldata        dw      10h             ; data segment alias for code
_selzero        dw      18h             ; data segment starting at 0:0
_sysbyte0       db      0               ; system bits:
                                        ;  0-1: 0=raw, 1=XMS, 2=VCPI, 3=DPMI

_getirqvect     dd      cp_getirq       ; get IRQ handler offset routine addx
_setirqvect     dd      cp_setirq       ; set IRQ handler offset routine addx

gdt32           dq      0
gdt32code32     db      0ffh,0ffh,0,0,0,9ah,0cfh,0
gdt32data32     db      0ffh,0ffh,0,0,0,92h,0cfh,0
gdt32zero32     db      0ffh,0ffh,0,0,0,92h,0cfh,0
gdt32code16     db      0ffh,0ffh,0,0,0,9ah,0,0
gdt32data16     db      0ffh,0ffh,0,0,0,92h,0,0
gdt32task       db      0ffh,0ffh,0,0,0,89h,0,0
gdt32vcpi       dq      3 dup(?)

v86r_edi        label   dword           ; vregs for pmode<>real communication
v86r_di         dw      ?, ?            ;  needz to stay this way cuz its a
v86r_esi        label   dword           ;  stupid DPMI structure thingy
v86r_si         dw      ?, ?
v86r_ebp        label   dword
v86r_bp         dw      ?, ?
                dd      0
v86r_ebx        label   dword
v86r_bx         label   word
v86r_bl         db      ?
v86r_bh         db      ?, ?,?
v86r_edx        label   dword
v86r_dx         label   word
v86r_dl         db      ?
v86r_dh         db      ?, ?,?
v86r_ecx        label   dword
v86r_cx         label   word
v86r_cl         db      ?
v86r_ch         db      ?, ?,?
v86r_eax        label   dword
v86r_ax         label   word
v86r_al         db      ?
v86r_ah         db      ?, ?,?
v86r_flags      dw      ?
v86r_es         dw      ?
v86r_ds         dw      ?
v86r_fs         dw      ?
v86r_gs         dw      ?
                dd      0,0

oint1bvect      dd      ?               ; old int 1bh vektor (ctrl+break)
oirqmask        dw      ?               ; old port 21h and 0a1h masks
intslotnum      db      20h,21h,22h,23h,24h,25h,26h,27h,28h,29h,2ah,2bh,2ch,2dh,2eh,2fh
                db      33h,32h,31h,0,1,2,3,4,5,6,7,8,9,0ah,0bh,0ch,0dh,0eh

code16off       dw      c_retreal       ; offset in 16bit of exit function
code16sel       dw      20h             ; 16bit pmode code selector
data16sel       dw      28h             ; 16bit pmode data selector

nextmodestack   dw      (STAKLEN-STAKSAFE)*16   ; stack for next mode switch
tempaddx        dd      ?               ; temporary xfer addx, seg:off or off
realstackbase   dd      ?               ; linear ptr to beginning of codeend

;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
; 32 bit common system code
;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
;ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
; BX -> real mode offset of int32 handler
p_start:                                ; common 32bit start
        mov eax,gs:[1bh*4]              ; neutralize crtl+break
        mov oint1bvect,eax
        db 65h,67h,0c7h,6       ; MOV DWORD PTR GS:[1bh*4],code16:nullint
        dw 1bh*4,nullint,code16 ;
        in al,21h                       ; save old PIC masks
        mov ah,al
        in al,0a1h
        mov oirqmask,ax
        jmp _main                       ; go to main code

;°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
; Allocate any mem, (first cheks low, then high)
; In:
;   EAX - size requested
; Out:
;   CF=0 - memory allocated
;   CF=1 - not enough mem
;   EAX - linear pointer to mem or ?
;°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
_getmem:
        push eax
        call _getlomem
        jnc short getmemd
        pop eax
        jmp short _gethimem
getmemd:
        add esp,4
_ret:                                   ; generic RET instruction
        ret
;°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
; Allocate some low mem
; In:
;   EAX - size requested
; Out:
;   CF=0 - memory allocated
;   CF=1 - not enough mem
;   EAX - linear pointer to mem or ?
;°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
_getlomem:
        add eax,_lomembase
        cmp eax,_lomemtop
        ja short getmemerr
        xchg eax,_lomembase
        clc
        ret
getmemerr:
        stc
        ret
;°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
; Allocate some high mem
; In:
;   EAX - size requested
; Out:
;   CF=0 - memory allocated
;   CF=1 - not enough mem
;   EAX - linear pointer to mem or ?
;°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
_gethimem:
        add eax,_himembase
        cmp eax,_himemtop
        ja short getmemerr
        xchg eax,_himembase
        clc
        ret
;°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
; Get amount of free low mem
; Out:
;   EAX - number of bytes free
;°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
_lomemsize:
        mov eax,_lomemtop
        sub eax,_lomembase
        ret
;°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
; Get amount of free high mem
; Out:
;   EAX - number of bytes free
;°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
_himemsize:
        mov eax,_himemtop
        sub eax,_himembase
        ret
;°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
; Get status of IRQ mask bit
; In:
;   BL - IRQ num (0-15)
; Out:
;   AL - status: 0=enabled, 1=disabled
;°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
_getirqmask:
        push ax
        in al,0a1h
        mov ah,al
        in al,21h
        xchg cl,bl
        shr ax,cl
        xchg cl,bl
        and al,1
        mov [esp],al
        pop ax
        ret
;°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
; Set status of IRQ mask bit
; In:
;   BL - IRQ num (0-15)
;   AL - status: 0=enabled, 1=disabled
;°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
_setirqmask:
        push ax bx cx dx
        mov cl,bl
        mov bx,0fffeh
        movzx dx,al
        rol bx,cl
        shl dx,cl
        in al,0a1h
        mov ah,al
        in al,21h
        and ax,bx
        or ax,dx
        out 21h,al
        mov al,ah
        out 0a1h,al
        pop dx cx bx ax
        ret
;°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
; Exit to real mode
;°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
_exit:
        cli
        mov eax,oint1bvect              ; restore ctrl+break
        mov gs:[1bh*4],eax
        mov ax,oirqmask                 ; restore PIC masks
        out 0a1h,al
        mov al,ah
        out 21h,al
        push code16sel                  ; go to 16bit pmode exit code
        push code16off
        mov ds,data16sel
        db 66h,0cbh             ; 16bit RETF

;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
; 32 bit DPMI system data
;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
dp_switchaddx   df      ?               ; switch to real mode addx
dp_saveaddx     df      ?               ; save/restore state addx
dp_savelen      dw      0,0             ; length of state buffer
dp_savedstakoff dd      ?               ; current saved stack offset
dp_savedstaksel dw      ?               ; current saved stack selector

dp_ointbuf      df      18 dup(?)       ; saved interrupt addx buffer

;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
; 32 bit DPMI system code
;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
;ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
vp_callpmode:                           ; VCPI pmode call from real mode
        mov esp,ebx
        mov ax,10h
        mov ds,ax
        mov es,ax
        mov ss,ax
;ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
dp_callpmode:                           ; DPMI pmode call from real mode
        cld
        mov ax,ds
        mov fs,ax
        mov gs,_selzero
        mov eax,v86r_eax
        mov ecx,v86r_ecx
        mov edx,v86r_edx
        mov ebx,v86r_ebx
        mov esi,v86r_esi
        mov edi,v86r_edi
        mov ebp,v86r_ebp
        push offset dp_callpmoded
        push tempaddx
dp_callpmodeif  db      ?       ; CLI or STI
        ret
dp_callpmoded:
        cli
        pushf
        pop v86r_flags
        mov v86r_eax,eax
        mov v86r_ecx,ecx
        mov v86r_edx,edx
        mov v86r_ebx,ebx
        mov v86r_esi,esi
        mov v86r_edi,edi
        mov v86r_ebp,ebp
        mov ecx,_code16a
        movzx ebx,gs:d_savedstakoff[ecx]
        mov dx,gs:d_savedstakseg[ecx]
        mov ax,code16
dp_callpmodem1:                         ; modified under VCPI
VCPIINT32M1=(vp_callpmoded-5)-$         ;
        mov cx,dx
        mov si,ax
        mov edi,offset d_int32d
        jmp dp_switchaddx
;-----------------------------------------------------------------------------
vp_callpmoded:                          ; VCPI done with pmode call
        push eax
        push eax
        push eax
        push eax
        push edx
        push ebx
        pushfd
        push eax
        db 68h                          ; 32bit PUSH offset v_retreal2
        dw d_int32d2,0                  ;
        mov ax,gs
        mov ds,ax
        mov ax,0de0ch
        call cs:vp_vcpientryaddx
;ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
dp_int32:                               ; DPMI INT 32h: CX:DX=seg:off
        pushad
        shl ecx,16
        mov cx,dx
        mov bp,offset callreal
        mov dl,1
        jmp short dp_int3_
;ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
dp_int33:                               ; DPMI INT 33h: AL=int num
        pushad
        movzx eax,al
        mov ecx,gs:[eax*4]
        mov bp,offset intreal
        xor dl,dl
;-----------------------------------------------------------------------------
dp_int3_:                               ; int or call to real mode
        mov ax,900h
        int 31h
        mov tempaddx,ecx
        push ax
        and al,dl
        add al,0fah
        mov ecx,_code16a
        mov gs:icrealif[ecx],al
        push dp_savedstakoff
        push dp_savedstaksel
        movzx ebx,nextmodestack
        lea eax,[ebx-STAKSAFE*16]
        mov nextmodestack,ax
        mov ax,ss
        mov es,ax
        sub esp,dword ptr dp_savelen
        mov edi,esp
        xor al,al
        call dp_saveaddx
        mov dp_savedstakoff,esp
        mov dp_savedstaksel,ss
        mov dx,codeend
        mov ax,v86r_ds
        mov cx,v86r_es
        movzx edi,bp
        mov si,code16
        jmp dp_switchaddx
dp_int3_d:
        mov edi,esp
        mov al,1
        call dp_saveaddx
        add esp,dword ptr dp_savelen
        pop dp_savedstaksel
        pop dp_savedstakoff
        add nextmodestack,STAKSAFE*16
        mov bx,v86r_flags
        mov ax,[esp+42]
        and ax,not 8d5h
        and bx,8d5h
        or ax,bx
        mov [esp+42],ax
        pop ax
        mov ah,9
        int 31h
        mov ax,ds
        mov es,ax
        mov fs,ax
        mov gs,_selzero
        popad
        iretd
;ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
; DPMI IRQ redirectors (needed to make all IRQ vector selectors = CS)
dp_irq0:
        jmp cs:dp_ointbuf[0]
dp_irq1:
        jmp cs:dp_ointbuf[6]
dp_irq2:
        jmp cs:dp_ointbuf[12]
dp_irq3:
        jmp cs:dp_ointbuf[18]
dp_irq4:
        jmp cs:dp_ointbuf[24]
dp_irq5:
        jmp cs:dp_ointbuf[30]
dp_irq6:
        jmp cs:dp_ointbuf[36]
dp_irq7:
        jmp cs:dp_ointbuf[42]
dp_irq8:
        jmp cs:dp_ointbuf[48]
dp_irq9:
        jmp cs:dp_ointbuf[54]
dp_irqa:
        jmp cs:dp_ointbuf[60]
dp_irqb:
        jmp cs:dp_ointbuf[66]
dp_irqc:
        jmp cs:dp_ointbuf[72]
dp_irqd:
        jmp cs:dp_ointbuf[78]
dp_irqe:
        jmp cs:dp_ointbuf[84]
dp_irqf:
        jmp cs:dp_ointbuf[90]

;°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
; DPMI get IRQ handler offset
; In:
;   BL - IRQ num (0-0fh)
; Out:
;   EDX - offset of IRQ handler
;°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
dp_getirq:
        push ax bx cx
        movzx bx,bl
        mov bl,intslotnum[bx]
        mov ax,204h
        int 31h
        pop cx bx ax
        ret
;°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
; DPMI set IRQ handler offset
; In:
;   BL - IRQ num (0-0fh)
;   EDX - offset of IRQ handler
;°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
dp_setirq:
        push ax bx cx
        movzx bx,bl
        mov bl,intslotnum[bx]
        mov cx,cs
        mov ax,205h
        int 31h
        pop cx bx ax
        ret

;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
; 32 bit VCPI system data
;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
vp_vcpientryaddx df      3800000000h    ; VCPI entry point in pmode

;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
; 32 bit VCPI system code
;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
;ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
vp_int3_:                               ; VCPI int or call to real mode
        mov ax,18h                      ; VCPI switch to real mode routine
        mov ds,ax
        mov ax,0de0ch
        call cs:vp_vcpientryaddx

;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
; 32 bit custom system data
;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
cp_tssesp0ptr   dd      0               ; ptr to ESP0 in TSS, or null in VCPI
cp_idt32ptr     dd      ?               ; ptr to 32bit IDT

cp_v86irqnum    db      ?               ; int num of IRQ for V86 mode
cp_savedstakoff dd      ?               ; current saved stack offset

;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
; 32 bit custom system code
;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
;ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
cp_int31:                               ; INT 31h: AX=900h,901h,902h
        cmp al,1
        mov al,[esp+9]
        jb short cp_int31f0
        ja short cp_int31f1
        or byte ptr [esp+9],2
        jmp short cp_int31f1
cp_int31f0:
        and byte ptr [esp+9],0fdh
cp_int31f1:
        shr al,1
        and al,1
        iretd
;ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
cp_int32:                               ; INT 32h: CX:DX=seg:off
        pushad
        shl ecx,16
        mov cx,dx
        mov ebp,offset callreal
        mov dl,1
        jmp short cp_int3_
;ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
cp_int33:                               ; INT 33h: AL=int num
        pushad
        movzx eax,al
        mov ecx,gs:[eax*4]
        mov ebp,offset intreal
        xor dl,dl
;-----------------------------------------------------------------------------
cp_int3_:                               ; int or call to real mode
        mov tempaddx,ecx
        mov edi,[esp+40]
        shld eax,edi,23
        and al,dl
        add al,0fah
        mov ecx,_code16a
        mov gs:icrealif[ecx],al
        movzx esi,nextmodestack
        lea eax,[esi-STAKSAFE*16]
        mov nextmodestack,ax
        mov ebx,cp_tssesp0ptr
        push dword ptr [ebx]
        add eax,realstackbase
        mov [ebx],eax
        push cp_savedstakoff
        mov cp_savedstakoff,esp
        xor eax,eax
        push eax
        push eax
        mov ax,v86r_ds
        push eax
        mov ax,v86r_es
        push eax
        mov ax,codeend
        push eax
        push esi
        or edi,20000h
        and di,0fdffh
        push edi
        db 68h                  ; 32bit PUSH code16
        dd code16               ;
        push ebp
cp_int3_m0:                             ; modified under VCPI
VCPIINT3_M0=(vp_int3_-5)-$              ;
        iretd
        db 4 dup(?)             ; buffer for modifiable instruction
cp_int3_d:
        mov ax,18h
        mov gs,ax
        mov ax,10h
        mov ds,ax
        mov es,ax
        mov fs,ax
        mov ss,ax
        mov esp,cp_savedstakoff
        pop cp_savedstakoff
        mov ebx,cp_tssesp0ptr
        pop dword ptr [ebx]
        mov bx,v86r_flags
;-----------------------------------------------------------------------------
cp_int3_d2:                             ; done with INT32/33 from real or V86
        add nextmodestack,STAKSAFE*16
        mov ax,[esp+40]
        and ax,not 8d5h
        and bx,8d5h
        or ax,bx
        mov [esp+40],ax
        popad
        iretd
;ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
cp_rint32:                              ; custom real INT 32h: EDX=off
        mov gs,cx
        mov ax,ss
        mov ds,ax
        mov es,ax
        mov fs,ax
        sub nextmodestack,STAKSAFE*16
        mov eax,v86r_eax
        mov ecx,v86r_ecx
        mov edx,v86r_edx
        mov ebx,v86r_ebx
        mov esi,v86r_esi
        mov edi,v86r_edi
        mov ebp,v86r_ebp
        push word ptr [esp+40]
        popf
        cld
        call dword ptr [esp+20]
        cli
        mov v86r_eax,eax
        mov v86r_ecx,ecx
        mov v86r_edx,edx
        mov v86r_ebx,ebx
        mov v86r_esi,esi
        mov v86r_edi,edi
        mov v86r_ebp,ebp
        pushf
        pop bx
        jmp cp_int3_d2
;ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
cp_excd:                                ; general protection violation
        test byte ptr [esp+14],2
        jnz short cp_excdv86
        pushad
        mov al,0dh
        jmp cp_exc
;-----------------------------------------------------------------------------
cp_excdv86:                             ; violation from V86 mode
        add esp,4
        pushad
        mov cx,18h
        mov ds,cx
        movzx ebx,word ptr [esp+36]
        shl ebx,4
        add ebx,[esp+32]
        inc word ptr [esp+32]
        mov al,[ebx]
        mov edx,3
        cmp al,0cch
        je short cp_v86int
        mov dl,4
        cmp al,0ceh
        je short cp_v86int
        inc word ptr [esp+32]
        mov dl,[ebx+1]
        cmp dl,32h
        je cp_rint32
        cmp dl,0ffh
        je cp_int3_d
;-----------------------------------------------------------------------------
cp_v86int:                              ; need to simulate a real mode int
        movzx ebx,word ptr [esp+48]
        shl ebx,4
        sub word ptr [esp+44],6
        add ebx,[esp+44]
        mov ax,[esp+40]
        mov [ebx+4],ax
        and ah,0fch
        mov [esp+41],ah
        mov ax,[esp+36]
        mov [ebx+2],ax
        mov ax,[esp+32]
        mov [ebx],ax
        mov eax,[edx*4]
        mov [esp+32],ax
        shr eax,16
        mov [esp+36],ax
        popad
        iretd
;ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
; all exceptions except 0dh. some are terminal, others are redirected.
cp_exc0:
        push ax
        xor al,al
        jmp cp_irq
cp_exc1:
        push ax
        mov al,1
        jmp cp_irq
cp_exc2:
        push ax
        mov al,2
        jmp cp_irq
cp_exc3:
        push ax
        mov al,3
        jmp cp_irq
cp_exc4:
        push ax
        mov al,4
        jmp cp_irq
cp_exc5:
        push ax
        mov al,5
        jmp cp_irq
cp_exc6:
        pushad
        mov al,6
        jmp short cp_exc
cp_exc7:
        push ax
        mov al,7
        jmp cp_irq
cp_exc8:
        pushad
        mov al,8
        jmp short cp_exc
cp_exc9:
        pushad
        mov al,9
        jmp short cp_exc
cp_exca:
        pushad
        mov al,0ah
        jmp short cp_exc
cp_excb:
        pushad
        mov al,0bh
        jmp short cp_exc
cp_excc:
        pushad
        mov al,0ch
        jmp short cp_exc
cp_exce:
        pushad
        mov al,0dh
        jmp short cp_exc
cp_excf:
        pushad
        mov al,-1
;-----------------------------------------------------------------------------
cp_exc:                                 ; main exception handler
	mov ax,10h
        mov ds,ax
        mov es,ax
        mov fs,ax
        mov gs,_selzero
	cld
        jmp _exit
;ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
; IRQ redirector between modes
cp_irq0:
        push ax
        mov al,8
        jmp short cp_irq
cp_irq1:
        push ax
        mov al,9
        jmp short cp_irq
cp_irq2:
        push ax
        mov al,0ah
        jmp short cp_irq
cp_irq3:
        push ax
        mov al,0bh
        jmp short cp_irq
cp_irq4:
        push ax
        mov al,0ch
        jmp short cp_irq
cp_irq5:
        push ax
        mov al,0dh
        jmp short cp_irq
cp_irq6:
        push ax
        mov al,0eh
        jmp short cp_irq
cp_irq7:
        push ax
        mov al,0fh
        jmp short cp_irq
cp_irq8:
        push ax
        mov al,70h
        jmp short cp_irq
cp_irq9:
        push ax
        mov al,71h
        jmp short cp_irq
cp_irqa:
        push ax
        mov al,72h
        jmp short cp_irq
cp_irqb:
        push ax
        mov al,73h
        jmp short cp_irq
cp_irqc:
        push ax
        mov al,74h
        jmp short cp_irq
cp_irqd:
        push ax
        mov al,75h
        jmp short cp_irq
cp_irqe:
        push ax
        mov al,76h
        jmp short cp_irq
cp_irqf:
        push ax
        mov al,77h
;-----------------------------------------------------------------------------
cp_irq:                                 ; select IRQ type: real->v86 or p->v86
        mov ss:cp_v86irqnum,al
        pop ax
        test byte ptr [esp+10],2
        jnz short cp_irqv86
        push ds es fs gs                ; a real mode IRQ in protected mode
        pushfd
        push cs
        push offset cp_irqpd
        pushad
        mov ax,ss
        mov ds,ax
        mov gs,_selzero
        movzx eax,cp_v86irqnum
        mov ecx,gs:[eax*4]
        mov ebp,offset c_irqreal
        jmp cp_int3_
cp_irqpd:
        pop gs fs es ds
        iretd
;-----------------------------------------------------------------------------
cp_irqv86:                              ; an IRQ from V86 mode
        pushad
        mov ax,18h
        mov ds,ax
        movzx edx,ss:cp_v86irqnum
        jmp cp_v86int

;°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
; Custom get IRQ handler offset
; In:
;   BL - IRQ num (0-0fh)
; Out:
;   EDX - offset of IRQ handler
;°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
cp_getirq:
        push ebx
        pushf
        cli
        movzx ebx,bl
        mov bl,intslotnum[ebx]
        lea ebx,[ebx*8]
        add ebx,cp_idt32ptr
        mov dx,[ebx+6]
        shl edx,16
        mov dx,[ebx]
        popf
        pop ebx
        ret
;°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
; Custom set IRQ handler offset
; In:
;   BL - IRQ num (0-0fh)
;   EDX - offset of IRQ handler
;°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
cp_setirq:
        push ebx
        pushf
        cli
        movzx ebx,bl
        mov bl,intslotnum[ebx]
        lea ebx,[ebx*8]
        add ebx,cp_idt32ptr
        mov [ebx],dx
        shr edx,16
        mov [ebx+6],dx
        popf
        pop ebx
        ret

code32  ends

;²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
; End of program (must be at end of program or you will suffer)
;²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
codeend segment para stack use32 'stack'
db STAKLEN*16 dup(?)
codeend ends
        end     start16

