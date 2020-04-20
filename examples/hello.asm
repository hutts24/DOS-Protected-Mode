;************************************************************************/
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

        mov	ax,4C00h		; exit to DOS
	int	21h

_start_	endp

_text	ends

_data	segment	para public use32 'data'

hellomsg db	'Hello world!!!!!!!!',0DH,0AH,'$'
	
_data	ends

_stack	segment byte stack use32 'stack'

	db	8192 dup (?)

_stack	ends

	end _start_
