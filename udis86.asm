; Udis86 Disassembly Module for Forthx64
;
; (c) Copyright 2013-2018 by John F. Healy. All rights reserved.
;
; udis86.asm is distributed under the terms of the 2-clause BSD License.
; Copyright (c) 2018, John F. Healy <healyjohnf@gmail.com>
; All rights reserved.
; Redistribution and use in source and binary forms, with or without modification,
; are permitted provided that the following conditions are met:
; 1. Redistributions of source code must retain the above copyright notice
;    this list of conditions and the following disclaimer.
; 2. Redistributions in binary form must reproduce the above copyright notice,
;    this list of conditions and the following disclaimer in the documentation
;    and/or other materials provided with the distribution.
; 
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
; ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;
; This module provides x86_64 disassembly capability for Forthx64. Code at
; arbitrary locations can be disassebled in either Intel or AT&T syntax.
; The syntax is specified by Forthx64 System Flag #47 - AT&T when set,
; Intel when clear. Default clear. To set to AT&T use `at&t`. To set back
; to Intel, use `intel`. Note that the Forthx64 listing uses Intel syntax.
; The disassembly format is one line per opcode, each line beginning with the
; absolute address of the instruction, followed by its address relative to the
; beginning of the disassembly.
; Invalid opcodes will either be flagged as invalid or produce bogus instructions.
;
; This module is included by default in the Forthx64 source file in the section
; titled "Include external modules here". To exclude it, simply comment out
; the line:
;
;   %include "udis86.asm"
;
; If the file is included, add -ludis86 to the compile command line to link
; the library.
;
; Udis86 Library Functions

    extern  ud_init, ud_set_input_buffer, ud_set_mode
    extern  ud_set_pc, ud_translate_intel, ud_translate_att
    extern  ud_set_syntax, ud_disassemble, ud_insn_asm
    extern  ud_insn_len, ud_insn_off, ud_insn_hex, ud_insn_ptr

; Structure for Udis86 - 80 quads

udstr0: times 80 dq 0

; udstruct - Address of Udis86 structure.

head    "udstruct", saco
udstc:  dq  udstc0
textm
udstc0: lea   rdi, [udstr0]
        sub   r14, 8
        mov   [r14], rdi
        nextm
datam

; at&t - Set Udis86 syntax to AT&T.

head    "at&t", udso
atat:   dq  atat0
textm
atat0:  bts   qword[flgs0], 47          ; Set AT&T syntax flag
        nextm
datam

; intel - Set Udis86 syntax to Intel.

head    "intel", udso
intl:   dq  intl0
textm
intl0:  btr   qword[flgs0], 47          ; Clear AT&T syntax flag
        nextm
datam

; udis  ( addr1 addr2 n1 --- addr2 n2 ) - Disassemble n1 bytes of code
; at addr1 to addr2 as an array of n2 counted strings, which are padded
; with blanks to an 8-byte boundary.

head    "udis", udso
udis:   dq  udis0
textm
udis0:  sub   r15, 8                    ; Free up r12
        mov   [r15], r12
        mov   r12, [r14+8]              ; Save original destination address
        xor   rbx, rbx                  ; Zero string count

; Initialize structure.

        lea   rdi, [udstr0]

        CFCm  ud_init

; Establish input buffer.

        lea   rdi, [udstr0]
        mov   rsi, [r14+16]             ; Get source address
        mov   rdx, [r14]                ; Set buffer size

        CFCm  ud_set_input_buffer

; Set mode to 64-bit.

        lea   rdi, [udstr0]
        mov   rsi, 64

        CFCm  ud_set_mode

; Set program counter to zero.

        lea   rdi, [udstr0]
        mov   rsi, 0

        CFCm  ud_set_pc

        bt    qword[flgs0], 47          ; Test syntax flag
        jc    udis1                     ; Branch if AT&T

; Set syntax to Intel NASM.

        lea   rdi, [udstr0]
        lea   rsi, [ud_translate_intel] ; Translate Intel

        CFCm  ud_set_syntax

        jmp   udis2

; Set syntax to Intel AT&T.

udis1:  lea   rdi, [udstr0]
        lea   rsi, [ud_translate_att]   ; Translate AT&T

        CFCm  ud_set_syntax

; Disassemble single line.

udis2:  lea   rdi, [udstr0]

        CFCm  ud_disassemble

        mov   rbp, rax                  ; Save bytes disassembled

; Get buffer address.

        lea   rdi, [udstr0]

        CFCm  ud_insn_asm

        mov   rsi, rax                  ; Copy buffer address

; Get pc for instruction.

        lea   rdi, [udstr0]

        CFCm  ud_insn_off

        mov   rdi, [r14+8]              ; Get destination address

        mov   [rdi], rax                ; Install pc offset
        add   rdi, 8                    ; Advance destination past offset
        add   qword[r14+8], 8           ; Add 8 for offset

        mov   rcx, [rsi-8]              ; Get length of string
        add   [r14+8], rcx              ; Advance destination address
        add   qword[r14+8], 8           ; Add 8 for length quad
        add   rbx, 1                    ; Increment # of strings
        mov   [rdi], rcx                ; Install string length
        add   rdi, 8                    ; Advance destination past length

; Pad string with blanks to 8-byte boundary.

        mov   r8, rcx                   ; Copy string length
        and   r8, 7                     ; Determine pad bytes
        je    udis3                     ; Branch if none needed
        neg   r8
        add   r8, 8
        add   [r14+8], r8               ; Add to destination address

        lea   rdx, [pad0]
        jmp   [rdx+r8*8-8]

pad0:   dq    pad1,pad2,pad3,pad4,pad5,pad6,pad7

pad1:   mov   byte[rdi+rcx], 32         ; Pad one byte
        jmp   udis3

pad2:   mov   byte[rdi+rcx+1], 32       ; Pad two bytes
        mov   byte[rdi+rcx], 32
        jmp   udis3

pad3:   mov   byte[rdi+rcx+2], 32       ; Pad three bytes
        mov   byte[rdi+rcx+1], 32
        mov   byte[rdi+rcx], 32
        jmp   udis3

pad4:   mov   byte[rdi+rcx+3], 32       ; Pad four bytes
        mov   byte[rdi+rcx+2], 32
        mov   byte[rdi+rcx+1], 32
        mov   byte[rdi+rcx], 32
        jmp   udis3

pad5:   mov   byte[rdi+rcx+4], 32       ; Pad five bytes
        mov   byte[rdi+rcx+3], 32
        mov   byte[rdi+rcx+2], 32
        mov   byte[rdi+rcx+1], 32
        mov   byte[rdi+rcx], 32
        jmp   udis3

pad6:   mov   byte[rdi+rcx+5], 32       ; Pad six bytes
        mov   byte[rdi+rcx+4], 32
        mov   byte[rdi+rcx+3], 32
        mov   byte[rdi+rcx+2], 32
        mov   byte[rdi+rcx+1], 32
        mov   byte[rdi+rcx], 32
        jmp   udis3

pad7:   mov   byte[rdi+rcx+6], 32       ; Pad seven bytes
        mov   byte[rdi+rcx+5], 32
        mov   byte[rdi+rcx+4], 32
        mov   byte[rdi+rcx+3], 32
        mov   byte[rdi+rcx+2], 32
        mov   byte[rdi+rcx+1], 32
        mov   byte[rdi+rcx], 32
        jmp   udis3

; Copy string to destination.

udis3:  mov   al, [rsi+rcx-1]
        mov   [rdi+rcx-1], al
        sub   rcx, 1
        jne   udis3

        sub   [r14], rbp                ; Decrement code bytes
        ja    udis2                     ; Loop if not zero or less

        mov   rdx, [r14+8]              ; Get final destination address
        sub   rdx, r12                  ; Subtract original address
        mov   [r12-8], rdx              ; Install total array length
        add   r14, 8                    ; Pop one stack item
        mov   [r14+8], r12              ; Return destination address
        mov   [r14], rbx                ; Return number of strings

        mov   r12, [r15]                ; Restore r12
        add   r15, 8
        nextm
datam

; disasm - ( addr n --- ) Disassemble n bytes of code at address and output
; it to the screen.
; Note that the address must be aligned on an instruction boundary for results
; to be valid. This can be somewhat involved since the size of x86_64 instructions
; can vary from 1 to 15 bytes.

; : disasm   base @ >a hex
;             over swap axb0 clear-buffer swap udis
;             0 do dup @ dup 3 pick + cr 6 u.lz space 4 u.lz 8+
;             dup count dup 8+ rnd>8 >a 4 spaces type a> + loop
;            2drop a> base ! ;

head    "disasm", udso
disasm: dq  docl0
        dq  base,qat,toa,hex,over,swp
        dq  axbn,clrbuf,swp
        dq  udis,zero,ddo,xdsm2
xdsm1:  dq  dupl,qat,dupl,three,pick
        dq  plus,cr,lit,6,udotlz,space
        dq  four,udotlz,eighp,dupl
        dq  count,dupl,eighp,rute,toa
        dq  four,spaces,typ,froma,plus
        dq  _loop,xdsm1
xdsm2:  dq  space,ddrop,froma,base,stor
        dq  semis

; show-code - ( CFA --- ) Disassemble code pointed to by the code-field address
; of a dictionary entry. Usage:  ' <entry> show-code

; : show-code   @ dup 8- @ disasm ;

head    "show-code", udso
shocod: dq  docl0
        dq  qat,dupl,eighm,qat,disasm,semis

; ascii>opcode - ( addr n1 --- u n2 ) Convert ASCII string of length n1 at addr,
; which represents an x86_64 hexadecimal opcode, to an actual opcode on the stack
; and put its byte-length on top of the stack. The string can be no more than 30
; bytes, resulting in the maximum of 15 bytes for the opcode. All characters must
; translate to hexadecimal digits in order to be valid. A leading zero will be
; assumed if the number of characters is odd. The result will be in little-endian
; byte order and may or may not be a valid x86_64 instruction.

head    "ascii>opcode", udso, hidden
asctop: dq  asctp0
textm
asctp0: mov   rdi, [r14+8]              ; Get addr of string
        mov   rdx, [r14]                ; Get byte count
        add   r14, 8                    ; Pop 1 item
        cmp   rdx, 31                   ; Test for maximum
        jnc   asctp2                    ; Not valid if larger

        mov   qword[r14], 0             ; Clear for result
        lea   rbp, [r14]                ; Load result pointer
        xor   rcx, rcx                  ; Zero index register
        lea   rsi, [hextb0]             ; Get table base address
        shr   rdx, 1                    ; Halve character count
        mov   r9, rdx                   ; Save copy
        je    asctp2                    ; Not valid if single byte

        jnc   asctp1                    ; Branch if even

; There should not be an odd number of characters in the string, but if there is,
; a leading zero is assumed.

        movzx rax, byte[rdi+rcx]        ; Get 1st byte
        movzx rbx, byte[rsi+rax]        ; Get hex digit
        cmp   bl, -1                    ; See if valid hex digit
        je    asctp1                    ; Branch if not
        shl   rbx, 4
        mov   [rbp], bl                 ; Install byte on stack
        add   rcx, 1                    ; Increment string pointer
        add   rbp, 1                    ; Decrement byte pointer

asctp1: movzx rax, byte[rdi+rcx]        ; Get 1st byte of pair
        movzx rbx, byte[rsi+rax]        ; Get hex digit
        cmp   bl, -1                    ; See if valid hex digit
        je    asctp2                    ; Branch if not

        shl   rbx, 4                    ; Shift left 1 nibble
        add   rcx, 1                    ; Increment string pointer
        movzx rax, byte[rdi+rcx]        ; Get 2nd byte of pair
        movzx r8, byte[rsi+rax]         ; Get hex digit
        cmp   r8b, -1                   ; See if valid hex digit
        je    asctp2                    ; Branch if not

        or    rbx, r8                   ; Or into low order nibble
        mov   [rbp], bl                 ; Install byte on stack
        add   rcx, 1                    ; Increment string pointer
        add   rbp, 1                    ; Decrement byte pointer
        sub   rdx, 1                    ; Decrement count
        jne   asctp1                    ; Loop for count

        jmp   asctp3                    ; Exit

asctp2: add   r14, 8                    ; Pop the stack

        throwm 125                      ; Throw invalid opcode

asctp3: sub   r14, 8
        mov   [r14], r9                 ; Return opcode byte length
        nextm
datam

; opcode" - ( --- opcode n) Move the string that follows in the input stream
; to the auxiliary buffer until the next quotation mark is encountered and
; convert it directly from ASCII to a quad on the stack followed by its byte-length.
; To disassemble the opcode, use `.opcode`.
;
; : opcode"   axb0 " dlmword dup 8- @ ascii>opcode ;

head    'opcode"', udso
opcdq:  dq  docl0
        dq  axbn,dbqt,dlmwrd,dupl
        dq  eighm,qat,asctop,semis

; .opcode - ( opcode n --- ) Disassemble the opcode entered by `opcode"`.

head    ".opcode", udso
dtopc:  dq  dtop0
textm
dtop0:  lea   rdi, [dstack0]            ; Check the stack for two values
        sub   rdi, r14                  ; Subtract current stack pointer
        cmp   rdi, 16                   ; See if there are 2 quads
        jnc   dtop1                     ; Branch if so

        throwm 68

dtop1:

; Initialize structure.

        lea   rdi, [udstr0]

        CFCm  ud_init

; Establish input buffer.

        lea   rdi, [udstr0]
        mov   rdx, [r14]                ; Set buffer size
        add   r14, 8                    ; Pop 1 stack item
        mov   rsi, r14                  ; Set source address

        CFCm  ud_set_input_buffer

; Set mode to 64-bit.

        lea   rdi, [udstr0]
        mov   rsi, 64

        CFCm  ud_set_mode

        bt    qword[flgs0], 47          ; Test syntax flag
        jc    dtop2                     ; Branch if AT&T

; Set syntax to Intel NASM.

        lea   rdi, [udstr0]
        lea   rsi, [ud_translate_intel] ; Translate Intel

        CFCm  ud_set_syntax

        jmp   dtop3

; Set syntax to Intel AT&T.

dtop2:  lea   rdi, [udstr0]
        lea   rsi, [ud_translate_att]   ; Translate AT&T

        CFCm  ud_set_syntax

; Disassemble single line.

dtop3:  lea   rdi, [udstr0]

        CFCm  ud_disassemble

; Get buffer address.

        lea   rdi, [udstr0]

        CFCm  ud_insn_asm

        mov   rdi, [sdout]              ; Standard handle
        mov   rsi, rax                  ; Address of string
        mov   rdx, [rsi-8]              ; Length of string

        OSCm  write

        add   r14, 8                    ; Pop stack
        nextm
datam
