; ptrace Interface for Forthx64
;
; (c) Copyright 2018 by John F. Healy. All rights reserved.
;
; ptrace.asm is distributed under the terms of the 2-clause BSD License.
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
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT O
;
; This module provides a ptrace interface for debugging Forthx64.
; This version uses Linux system calls with error handling.
; Symbolic constants representing requests are passed in through the forthx.c
; file.
;
; This module is included by default in the Forthx64 source file in the section
; titled "Include external modules here". To exclude it, simply comment out
; the line:
;
;   %include "ptrace.asm"
;
; No changes need to be made to the assemble, compile, and link command line.
;
; Note: To use these functions, Forthx64 must be run as root,
; unless the following changes are made:
;
; Edit /etc/sysctl.d/10-ptrace.conf and change the line:
;
;   kernel.yama.ptrace_scope = 1
;
; To read:
;
;   kernel.yama.ptrace_scope = 0

; External References

    extern  ptrace
    extern  pt_traceme, pt_peektext, pt_poketext
    extern  pt_getregs, pt_getfpregs, pt_setregs
    extern  ptrace_setfpregs, pt_singlestep, pt_cont
    extern  pt_attach,pt_detach, pt_seize, pt_interrupt

    extern  waitid
    extern  wstopped, p_pid

; Place to put PTRACE_GETREGS structure - 256 bytes in extent.

gtrgs0: times 32 dq 0

; Place to put PTRACE_GETFPREGS structure - 128 quads in extent.

gfrgs0: times 128 dq 0

; Place to put sys_waitid info structure. The siginfo_t structure is 128 bytes
; in extent.

infp0: times 16 dq 0

; infop - Address of sys_waitid info structure.

head    "infop", saco
infop:  dq  infop0
textm
infop0: lea   rdi, [infp0]
        sub   r14, 8
        mov   [r14], rdi
        nextm
datam

; ptregs - Address of PTRACE_GETREGS structure.

head    "ptregs", saco
ptrgs:  dq  ptrgs0
textm
ptrgs0: lea   rdi, [gtrgs0]
        sub   r14, 8
        mov   [r14], rdi
        nextm
datam

; pt_waitid - ( tpid --- result ) Return status of child process.

head    "pt_waitid"
ptwid:  dq  ptwid0
textm
ptwid0: mov   rdi, [p_pid]              ; P_PID wait for child pid
        mov   rsi, [r14]                ; Get tracee pid
        lea   rdx, [infp0]              ; Address of info structure
        mov   r10, [wstopped]           ; WSTOPPED

; Failure to null the 5th argument results in a crash.

        xor   r8, r8

        OSCErrm waitid

        mov   [r14], rax
        nextm
datam

; pt_attach - ( tpid --- ) Attach to the process whose ID is passed
; on the stack.

head    "pt_attach"
ptat:   dq  ptat0
textm
ptat0:  mov   rdi, [pt_attach]          ; Load request
        mov   rsi, [r14]                ; Get tracee pid
        xor   rdx, rdx                  ; Null address field
        xor   r10, r10                  ; Null data field

        OSCErrm ptrace

; Note: The process ID in rsi apparently persists through the call.

        mov   rdi, [p_pid]              ; P_PID - wait for child pid
        lea   rdx, [infp0]              ; Load address of siginfo_t structure
        mov   r10, [wstopped]           ; WSTOPPED

; Failure to null the 5th argument results in a crash.

        xor   r8, r8

        OSCErrm waitid

        mov   [r14], rax                ; Return status

        nextm
datam

; pt_detach - ( tpid --- ) Detach from the process whose ID is passed
; on the stack. Note that process must be stopped in order to detach.

head    "pt_detach"
ptdt:   dq  ptdt0
textm
ptdt0:  mov   rdi, [pt_detach]        
        mov   rsi, [r14]
        xor   rdx, rdx
        xor   r10, r10

        OSCErrm ptrace

        add   r14, 8
        nextm
datam

; pt_seize - ( tpid --- ) Take control to the process whose ID is passed
; on the stack.

head    "pt_seize"
ptsz:   dq  ptsz0
textm
ptsz0:  mov   rdi, [pt_seize]           ; Load request
        mov   rsi, [r14]
        xor   rdx, rdx
        xor   r10, r10

        OSCErrm ptrace

        add   r14, 8
        nextm
datam

; pt_interrupt - ( tpid --- ) Stop the seized tracee whose pid is on the stack.

head    "pt_interrupt"
ptint:  dq  ptint0
textm
ptint0: mov   rdi, [pt_interrupt]       ; Load request
        mov   rsi, [r14]
        xor   rdx, rdx
        xor   r10, r10

        OSCErrm ptrace

        add   r14, 8
        nextm
datam

; pt_get_regs_raw - ( tpid buffer --- ) Get registers from attached process
; and put them in the buffer whose address is passed on the stack.
; As the name implies, this version reproduces the register data exactly
; as it is in the ptrace register structure. The buffer must have room for
; at least 216 bytes. Its character count will be set to 216.

head    "pt_get_regs_raw"
ptgrw:  dq  ptgrw0
textm
ptgrw0: mov   rdi, [pt_getregs]         ; Load request
        mov   rsi, [r14+8]              ; Get tracee pid
        xor   rdx, rdx
        lea   r10, [r14]                ; Load buffer address
        cmp   qword[r10-16], 216
        jnc   ptgrw1

; Throw error if buffer cannot accommodate data.

        add   r14, 16                   ; Pop stack

        throwm 61

ptgrw1: OSCErrm ptrace

        mov   qword[rbx-8], 216         ; Update buffer character count
        add   r14, 16                   ; Pop stack
        nextm
datam

; pt_get_reg - ( tpid n --- ) Get the contents of the register
; whose number is passed on the stack. Registers are numbered 0-17,
; with rip being 16 and rFLAGS being 17. Throw error if number is
; out of bounds.

head    "pt_get_reg"
ptgr:   dq  ptgr0
textm
ptgr0:  mov   rbx, [r14]                ; Get register number
        add   r14, 8                    ; Pop 1 stack item
        cmp   rbx, 18                   ; See if reg # too high
        jnc   ptgr19                    ; Branch to throw error

        cmp   rbx, 0                    ; Too low
        jc    ptgr19                    ; Branch to throw error

        mov   rdi, [pt_getregs]         ; Load request
        mov   rsi, [r14]                ; Get tracee pid
        xor   rdx, rdx                  ; Null address field
        lea   r10, [gtrgs0]             ; Place to put data
        mov   rbp, r10                  ; Save address

        OSCErrm ptrace

        lea   rcx, [jptgr0]             ; Get jump table address
        jmp   [rcx+rbx*8]               ; Jump to indexed entry

ptgr1:  mov   rax, [rbp+80]             ; rax
        jmp   ptgr20

ptgr2:  mov   rax, [rbp+40]             ; rbx
        jmp   ptgr20

ptgr3:  mov   rax, [rbp+88]             ; rcx
        jmp   ptgr20

ptgr4:  mov   rax, [rbp+96]             ; rdx
        jmp   ptgr20

ptgr5:  mov   rax, [rbp+104]            ; rsi
        jmp   ptgr20

ptgr6:  mov   rax, [rbp+112]            ; rdi
        jmp   ptgr20

ptgr7:  mov   rax, [rbp+32]             ; rbp
        jmp   ptgr20

ptgr8:  mov   rax, [rbp+152]            ; rsp
        jmp   ptgr20

ptgr9:  mov   rax, [rbp+72]             ; r8
        jmp   ptgr20

ptgr10: mov   rax, [rbp+64]             ; r9
        jmp   ptgr20

ptgr11: mov   rax, [rbp+56]             ; r10
        jmp   ptgr20

ptgr12: mov   rax, [rbp+48]             ; r11
        jmp   ptgr20

ptgr13: mov   rax, [rbp+24]             ; r12
        jmp   ptgr20

ptgr14: mov   rax, [rbp+16]             ; r13
        jmp   ptgr20

ptgr15: mov   rax, [rbp+8]              ; r14
        jmp   ptgr20

ptgr16: mov   rax, [rbp]                ; r15
        jmp   ptgr20

ptgr17: mov   rax, [rbp+128]            ; rip
        jmp   ptgr20

ptgr18: mov   rax, [rbp+144]            ; rFLAGS
        jmp   ptgr20

ptgr19: add   r14, 8                    ; Pop data stack

        throwm 14                       ; Throw index out of bounds

ptgr20: mov   [r14], rax                ; Return register contents
        nextm

; Jump table for register numbers:

jptgr0: dq  ptgr1,ptgr2,ptgr3,ptgr4,ptgr5,ptgr6
        dq  ptgr7,ptgr8,ptgr9,ptgr10,ptgr11,ptgr12
        dq  ptgr13,ptgr14,ptgr15,ptgr16,ptgr17,ptgr18
datam

; pt_get_regs - ( tpid addr --- ) Get only the basic registers
; from attached process and organize them in the buffer at addr
; in the following order:
;   rax, rbx, rcx, rdx, rsi, rdi, rbp, rsp, r8, r9, r10, r11, r12, r13,
;   r14, r15, rip, rFLAGS
; This is of course the same order used by <show-regs> to display them.
; The buffer must have room for at least 144 bytes. Its character count
; will be set to 144.

head    "pt_get_regs"
ptgrs:  dq  ptgrs0
textm
ptgrs0: mov   rdi, [pt_getregs]         ; Load request
        mov   rsi, [r14+8]              ; Get tracee pid
        xor   rdx, rdx                  ; Null address field
        lea   r10, [gtrgs0]             ; Place to put data
        mov   rbp, r10                  ; Save base address

        OSCErrm ptrace

        mov   rdi, [r14]

        mov   rax, [rbp+80]             ; rax
        mov   [rdi], rax

        mov   rax, [rbp+40]             ; rbx
        mov   [rdi+8], rax

        mov   rax, [rbp+88]             ; rcx
        mov   [rdi+16], rax

        mov   rax, [rbp+96]             ; rdx
        mov   [rdi+24], rax

        mov   rax, [rbp+104]            ; rsi
        mov   [rdi+32], rax

        mov   rax, [rbp+112]            ; rdi
        mov   [rdi+40], rax

        mov   rax, [rbp+32]             ; rbp
        mov   [rdi+48], rax

        mov   rax, [rbp+152]            ; rsp
        mov   [rdi+56], rax

        mov   rax, [rbp+72]             ; r8
        mov   [rdi+64], rax

        mov   rax, [rbp+64]             ; r9
        mov   [rdi+72], rax

        mov   rax, [rbp+56]             ; r10
        mov   [rdi+80], rax

        mov   rax, [rbp+48]             ; r11
        mov   [rdi+88], rax

        mov   rax, [rbp+24]             ; r12
        mov   [rdi+96], rax

        mov   rax, [rbp+16]             ; r13
        mov   [rdi+104], rax

        mov   rax, [rbp+8]              ; r14
        mov   [rdi+112], rax

        mov   rax, [rbp]                ; r15
        mov   [rdi+120], rax

        mov   rax, [rbp+128]            ; rip
        mov   [rdi+128], rax

        mov   rax, [rbp+144]            ; rFLAGS
        mov   [rdi+136], rax

        mov   qword[rdi-8], 144         ; Update buffer character count

        add   r14, 16
        nextm
datam

; pt_set_reg - ( tpid val n --- ) Set the contents of register n
; to val in tracee tpid. Registers are numbered 0-17, with rip
; being 16 and rFLAGS being 17. Throw error if number is out of bounds.

head    "pt_set_reg"
ptsr:   dq  ptsr0
textm
ptsr0:  mov   rbx, [r14]                ; Get register number
        add   r14, 8                    ; Pop 1 stack item
        cmp   rbx, 18                   ; See if reg # too high
        jnc   ptsr19                    ; Branch to throw error

        cmp   rbx, 0                    ; See if too low
        jc    ptsr19                    ; Branch to throw error

        mov   rdi, [pt_getregs]         ; Load request
        mov   rsi, [r14+8]              ; Get tracee pid
        xor   rdx, rdx                  ; Null address field
        lea   r10, [gtrgs0]             ; Place to put data
        mov   rbp, r10                  ; Save base address

        OSCErrm ptrace

        mov   rax, [r14]                ; Get new register value
        lea   rcx, [jptsr0]             ; Get jump table address
        jmp   [rcx+rbx*8]               ; Jump to indexed entry

ptsr1:  mov   [rbp+80], rax             ; rax
        jmp   ptsr20

ptsr2:  mov   [rbp+40], rax             ; rbx
        jmp   ptsr20

ptsr3:  mov   [rbp+88], rax             ; rcx
        jmp   ptsr20

ptsr4:  mov   [rbp+96], rax             ; rdx
        jmp   ptsr20

ptsr5:  mov   [rbp+104], rax            ; rsi
        jmp   ptsr20

ptsr6:  mov   [rbp+112], rax            ; rdi
        jmp   ptsr20

ptsr7:  mov   [rbp+32], rax             ; rbp
        jmp   ptsr20

ptsr8:  mov   [rbp+152], rax            ; rsp
        jmp   ptsr20

ptsr9:  mov   [rbp+72], rax             ; r8
        jmp   ptsr20

ptsr10: mov   [rbp+64], rax             ; r9
        jmp   ptsr20

ptsr11: mov   [rbp+56], rax             ; r10
        jmp   ptsr20

ptsr12: mov   [rbp+48], rax             ; r11
        jmp   ptsr20

ptsr13: mov   [rbp+24], rax             ; r12
        jmp   ptsr20

ptsr14: mov   [rbp+16], rax             ; r13
        jmp   ptsr20

ptsr15: mov   [rbp+8], rax              ; r14
        jmp   ptsr20

ptsr16: mov   [rbp], rax                ; r15
        jmp   ptsr20

ptsr17: mov   [rbp+128], rax            ; rip
        jmp   ptsr20

ptsr18: mov   [rbp+144], rax            ; rFLAGS
        jmp   ptsr20

ptsr19: add   r14, 16                   ; Pop data stack

        throwm 14                       ; Throw index out of bounds

ptsr20: mov   rdi, [pt_setregs]         ; Load request
        mov   rsi, [r14+8]              ; Get pid
        xor   rdx, rdx                  ; Null address field
        mov   r10, rbp

        OSCErrm ptrace

        add   r14, 16                   ; Pop stack
        nextm

; Jump table for register numbers:

jptsr0: dq  ptsr1,ptsr2,ptsr3,ptsr4,ptsr5,ptsr6
        dq  ptsr7,ptsr8,ptsr9,ptsr10,ptsr11,ptsr12
        dq  ptsr13,ptsr14,ptsr15,ptsr16,ptsr17,ptsr18
datam

; pt_set_regs - ( tpid addr --- ) Replace the ptrace register structure
; with the values at addr and set the tracee registers accordingly.

head    "pt_set_regs"
ptsrs:  dq  ptsrs0
textm
ptsrs0: mov   rdi, [pt_setregs]         ; Load request
        mov   rsi, [r14+8]              ; Get tracee pid
        xor   rdx, rdx                  ; Null address field
        mov   r10, [r14]                ; Address of new register structure

        OSCErrm ptrace

        add   r14, 16                   ; Pop stack
        nextm
datam

; pt_peek - ( tpid addr --- quad ) Obtain the quad at addr in the tracee
; and put it on the stack.
; Note that this call uses a different API from other ptrace requests.
; The quad is returned to the address specified in the 4th parameter, r10.
; This is in order to distinguish between data returned and an error
; condition.

head    "pt_peek"
ptpk:   dq  ptpk0
textm
ptpk0:  mov   rdi, [pt_peektext]        ; Load request
        mov   rsi, [r14+8]              ; Get tracee pid
        mov   rdx, [r14]                ; Get source addr
        add   r14, 8                    ; Pop 1 item
        mov   r10, r14                  ; Put data on stack

        OSCErrm ptrace

        nextm
datam

; pt_get_mem - ( tpid addr1 addr2 n --- ) Move n quads of memory
; from addr1 in the tracee to addr2 in the tracer.

head    "pt_get_mem"
ptgm:   dq  ptgm0
textm
ptgm0:  mov   rdi, [pt_peektext]        ; Load request
        mov   rsi, [r14+24]             ; Get tracee pid
        mov   rdx, [r14+16]             ; Get source addr
        mov   r10, [r14+8]              ; Get destination addr
        mov   rbx, [r14]                ; Get quad count

; Note how parameters stay valid through calls.

ptgm1:  OSCErrm ptrace

        add   r10, 8
        add   rdx, 8
        sub   rbx, 1
        jne   ptgm1

        add   r14, 32
        nextm
datam

; pt_dump - ( tpid addr u --- ) Dump u memory quadwords from tracee,
; starting at addr, in four columns of quads and one column of ASCII
; translation.

; : pt_dump   cr 4 /mod swap >a ?dup
;             if
;               0 do dup hex.nlz space
;                    4 0 do over over i 8* + pt_peek hex. loop
;                    2 spaces
;                    4 0 do over over i 8* + pt_peek .ascii loop
;               32 + cr loop then
;             a> ?dup
;               4 over - >a
;               over hex.nlz space
;               dup >a 0 do over over i 8* + pt_peek hex. loop
;               a> a> 0 do 17 spaces loop
;               2 spaces 0 do over over i 8* + pt_peek .ascii loop
;               cr then 2drop ;

head    "pt_dump", hlfo
ptdmp:  dq  docl0
        dq  cr,four,slmod,swp,toa,dupnz,izbrn,ptdp6
        dq  zero,ddo,ptdp6
ptdp1:  dq  dupl,hxdnz,space,four
        dq  zero,ddo,ptdp3
ptdp2:  dq  over,over,indx,estar,plus,ptpk
        dq  hexdt,_loop,ptdp2
ptdp3:  dq  two,spaces
        dq  four,zero,ddo,ptdp5
ptdp4:  dq  over,over,indx,estar,plus,ptpk
        dq  dotasc,_loop,ptdp4
ptdp5:  dq  lit,32,plus,cr,_loop,ptdp1
ptdp6:  dq  froma,dupnz,izbrn,ptdp12
        dq  four,over,minus,toa,over
        dq  hxdnz,space,dupl,toa,zero,ddo,ptdp8
ptdp7:  dq  over,over,indx,estar,plus,ptpk
        dq  hexdt,_loop,ptdp7
ptdp8:  dq  froma,froma,zero,ddo,ptdp10
ptdp9:  dq  lit,17,spaces,_loop,ptdp9
ptdp10: dq  two,spaces,zero,ddo,ptdp12
ptdp11: dq  over,over,indx,estar,plus,ptpk
        dq  dotasc,_loop,ptdp11,cr
ptdp12: dq  ddrop,semis

; pt_poke - ( tpid addr quad --- ) Place the quad on the stack
; at the address in the tracee (pid).

head    "pt_poke"
ptpo:   dq  ptpo0
textm
ptpo0:  mov   rdi, [pt_poketext]        ; Load request
        mov   rsi, [r14+16]             ; Get tracee pid
        mov   rdx, [r14+8]              ; Get destination addr
        mov   r10, [r14]                ; Get new quad

        OSCErrm ptrace

        add   r14, 24                   ; Pop stack
        nextm
datam

; pt_step - ( tpid --- ) Single-step the tracee.

head    "pt_step"
ptsp:   dq  ptsp0
textm
ptsp0:  mov   rdi, [pt_singlestep]      ; Load request
        mov   rsi, [r14]                ; Get tracee pid
        xor   rdx, rdx                  ; Null address field
        xor   r10, r10                  ; Null data field

        OSCErrm ptrace

; Note: The process ID in rsi apparently persists through the call.

        mov   rdi, [p_pid]              ; P_PID - wait for child pid
        lea   rdx, [infp0]              ; Load address of siginfo_t structure
        mov   r10, [wstopped]           ; WSTOPPED

; Failure to null the 5th argument results in a crash.

        xor   r8, r8

        OSCErrm waitid

        mov   [r14], rax                ; Return status
        nextm
datam

; pt_cont - ( tpid --- ) Resume running the tracee.

head    "pt_cont"
ptcn:   dq  ptcn0
textm
ptcn0:  mov   rdi, [pt_cont]            ; Load request
        mov   rsi, [r14]                ; Get tracee pid
        xor   rdx, rdx                  ; Null address field
        xor   r10, r10                  ; Null data field

        OSCErrm ptrace

        add   r14, 8                    ; Pop the stack
        nextm
datam

; pt_cont_signal - ( tpid sig# --- ) Resume running the tracee
; and send sig#.

head    "pt_cont_signal"
ptcsg:  dq  ptcsg0
textm
ptcsg0: mov   rdi, [pt_cont]            ; Load request
        mov   rsi, [r14+8]              ; Get tracee pid
        xor   rdx, rdx                  ; Null address field
        mov   r10, [r14]                ; Get signal

        OSCErrm ptrace

        add   r14, 16                   ; Pop the stack
        nextm
datam

; pt_set_break - ( tpid addr --- pid addr quad ) Set a breakpoint in the
; tracee at addr. The quad is fetched from the address and duplicated,
; then the INT3 (CC) instruction is placed in the 1st byte and the quad
; is written back to the tracee. The original pid and address remain on
; the stack along with the quad.

head    "pt_set_break"
ptbrk:  dq  ptbrk0
textm
ptbrk0: mov   rdi, [pt_peektext]        ; Load request
        mov   rsi, [r14+8]              ; Get tracee pid
        mov   rdx, [r14]                ; Get address
        sub   r14, 8                    ; Make room for quad
        mov   r10, r14

        OSCErrm ptrace

; Values in rsi and rdx persist through the ptrace call.

        mov   rbx, [r14]                ; Get returned quad
        mov   bl, 0xCC                  ; Install INT3
        mov   r10, rbx                  ; Copy to 4th parameter register

        mov   rdi, [pt_poketext]        ; Load request

        OSCErrm ptrace

        nextm
datam

; pt_clear_break - ( tpid addr quad --- ) Clear the breakpoint
; in the tracee by restoring the original quad at addr and
; decrementing the instruction pointer (rip).

head    "pt_clear_break"
ptcbr:  dq  ptcbr0
textm
ptcbr0: mov   rdi, [pt_poketext]        ; Load request 
        mov   rsi, [r14+16]             ; Get tracee pid
        mov   rdx, [r14+8]              ; Get destination addr
        mov   r10, [r14]                ; Get original quad

        OSCErrm ptrace

        mov   rdi, [pt_getregs]         ; Load request
        xor   rdx, rdx                  ; Null address field

        lea   r10, [gtrgs0]             ; Get address of structure
        mov   rbp, r10                  ; Save copy

        OSCErrm ptrace

        sub   qword[rbp+128], 1         ; Back up rip
        mov   rdi, [pt_setregs]         ; Load request
        xor   rdx, rdx                  ; Null address field
        mov   r10, rbp                  ; Where to get new address data

        OSCErrm ptrace

        add   r14, 24                   ; Pop stack
        nextm
datam

; pt_disasm - ( tpid addr n --- ) Disassemble n bytes of code at addr
; in the tracee and output it to the screen.
;
; : pt_disasm   base @ >a hex
;              2dup 2>a rot 2a> rnd>8 dup axb0 clear-buffer 8- !
;              8/ axb0 swap pt_get_mem >a axb0 cfib @ a> udis
;              0 do dup @ dup 3 pick + cr 6 u.lz space 4 u.lz 8+
;              dup count dup 8+ rnd>8 >a 4 spaces type a> + loop
;              2drop a> base ! ;

head    "pt_disasm", oupo
ptdsm:  dq  docl0
        dq  base,qat,toa,hex,ddup
        dq  dtoa,rot,dfroma,rute,dupl
        dq  axbn,clrbuf,eighm
        dq  stor,eslsh,axbn,swp
        dq  ptgm,toa,axbn,cfib,qat
        dq  froma,udis,zero,ddo,xpds2
xpds1:  dq  dupl,qat,dupl,three,pick
        dq  plus,cr,lit,6,udotlz,space
        dq  four,udotlz,eighp,dupl
        dq  count,dupl,eighp,rute,toa
        dq  four,spaces,typ,froma,plus
        dq  _loop,xpds1
xpds2:  dq  space,ddrop,froma,base,stor
        dq  semis

; pt_show_code - ( tpid CFA --- ) Disassemble code in the tracee
; pointed to by the code-field address of a dictionary entry.
; Usage:  tpid @ ' <entry> pt_show_code
; Note that this definition depends on the tracee being identical
; to the tracer.

; : pt_show_code   @ dup 8- @ pt_disasm ;

head    "pt_show_code", oupo
ptshcd: dq  docl0
        dq  qat,dupl,eighm,qat,ptdsm,semis
