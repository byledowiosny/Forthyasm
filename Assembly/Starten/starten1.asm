; starten1.asm - Module for multiplying by 10 by shifting and adding.

; Assemble with:  yasm -f bin starten1.asm -o starten1.fcm

; Tell the assembler this is a 64-bit module:

        BITS 64

; Tell the assembler to make any address references
; program counter relative:

        default rel

; Code goes here:

        mov   rcx, [r14]
        add   r14, 8
strt1:  mov   rax, 0xFFFFFFFFFFF
        mov   rbx, rax
        shl   rax, 2
        add   rax, rax
        shl   rbx, 1
        add   rax, rbx
        sub   rcx, 1
        jne   strt1

; The following is code, called "next", to continue execution
; in the Forthx64 inner interpreter:

        mov   r11, [r12]
        add   r12, 8
        jmp   [r11]

