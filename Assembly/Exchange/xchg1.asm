; xchg1.asm -This module gets a loop count from the stack and does
; a register-to-memory exchange using the xchg instruction.

; Assemble with:  yasm -f bin xchg1.asm -o xchg1.fcm

; Tell the assembler this is a 64-bit module:

        BITS 64

; Tell the assembler to make any address references
; program counter relative:

        default rel

; Code goes here:

        mov   rcx, [r14]
        mov   rax, 2
sxch1:  xchg  [r14], rax
        sub   rcx, 1
        jne   sxch1
        add   r14, 8

; The following is code, called "next", to continue execution
; in the Forthx64 inner interpreter:

        mov   r11, [r12]
        add   r12, 8
        jmp   [r11]
