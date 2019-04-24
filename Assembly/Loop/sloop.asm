; sloop.asm - Module for simple loop using the loop instruction.
; Gets the count from the stack.

; Assemble with:  yasm -f bin sloop.asm -o sloop.fcm

; Tell the assembler this is a 64-bit module:

        BITS 64

; Tell the assembler to make any address references
; program counter relative:

        default rel

; Code goes here:

        mov   rcx, [r14]
        add   r14, 8
slp1:   loop  slp1

; The following is code, called "next", to continue execution
; in the Forthx64 inner interpreter:

        mov   r11, [r12]
        add   r12, 8
        jmp   [r11]
