; floop.asm - Module for simple loop using decrement and branch.
; Gets the count from the stack.

; Assemble with:  yasm -f bin floop.asm -o floop.fcm

; Tell the assembler this is a 64-bit module:

        BITS 64

; Tell the assembler to make any address references
; program counter relative:

        default rel

        mov   rcx, [r14]
        add   r14, 8
flp1:   sub   rcx, 1
        jne   flp1

; The following is code, called "next", to continue execution
; in the Forthx64 inner interpreter:

        mov   r11, [r12]
        add   r12, 8
        jmp   [r11]

