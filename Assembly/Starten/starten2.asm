; starten2.asm - Module for multiplying by 10 using mul instruction.

; Assemble with:  yasm -f bin starten2.asm -o starten2.fcm

; Tell the assembler this is a 64-bit module:

        BITS 64

; Tell the assembler to make any address references
; program counter relative:

        default rel

; Code goes here:

        mov   rcx, [r14]
        add   r14, 8
        mov   rbx, 10

strt2:  mov   rax, 0xFFFFFFFFFFF
        mul   rbx
        sub   rcx, 1
        jne   strt2

; The following is code, called "next", to continue execution
; in the Forthx64 inner interpreter:

        mov   r11, [r12]
        add   r12, 8
        jmp   [r11]

