; assemble.asm - Shell for Creating Assembly Language Modules
;
; Assemble with:  yasm -f bin <file>.asm -o <file>.fcm

; Tell the assembler this is a 64-bit module:

        BITS 64

; Tell the assembler to make any address references
; program counter relative:

        default rel


; Code goes here:


; The following is code, called "next", to continue execution
; in the Forthx64 inner interpreter:

        mov   r11, [r12]
        add   r12, 8
        jmp   [r11]

; This must be the last code executed.

; Data or containers are located here:

