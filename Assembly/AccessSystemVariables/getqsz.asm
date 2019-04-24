; getqsz.asm - Example of referencing a system variable using an offset from sysvar0.

; Assemble with:  yasm -f bin getqsz.asm -o getqsz.fcm

; Tell the assembler this is a 64-bit module:

        BITS 64

; Tell the assembler to make any address references
; program counter relative:

        default rel
  
        mov   rdi, [r11]                ; Get code address
        mov   rsi, [rdi-16]             ; Get address of sysvar0
        mov   rdx, [rsi+240]            ; Offset to qsz
        sub   r14, 8                    ; Push to stack
        mov   [r14], rdx

; The following is code, called "next", to continue execution
; in the Forthx64 inner interpreter:

        mov   r11, [r12]
        add   r12, 8
        jmp   [r11]
