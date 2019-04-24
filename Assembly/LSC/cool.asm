; cool.asm - Output a message with Linux System Call 1
;
; Assemble with:  yasm -f bin cool.asm -o cool.fcm

; Tell the assembler this is a 64-bit module:

        BITS 64

; Tell the assembler to make any address references
; program counter relative:

        default rel

; Code goes here:

; Write string.

        mov   rax, 1                ; Syscall 1 is write
        mov   rdi, 1                ; Standard handle
        lea   rsi, [str0]           ; Get address of string length 
        mov   rdx, [rsi]            ; Get length
        add   rsi, 8                ; Advance to string

        syscall

; The following is code, called "next", to continue execution
; in the Forthx64 inner interpreter:

        mov   r11, [r12]
        add   r12, 8
        jmp   [r11]

; Data or containers go here:

str0:   dq    end0-$-8
        db    10,"This is cool!",10
end0

