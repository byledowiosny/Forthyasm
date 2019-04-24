; rand.asm -  - Return a 64-bit random number using Linux System Call 318

; Assemble with:  yasm -f bin rand.asm -o rand.fcm

; Tell the assembler this is a 64-bit module:

        BITS 64

; Tell the assembler to make any address references
; program counter relative:

        default rel

; Code goes here:

        sub   r14, 8
        mov   rdi, r14              ; Use stack for buffer
        mov   rsi, 8                ; Get 8 bytes
        xor   rdx, rdx              ; Set no flags
        mov   rax, 318              ; Call # for getrandom

        syscall

; The following is code, called "next", to continue execution
; in the Forthx64 inner interpreter:

        mov   r11, [r12]
        add   r12, 8
        jmp   [r11]

