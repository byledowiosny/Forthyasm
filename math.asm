; Double-precision Math Module for Forthx64
;
; (c) Copyright 2018 by John F. Healy. All rights reserved.
;
; math.asm is distributed under the terms of the 2-clause BSD License.
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
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

; C Double-Precision Math Library Functions.

; Function names end with "d" to indicate that they accept
; and return double-precision floating-point operands.
; These operands occupy one quad each on the stack.
; Note that error processing is not yet included with these
; functions.

; This module is included by default in the Forthx64 source file in the section
; titled "Include external modules here". To exclude it, simply comment out
; the line:
;
;   %include "math.asm"
;
; If math.asm is included, add -lm to the compile command line to link
; the library.
;
; C Standard Library Math Functions

    extern  trunc, round, fabs, ceil, floor
    extern  cbrt, pow, hypot, fmod, modf
    extern  sin, cos, sincos, tan, asin, acos, atan, atan2
    extern  sinh, cosh, tanh, asinh, acosh, atanh
    extern  exp, exp2, expm1, log, log2, log10, log1p

; Macro for SSE Functions - single operand

%macro  SSEm  1
        %1    xmm0, [r14]
        movsd [r14], xmm0
%endmacro

; Macro for SSE Functions - two operands

%macro  SSE2m 1
        movsd xmm0, [r14+8]
        %1    xmm0, [r14]
        add   r14, 8
        movsd [r14], xmm0
%endmacro

; Macro for Math Function Calls - single operand

%macro  MFCm  1
        movsd xmm0, [r14]
        sub   rsp, 8

        call  %1

        add   rsp, 8
        movsd [r14], xmm0
%endmacro

; Macro for Math Function Calls - two operands

%macro  MFC2m 1
        movsd xmm0, [r14+8]
        movsd xmm1, [r14]
        add   r14, 8
        sub   rsp, 8

        call  %1

        add   rsp, 8
        movsd [r14], xmm0
%endmacro

; Precompiled Constants

; zerod - Put double-precision zero on stack.

head    "zerod", dpco
zrod:   dq  cons0
        dq  0

; oned - Put double-precision one on stack.

head    "oned", dpco
oned:   dq  cons0
        dq  4607182418800017408

; pid - Put double-precision pi on stack.

head    "pid", dpco
pidp:   dq  cons0
        dq  4614256656552045848

; ed - Put double-precision e on stack.

head    "ed", dpco
etod:   dq  cons0
        dq  4613303445314885481

; Arithmetic Operators

; x*2d ( x --- 2*x )

head    "x*2d", dpmo
xttd:   dq  xttd0
textm
xttd0:  movsd xmm0, [r14]
        addsd xmm0, [r14]
        movsd [r14], xmm0
        nextm
datam

; truncd ( x --- Chop to integer )

head    "truncd", dpmo
trncd:  dq  trncd0
textm
trncd0: MFCm  trunc
        nextm
datam

; fracd ( x --- Fractional part of x )

head    "fracd", dpmo
fracd:  dq  fracd0
textm
fracd0: mov   rdi, r14
        MFCm  modf
        nextm
datam

; roundd ( x --- nearest integer to x )

head    "roundd", dpmo
rondd:  dq  rondd0
textm
rondd0: MFCm  round
        nextm
datam

; floord ( x --- floor x )

head    "floord", dpmo
floord: dq  flrdd0
textm
flrdd0: MFCm  floor
        nextm
datam

; ceild ( x --- ceil x )

head    "ceild", dpmo
ceild:  dq  ceild0
textm
ceild0: MFCm  ceil
        nextm
datam

; absd ( x --- abs x )

head    "absd", dpmo
absd:   dq  absd0
textm
absd0:  MFCm  fabs
        nextm
datam

; negd ( x --- -x )

head    "negd", dpmo
negd:   dq  negd0
textm
negd0:  btc   qword[r14], 63
        nextm
datam

; mind ( x y  --- min )

head    "mind", dpmo
mind:   dq  mind0
textm
mind0:  SSE2m minsd
        nextm
datam

; maxd ( x y  --- max )

head    "maxd", dpmo
maxd:   dq  maxd0
textm
maxd0:  SSE2m maxsd
        nextm
datam

; +d ( x y  --- x+y )

head    "+d", dpmo
plusd:  dq  plusd0
textm
plusd0: SSE2m addsd
        nextm
datam

; -d ( x y  --- x-y )

head    "-d", dpmo
mnusd:  dq  mnusd0
textm
mnusd0: SSE2m subsd
        nextm
datam

; *d ( x y  --- x*y )

head    "*d", dpmo
stard:  dq  stard0
textm
stard0: SSE2m mulsd
        nextm
datam

; /d ( x y  --- x/y )

head    "/d", dpmo
slshd:  dq  slshd0
textm
slshd0: SSE2m divsd
        nextm
datam

; modd ( x y --- x mod y )

head    "modd", dpmo
modd:   dq  modd0
textm
modd0:  MFC2m fmod
        nextm
datam

; 1/d ( x --- 1/x )

head    "1/d", dpmo
rcipd:  dq  rcipd0
textm
rcipd0: mov       rax, 1
        cvtsi2sd  xmm0, rax
        divsd     xmm0, [r14]
        movsd [r14], xmm0
        nextm
datam

; Transcendental Functions

; sind ( x --- sin x )

head    "sind", dpmo
sind:   dq  sind0
textm
sind0:  MFCm  sin
        nextm
datam

; cosd ( x --- cos x )

head    "cosd", dpmo
cosd:   dq  cosd0
textm
cosd0:  MFCm  cos
        nextm
datam

; sincosd ( x --- cos[x] sin[x] )

head    "sincosd", dpmo
sncsd:  dq  sncsd0
textm
sncsd0: movsd xmm0, [r14]
        mov   rsi, r14
        sub   r14, 8
        mov   rdi, r14
        sub   rsp, 8

        call  sincos

        add   rsp, 8
        nextm
datam

; hypotd ( a b --- sqrt[a^2 + b^2] )

head    "hypotd", dpmo
hyptd:  dq  hyptd0
textm
hyptd0: MFC2m hypot
        nextm
datam

; tand ( x --- tan x )

head    "tand", dpmo
tand:   dq  tand0
textm
tand0:  MFCm  tan
        nextm
datam

; asind ( x --- asin x )

head    "asind", dpmo
asind:  dq  asind0
textm
asind0: MFCm  asin
        nextm
datam

; acosd ( x --- acos x )

head    "acosd", dpmo
acosd:  dq  acosd0
textm
acosd0: MFCm  acos
        nextm
datam

; atand ( x --- atan x )

head    "atand", dpmo
atand:  dq  atand0
textm
atand0: MFCm  atan
        nextm
datam

; atan2d ( x --- atan2 x )

head    "atan2d", dpmo
atant:  dq  atant0
textm
atant0: MFCm  atan2
        nextm
datam

; x**2d ( x --- x*x )

head    "x**2d", dpmo
xsqd:   dq  xsqd0
textm
xsqd0:  movsd xmm0, [r14]
        mulsd xmm0, [r14]
        movsd [r14], xmm0
        nextm
datam

; exp2d ( x --- exp2 x )

head    "exp2d", dpmo
exptd:  dq  exptd0
textm
exptd0: MFCm  exp2
        nextm
datam

; expd ( x --- exp x )

head    "expd", dpmo
expd:   dq  expd0
textm
expd0:  MFCm  exp
        nextm
datam

; expm1d ( x --- exp x - 1 )

head    "expm1d", dpmo
expmod: dq  xpmod0
textm
xpmod0: movsd     xmm0, [r14]
        sub       rsp, 8

        call      exp

        add       rsp, 8
        mov       rax, 1
        cvtsi2sd  xmm1, rax
        subsd     xmm0, xmm1
        movsd     [r14], xmm0
        nextm
datam

; y**xd ( y x --- y^x )

head    "y**xd", dpmo
yttxd:  dq  yttxd0
textm
yttxd0: MFC2m pow
        nextm
datam

; sqrtd ( x --- x^1/2 )

head    "sqrtd", dpmo
sqrtd:  dq  sqrtd0
textm
sqrtd0: SSEm sqrtsd
        nextm
datam

; cbrtd ( x --- x^1/3 )

head    "cbrtd", dpmo
cbtd:   dq  cbtd0
textm
cbtd0:  MFCm  cbrt
        nextm
datam

; log2d ( x --- log2 x )

head    "log2d", dpmo
logtd:  dq  logtd0
textm
logtd0: MFCm  log2
        nextm
datam

; logd ( x --- log10 x )

head    "logd", dpmo
logd:   dq  logd0
textm
logd0:  MFCm  log10
        nextm
datam

; lnd ( x --- loge x )

head    "lnd", dpmo
lnd:    dq  lnd0
textm
lnd0:   MFCm  log
        nextm
datam

; ln1pd ( x --- loge[1+x] )

head    "ln1pd", dpmo
lnopd:  dq  lnopd0
textm
lnopd0: movsd     xmm0, [r14]
        mov       rax, 1
        cvtsi2sd  xmm1, rax
        addsd     xmm0, xmm1
        sub       rsp, 8

        call      log

        add       rsp, 8
        movsd     [r14], xmm0
        nextm
datam

; log1pd ( x --- loge[1+x] )
; Although this version uses the standard C-library function,
; it produces a slightly different result from the previous,
; explicit version. The previous version yields a result that
; is the same as that from the x87 FPU rounded from extended
; precision to double-precision, and so seems to be more
; accurate.

head    "log1pd", dpmo
lgopd:  dq  lgopd0
textm
lgopd0: MFCm  log1p
        nextm
datam

; sinhd ( x --- sinh x )

head    "sinhd", dpmo
sinhd:  dq  sinhd0
textm
sinhd0: MFCm  sinh
        nextm
datam

; coshd ( x --- cosh x )

head    "coshd", dpmo
coshd:  dq  coshd0
textm
coshd0: MFCm  cosh
        nextm
datam

; tanhd ( x --- tanh x )

head    "tanhd", dpmo
tanhd:  dq  tanhd0
textm
tanhd0: MFCm  tanh
        nextm
datam

; asinhd ( x --- asinh x )

head    "asinhd", dpmo
asnhd:  dq  asnhd0
textm
asnhd0: MFCm  asinh
        nextm
datam

; acoshd ( x --- acosh x )

head    "acoshd", dpmo
acshd:  dq  acshd0
textm
acshd0: MFCm  acosh
        nextm
datam

; atanhd ( x --- atanh x )

head    "atanhd", dpmo
atnhd:  dq  atnhd0
textm
atnhd0: MFCm  atanh
        nextm
datam

