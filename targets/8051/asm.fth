\ Copyright 2016 Lars Brinkhoff

\ Assembler for 8051.

hex

\ Operand
\ 4 A
\ 4 #
\ 5 data addr
\ 6 @r0
\ 7 @r1
\ 8 r0
\ 9 r1
\ A r2
\ B r3
\ C r4
\ D r5
\ E r6
\ F r7

\ 00 nop,
\ 00 inc,
\ 01 ajmp,
\ 02 ljmp,
\ 03 rr,
\ 10 jbc,
\ 10 dec,
\ 11 acall,
\ 12 lcall,
\ 13 rrc,
\ 20 jb,
\ 20 add,
\ 22 ret,
\ 23 rl,
\ 30 jnb,
\ 30 addc,
\ 32 reti,
\ 33 rlc,
\ 40 jc,
\ 40 orl,
\ 50 jnc,
\ 50 anl,
\ 60 jz,
\ 60 xrl,
\ 70 jnz,
\ 70 mov,
\ 72 orl,
\ 73 jmp,
\ 80 sjmp,
\ 80 mov,
\ 82 anl,
\ 83 movc,
\ 84 div,
\ 90 mov,
\ 90 subb,
\ 92 mov,
\ 93 movc,
\ A0 orl,
\ A0 mov,
\ A2 mov,
\ A3 inc,
\ A4 mul,
\ A5 (reserved)
\ B0 anl, 
\ B0 cjne,
\ B2 cpl,
\ B3 cpl,
\ C0 push,
\ C0 xch,
\ C2 clr,
\ C3 clr,
\ C4 swap,
\ D0 pop,
\ D0 djnz,
\ D0 xchd,
\ D2 setb,
\ D3 setb,
\ D4 da,
\ E0 movx,
\ E0 mov,
\ E2 movx,
\ E3 movx,
\ E4 clr,
\ F0 movx,
\ F0 mov,
\ F2 movx,
\ F3 movx,
\ F4 cpl,
