hex

\ Opcodes with operand
\ 00 brk,
\ 01 ora,
\ 02 asl,
\ 10 bpl,
\ 20 jsr,
\ 20 bit,
\ 21 and,
\ 22 rol,
\ 30 bmi,
\ 40 rti,
\ 40 jmp, \
\ 41 eor,
\ 42 lsr,
\ 50 bvc,
\ 60 rts,
\ 60 jmp, \ absolute
\ 61 adc,
\ 62 ror,
\ 70 bvs,
\ 80 sty,
\ 81 sta, \ no immediate mode, 89
\ 82 stx,
\ 90 bcc,
\ A0 ldy,
\ A1 lda,
\ A2 ldx,
\ B0 bcs,
\ C0 cpy,
\ C1 cmp,
\ C2 dec,
\ D0 bne,
\ E0 cpx,
\ E1 sbc,
\ E2 inc,
\ F0 beq,

\ Opcodes without operand
\ 08 php,
\ 18 clc,
\ 28 plp,
\ 38 sec,
\ 48 pha,
\ 58 cli,
\ 68 pla,
\ 78 sei,
\ 88 dey,
\ 8A txa,
\ 98 tya,
\ 9A txs,
\ A8 tay,
\ AA tax,
\ B8 clv,
\ BA tsx,
\ C8 iny,
\ CA dex,
\ D8 cld,
\ E8 inx,
\ EA nop,
\ F8 sed,

\ Addressing modes
\ 00 #     immediate, for 00 and 02 (ldx, ldy, cpy, cpx)
\ 00 ,x)   (zero page,X), for 01
\ 04       zero page
\ 08 #     immediate, for 01
\ 08 a     accumulator, for 02 (asl, rol, lsr, ror)
\ 0C       absolute
\ 10 ),y   (zero page,Y)
\ 14 ,x    zero page,X
\ 18 ,y    absolute,Y
\ 1C ,x    absolute,X
