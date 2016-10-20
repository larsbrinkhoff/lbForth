\ Copyright 2017 Lars Brinkhoff

\ Assembler for Z80.

\ Addressing modes.

\ Register indirect	(HL) (BC) (DE)
\ Memory indirect	n )
\ Indexed		n r )#

\ Registers

\ 07 A
\ 00 B
\ 01 C
\ 02 D
\ 03 E
\ 04 H
\ 05 L
\ 06 (HL)

\ Instruction set.

\ 00   nop,
\ 04   inc,	03/inx
\ 05   dec,	0B/dcx
\ 07   rlca,
\ 09   dad,
\ 0F   rrca,
\ 10   djnz,
\ 17   rla,
\ 1F   rra,
\ 27   daa,
\ 2F   cpl,
\ 37   scf,
\ 3F   ccf,
\ 76   hlt,
\ 80   add,
\ 88   adc,
\ 90   sub,
\ 98   sbc,
\ A0   and,
\ A8   xor,
\ B0   or,
\ B8   cp,
\ C1   pop,
\ C2   jp,
\ C4   call,
\ C5   push,
\ C7   rst,
\ C9   ret,
\ CB00 rlc,
\ CB00 rrc,
\ CB10 rl,
\ CB18 rr,
\ CB20 sla,
\ CB28 sra,
\ CB38 srl,
\ CBC0 set,
\ D9   exx,
\ DB40 bit,
\ DB80 res,
\ ED44 neg,
\ ED45 retn,
\ ED4D reti,
\ ED67 rrd,
\ ED6F rld,
\ EDA0 ldi,
\ EDA1 cpi,
\ EDA2 ini,
\ EDA3 outi,
\ EDA8 ldd,
\ EDA9 cpd,
\ EDAA ind,
\ EDB0 ldir,
\ EDB1 cpir,
\ EDB2 inir,
\ EDB3 otir,
\ EDB8 lddr,
\ EDB9 cpdr,
\ EDBA indr,
\ F3   ei,
\ FB   di,

\      ex,
\      im,
\      in,
\      jr,
\      ld,
\      out,
