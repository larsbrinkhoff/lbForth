require targets/6502/asm.fth

hex

: fail? ( c a -- a' f ) 1- tuck c@ <> ;
: .fail   cr ." FAIL: " source 5 - type cr ;
: ?fail   fail? if .fail abort then ;
: check   here begin depth 1- while ?fail repeat drop ;

\ Put machine code for a JMP instruction on the stack.
: <jump   4C  here FF and  here 8 rshift FF and ;

.( Assembler test: )
code assembler-test

   brk,                     00 check

   0 ,x) ora,               01 00 check
   0 ora,                   05 00 check
   0 # ora,                 09 00 check
   100 ora,                 0D 00 01 check
   0 ),y ora,               11 00 check
   0 ,x ora,                15 00 check
   100 ,y ora,              19 00 01 check
   100 ,x ora,              1D 00 01 check

   0 asl,                   06 00 check
   a asl,                   0A check
   100 asl,                 0E 00 01 check
   0 ,x asl,                16 00 check
   100 ,x asl,              1E 00 01 check

   php,                     08 check
   clc,                     18 check
   0 jsr,                   20 00 00 check

   0 bit,                   24 00 check
   100 bit,                 2C 00 01 check

   plp,                     28 check
   sec,                     38 check
   rti,                     40 check
   pha,                     48 check

   0 jmp,                   4C 00 00 check
   100 jmp,                 4C 00 01 check
   <jump here jmp,          check
   here 3 + jmp,            <jump check

   cli,                     58 check
   rts,                     60 check
   pla,                     68 check
   100 ) jmp,               6C 00 01 check
   sei,                     78 check

   0 sty,                   84 00 check
   100 sty,                 8C 00 01 check
   0 ,x sty,                94 00 check

   dey,                     88 check
   txa,                     8A check
   0 ,y stx,                96 00 check
   tya,                     98 check
   txs,                     9A check

   0 # ldy,                 A0 00 check
   0 ldy,                   A4 00 check
   100 ldy,                 AC 00 01 check
   0 ,x ldy,                B4 00 check
   100 ,x ldy,              BC 00 01 check

   0 # ldx,                 A2 00 check
   tay,                     A8 check
   tax,                     AA check
   0 ,y ldx,                B6 00 check
   clv,                     B8 check
   tsx,                     BA check
   100 ,y ldx,              BE 00 01 check
   iny,                     C8 check
   dex,                     CA check
   cld,                     D8 check
   inx,                     E8 check
   nop,                     EA check
   sed,                     F8 check

   create l \ label
   l bpl,                   10 FE check
   l bmi,                   30 FC check
   l bvc,                   50 FA check
   l bvs,                   70 F8 check
   l bcc,                   90 F6 check
   l bcs,                   B0 F4 check

   ahead, then,             <jump check
   0=, if, then,            D0 00 check
   <jump begin, again,      check
   begin, 0<>, until,       F0 FE check
end-code
.( PASS ) cr
