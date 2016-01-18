\ Test assembler and nucleus by loading into running Forth.
\ The existing CODE words will be patched to point to the
\ new nucleus.

require targets/m68k/asm.fth

: w@ ( a -- u ) dup c@ 8 lshift swap 1+ c@ + ;
: fail? ( c a -- a' f ) 2 - tuck w@ <> ;
: .fail   cr ." FAIL: " source 5 - type cr ;
: ?fail   fail? if .fail abort then ;
: check   here begin depth 1- while ?fail repeat drop ;

.( Assembler test: )
code assembler-test
   hex

   illegal,                 4A7C  check
   reset,                   4E70  check
   nop,                     4E71  check
   rte,                     4E73  check
   rts,                     4E75  check
   trapv,                   4E76  check
   rtr,                     4E77  check

   7 # bkpt,                484F  check
   0F # trap,               4E4F  check

   d0 clr, .b               4200  check
   d1 clr, .w               4241  check
   d7 clr, .l               4287  check

   a0 ) clr, .b             4210  check
   a0 )+ clr, .b            4218  check
   a0 -) clr, .b            4220  check
   1001 a0 )# clr, .b       4228 1001  check
   12345678 clr, .b         4239 1234 5678  check

   d0 negx, .b              4000  check
   d7 neg, .l               4487  check
   d0 not, .w               4640  check
 \ pea,
   d5 swap,                 4845  check
   d0 tst, .l               4A80  check
   d7 tas,                  4AC7  check
 \ a7 unlk,                 4E5F  check

   d0 d1 add,               D200  check
   a0 ) d1 add,             D210  check
   d1 a0 ) add,             D310  check
   a1 ) a0 lea,             41D1  check

   d1 d2 or,                8401  check
   a1 ) d3 sub,             9611  check
   d4 a2 ) cmp,             B912  check
   d5 a3 )+ eor,            BB1B  check
   a4 -) d6 eor,            BC24  check
   d7 1234 a7 )# and,       CF2F 1234  check

   d7 d7 mulu,              CEC7  check
   d7 d7 muls,              CFC7  check

   create l \ label
 \ l jo,                    70 FE  check

 \ ahead, then,             E9 00 00 00 00  check
 \ 0=, if, then,            75 00  check
 \ begin, again,            E9 FB FF FF FF  check
 \ begin, 0<>, until,       74 FE  check

   decimal
end-code
.( PASS ) cr

[then]
