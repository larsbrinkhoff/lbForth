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

   d0 .b clr,               4200  check
   d1 .w clr,               4241  check
   d7 .l clr,               4287  check

   a0 ) .b clr,             4210  check
   a0 )+ .b clr,            4218  check
   a0 -) .b clr,            4220  check
   1001 a0 )# .b clr,       4228 1001  check
   12345678 .b clr,         4239 1234 5678  check
   here pc) pea,            487A FFFE  check

   d0 .b negx,              4000  check
   d7 .l neg,               4487  check
   d0 .w not,               4640  check
 \ pea,
   d5 swap,                 4845  check
   d0 .l tst,               4A80  check
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

   0 # d0 .b ori,           0000 0000  check
   -1 # a7 ) .w andi,       0257 FFFF  check
   -1 # 1234 a0 )# .l subi, 04A8 FFFF FFFF 1234  check
   0 # 12345678 .w addi,    0679 0000 1234 5678  check
   8000 # d0 .w eori,       0A40 8000  check
   7FFF # d0 .w cmpi,       0C40 7FFF  check

   d1 d0 .b move,           1001  check
   d1 d0 .l move,           2001  check
   d1 d0 .w move,           3001  check
   a1 d0 .l move,           2009  check
   a1 ) d0 .b move,         1011  check
   12345678 d0 .b move,     1039 1234 5678  check
   1 # d0 .b move,          103C 0001  check
   d0 d1 .b move,           1200  check
   d0 a1 ) .b move,         1280  check
   d0 12345678 .b move,     13C0 1234 5678  check
   1 # 12345678 .b move,    13FC 0001 1234 5678  check
 \ 12345678 87654321 .b move, 13F9 1234 5678 8765 4321  check

   create l \ label
   l bra,                   6000 FFFE  check
   l bsr,                   6100 FFFA  check
   l bhi,                   6200 FFF6  check
   l ble,                   6F00 FFF2  check

   ahead, then,             6000 0002  check
   0=, if, then,            6600 0002  check
   begin, again,            6000 FFFE  check
   begin, 0<>, until,       6700 FFFE  check

   decimal
end-code
.( PASS ) cr

[then]
