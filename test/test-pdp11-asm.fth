require targets/pdp11/asm.fth

: w@ ( a -- u ) dup c@ swap 1+ c@ 8 lshift + ;
: fail? ( c a -- a' f ) 2 - tuck w@ <> ;
: .fail   cr ." FAIL: " source 5 - type cr ;
: ?fail   fail? if .fail abort then ;
: check   here begin depth 1- while ?fail repeat drop ;

.( Assembler test: )
code assembler-test
   octal

   halt,                    000000 check
   wait,                    000001 check
   rti,                     000002 check
   bpt,                     000003 check
   iot,                     000004 check
   reset,                   000005 check
   clc,                     000241 check
   clv,                     000242 check
   clz,                     000244 check
   cln,                     000250 check
   sec,                     000261 check
   sev,                     000262 check
   sez,                     000264 check
   sen,                     000270 check

   0 # emt,                 104000 check
   0 # trap,                104400 check

   r5 rts,                  000205 check

   r0 clr,                  005000 check
   r1 ) clr,                005011 check
   r2 )+ clr,               005022 check
   r2 )+@ clr,              005032 check
   r3 -) clr,               005043 check
   r3 -)@ clr,              005053 check
   4 r4 )# clr,             005064 000004 check
   4 r4 )#@ clr,            005074 000004 check
   -1 clr,                  005037 177777 check
   here pc) clr,            005067 177776 check

   r0 ) jmp,                000110 check
   r1 )+ swab,              000321 check
   r2 )+@ com,              005132 check
   r3 -) comb,              105143 check

   r4 ) r5 jsr,             004514 check

   r0 r0 mov,               010000 check
   r0 r1 mov,               010001 check
   r1 r0 mov,               010100 check
   r0 r0 movb,              110000 check
   1 # r0 mov,              012700 000001 check

   r1 r2 )+ add,            060122 check
   r3 -) r4 sub,            164304 check

   create l \ label
   l br,                    000777 check
   l bne,                   001376 check
   l ble,                   003775 check
   l bpl,                   100374 check
   l bcs,                   103773 check

   ahead, then,             000400 check
   0=, if, r0 r0 mov, then, 001001 010000 check
   begin, again,            000777 check
   begin, 0<>, until,       001777 check
end-code
.( PASS ) cr
