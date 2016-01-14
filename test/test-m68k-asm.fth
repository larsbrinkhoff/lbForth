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

   nop,                     4E71  check
   rts,                     4E73  check

   d0 clr, .b               4200  check
   d1 clr, .w               4241  check
   d7 clr, .l               4287  check

   a0 ) clr, .b             4210  check
   a0 )+ clr, .b            4218  check
   a0 -) clr, .b            4220  check
   1001 a0 )# clr, .b       4228 1001  check
   12345678 clr, .b         4239 1234 5678  check

 \ d1 d2 add,               D283  check

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
