\ Test MSP430 assembler.

require targets/msp430/asm.fth

: w@ ( a -- u ) dup c@ swap 1+ c@ 8 lshift + ;
: fail? ( c a -- a' f ) 2 - tuck w@ <> ;
: .fail   cr ." FAIL: " source 5 - type cr ;
: ?fail   fail? if .fail abort then ;
: check   here begin depth 1- while ?fail repeat drop ;

.( Assembler test: )
code assembler-test
   hex

   reti,                      1300  check

   r4 push,                   1204  check
   1234 r4 )# push,           1214 1234  check
   r4 ) push,                 1224  check
   r4 )+ push,                1234  check
   3 # push,                  1230  0003  check
 \ here push,                 1210  FFFE  check
   1234 & push,               1212  1234  check

   -1# push,                  1233  check
   0# push,                   1203  check
   1# push,                   1213  check
   2# push,                   1223  check
   4# push,                   1222  check
   8# push,                   1232  check

   r4 r5 mov,                 4405  check
   r4 )+ r5 mov,              4435  check
   r4 ) 1234 r5 )# mov,       44A5 1234 check
   1234 r4 )# r5 mov,         4415 1234 check
   1234 & 5678 r5 )# mov,     4295 1234 5678  check

   decimal
end-code
.( PASS ) cr
