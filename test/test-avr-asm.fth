\ Test assembler and nucleus by loading into running Forth.
\ The existing CODE words will be patched to point to the
\ new nucleus.

require targets/avr/asm.fth

: w@ ( a -- u ) dup c@ swap 1+ c@ 8 lshift + ;
: fail? ( c a -- a' f ) 2 - tuck w@ <> ;
: .fail   cr ." FAIL: " source 5 - type cr ;
: ?fail   fail? if .fail abort then ;
: check   here begin depth 1- while ?fail repeat drop ;

.( Assembler test: )
code assembler-test
   hex

   nop,                     0000 check

   r0 z xch,                9204 check
   r1 z las,                9215 check
   r2 z lac,                9226 check
   r3 z lat,                9237 check
   r4 pop,                  904F check
   r5 push,                 925F check
   r6 com,                  9460 check
   r7 neg,                  9471 check
   r8 swap,                 9482 check
   r9 inc,                  9493 check
   r10 asr,                 94A5 check
   r11 lsr,                 94B6 check
   r12 ror,                 94C7 check
   r13 dec,                 94DA check
   r14 rol,                 0CEE check
   r15 lsl,                 1CFF check
   r16 clr,                 2700 check

   r0 r0 cpc,               0400 check
   r0 r31 cpc,              05F0 check
   r31 r0 cpc,              060F check
   r0 r1 sub,               1810 check
   r1 r0 adc,               1C01 check
   r0 r0 mov,               2C00 check
   r1 r2 mul,               9C21 check

   0 # r16 cpi,             3000 check
   FF # r17 sbci,           4F1F check
   1 # r18 subi,            5021 check
   10 # r19 ori,            6130 check
   F0 # r20 ldi,            EF40 check

   z+ r0 ld,                9001 check
   r1 -z st,                9212 check
   r2 y+ st,                9229 check
   -y r3 ld,                903A check
   x r4 ld,                 904C check
   x+ r5 ld,                905D check
   -x r6 ld,                906E check
   0 z+ )# r7 ldd,          8070 check
   29 y+ )# r8 std,         A689 check

   0 r1 lds,                9010 0000 check
   FFFF r31 sts,            93F0 FFFF check

   z ijmp,                  9409 check
   z icall,                 9509 check
   0 jmp,                   940C 0000 check
   here rjmp,               CFFF check
   22F123 call,
cell 2 = [if]
                            940E F123 check
[else]
                            951E F123 check
[then]

   create l \ label
   l brcs,                  F3F8 check
   l breq,                  F3F1 check
   l brie,                  F3EF check
   l brcc,                  F7E0 check
   l brpl,                  F7DA check
   l brid,                  F7D7 check

   ahead, then,             C000 check
   0=, if, then,            F401 check
   begin, again,            CFFF check
   begin, 0<>, until,       F3F9 check
end-code
.( PASS ) cr
