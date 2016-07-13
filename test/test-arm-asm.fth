require targets/arm/asm.fth

: h@ ( a -- u ) dup c@ swap 1+ c@ 8 lshift + ;
: w@ ( a -- u ) dup h@ swap 2 + h@ 16 lshift + ;
: fail? ( c a -- a' f ) 4 - tuck w@ <> ;
: .fail   cr ." FAIL: " source 5 - type cr ;
: ?fail   fail? if .fail abort then ;
: check   here begin depth 1- while ?fail repeat drop ;

.( Assembler test: )
code assembler-test
   hex

   nop,                     E1A00000 check

   0 # svc,                 EF000000 check
   0 # eq svc,              0F000000 check
   1 # bkpt,                E1200071 check

   r0 r0 r1 and,            E0001000 check
   r0 r1 r0 and,            E0010000 check
   r1 r0 r0 and,            E0000001 check
   r0 r1 mov,               E1A01000 check
   r1 r0 mov,               E1A00001 check
   r4 r2 clz,               E16F2F14 check
   r0 r1 cmp,               E1510000 check
   r1 r0 cmp,               E1500001 check

   1 # r0 mov,              E3A00001 check
   1 # r0 r0 add,           E2800001 check

   000000FF # r0 mov,       E3A000FF check
   000003FC # r0 mov,       E3A00FFF check
   FF000000 # r0 mov,       E3A004FF check

   here r0 ldr,             E51F0008 check
   here 10 + r0 ldr,        E59F0008 check
   here r0 adr,             E24F0008 check
   here 10 + r0 adr,        E28F0008 check

   r1 ) r0 str,             E5810000 check
   r0 ) r1 ldr,             E5901000 check
   4 r1 )# r0 str,          E5810004 check
   -4 r1 )# r0 str,         E5010004 check
   4 r1 )#! r0 str,         E5A10004 check
   4 r1 #) r0 str,          E4810004 check

 \ r2 r1 )# r0 str,         E7810002 check
 \ r2 r1 )#! r0 str,        E7A10002 check
 \ r2 r1 #) r0 str,         E6810002 check

   r1 ) r0 strb,            E5C10000 check
   r1 ) r0 ldrb,            E5D10000 check
 \ r1 ) r0 strh,            E1C100B0 check
 \ r1 ) r0 ldrh,            E1D100B0 check
 \ r1 ) r0 ldrsh,           E1D100F0 check
 \ r1 ) r0 ldrsb,           E1D100D0 check

   r1 1 # lsl r0 mov,       E1A00081 check
   r1 1 # lsr r0 mov,       E1A000A1 check
   r1 1 # asr r0 mov,       E1A000C1 check
   r1 1 # ror r0 mov,       E1A000E1 check
   r6 r7 lsl r5 mov,        E1A05716 check

   lr bx,                   E12FFF1E check

   create l \ label
   l b,                     EAFFFFFE check
   l bl,                    EBFFFFFD check
   l eq b,                  0AFFFFFC check
   l ne b,                  1AFFFFFB check
   l ge b,                  AAFFFFFA check

   ahead, then,             EAFFFFFF check
   0=, if, then,            1AFFFFFF check
   begin, again,            EAFFFFFE check
   begin, 0<>, until,       0AFFFFFE check

   decimal
end-code
.( PASS ) cr
