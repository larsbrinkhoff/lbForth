\ Copyright 2016 Lars Brinkhoff.

\ Simulator for Texas Instruments MSP430.

require search.fth
require lib/common.fth

vocabulary insn

create registers  16 2 * allot
: reg ( u -- a ) 2 * registers + ;
: pc   0 reg ;
: sp   1 reg ;
: sr   2 reg ;
: fetch ( -- u ) pc @ @  2 pc +! ;
: push ( u -- ) -2 sp +!  sp @ ! ;
: pop ( -- u ) sp @ @  2 sp +! ;

defer process ( u xt -- u )
: ?process ( u xt f1 -- f2 ) if process 0 else drop -1 then ;
: match? ( u xt -- flag ) .s >body 2@ .s cr rot and = ;
: search ( u xt -- u flag ) 2dup match? .s ?process ;
: decode ( -- ) fetch ['] insn ['] search traverse-wordlist drop ;

: instruction  drop ;
: mnemoic   create , ,  does> instruction ;
: format:   create  does> drop mnemoic ;

format: 0op
format: 1op
format: 2op
format: jump

also insn definitions
hex

  0000 0000 0op illegal

  \ 0000 F0C0 msp430x mova, \ x,Rn
  \ 0040 F3E0 msp430x rrcm,
  \ 0060 F0E0 msp430x mova, \ Rn,x
  \ 0080 F0B0 msp430x mova, \ #,Rn + Rn,Rn
  \ 0090 F0B0 msp430x cmpa,
  \ 00A0 F0B0 msp430x adda,
  \ 00B0 F0B0 msp430x suba,
  \ 0140 F3E0 msp430x rram,
  \ 0240 F3E0 msp430x rlam,
  \ 0340 F3E0 msp430x rrum,

  1000 FF80 1op rrc,
  1080 FFC0 1op swpb,
  1100 FF80 1op rra,
  1180 FFC0 1op sxt,
  1200 FF80 1op push,
  1280 FFC0 1op call,
  1300 FFFF 0op reti,

  \ 1340 FFC0 msp430x calla,
  \ 1380 FFC0 msp430x calla,
  \ 1400 FE00 msp430x pushm,
  \ 1600 FE00 msp430x popm,
  \ 1800 F800 msp430x prefix

  2000 FC00 jump jne,
  2400 FC00 jump jeq,
  2800 FC00 jump jnc,
  2C00 FC00 jump jc,
  3000 FC00 jump jn,
  3400 FC00 jump jge,
  3800 FC00 jump jl,
  3C00 FC00 jump jmp,

  4000 F000 2op mov,
  5000 F000 2op add,
  6000 F000 2op addc,
  7000 F000 2op subc,
  8000 F000 2op sub,
  9000 F000 2op cmp,
  A000 F000 2op dadd,
  B000 F000 2op bit,
  C000 F000 2op bic,
  D000 F000 2op bis,
  E000 F000 2op xor,
  F000 F000 2op and,

previous definitions

: disassemble ( u xt -- u ) .s >name type ;
' disassemble is process
