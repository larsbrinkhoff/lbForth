require lib/mem.fth

base @ hex

( File types )

0103 constant exe
\ 0001 No relocations.
\ 0002 Executable.
\ 0004 No line nubmers.
\ 0008 No local symbols.
\ 0010 Aggressively trim working set.
\ 0020 Handles >2GB.
\ 0100 32-bit.
\ 0200 Separate .dbg.
\ 1000 System file; driver.
\ 2000 Dll.
\ 4000 Uniprocessor.

( Machine types )

014C constant x86
01C0 constant arm
01C2 constant thumb
0268 constant m68k
8664 constant x64

( Section types )

00000020 constant code
00000040 constant data
00000080 constant empty
20000000 constant executable
40000000 constant readable
80000000 constant writable

( Data types )

: s, ( a u -- )   here swap dup allot cmove ;
: zeros, ( u -- )   here swap dup allot erase ;
: h+! ( u a -- )   dup h@ rot + swap h! ;
: w+! ( u a -- )   dup w@ rot + swap w! ;

( MZ header )

: mzhdr, ( -- )   s" MZ" s, 3A zeros, 40 w, ;

( PE header )

: pesig, ( -- )   s" PE" s, 0 h, ;
: pehdr, ( -- )   pesig,  x86 h,  10 zeros,  exe h, ;

( Optional header; required for executables )

: opthdr, ( -- )   010B h, 01A zeros, 400000 w, 1 w, 1 w, 8 zeros,
  4 h, 12 zeros, 3 h, 01A zeros, ;

( Data directories. )

: dd, ( -- )   80 zeros, ;

( Section header )

: #s+ ( a -- a )   1 over 46 + h+! ;
: sname, ( a u -- )   here 8 erase here swap cmove 8 allot ;
: shdr, ( a1 a2 u -- a1 )   #s+  sname, ( size ) 0 w, FF w, ( codesize ) 0 w,
   ( code ) 0 w, 0C zeros, 60000020 w, ;

( Lay down a PE header in the dictionary. )

: opthdrsize! ( a -- a )   here over - 58 - over 54 + h! ;
: hdrsize! ( a -- a u )   here over - 2dup swap 2dup 068 + w!  2dup 090 + w!
   2dup 094 + w!  ( 2dup 0C4 + 80 + w!  0CC + 80 + w! ) 2drop ;

: pe, ( -- a u )   here  mzhdr, pehdr, opthdr, ( dd, ) opthdrsize!
   ( s" .text" shdr, ) hdrsize! ;
: padding ( a u -- a u )   over here swap - 148 swap - dup 0> and zeros, ;
: codesize! ( a u -- )   swap  2dup 090 + w+!  ( 2dup 0C0 + 80 + w! )
   ( 0C8 + 80 + w! ) 2drop ;
: pe! ( a u -- )   padding  over + here swap - codesize! ;

( Test )

0 [if]
0 constant eax
: movi, ( x reg -- )   B8 + c, w, ;
: ret, ( -- )   C3 c, ;
: exit, ( -- )   42 eax movi,  ret, ;

: output ( a u -- )   bounds do i c@ emit loop ;
: test   here  pe, exit, pe!  here over - ( 2dup dump ) output ;
test
[then]

base !
