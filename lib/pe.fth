\ Create a Portable Executable (PE) image file in memory.

\ Usage:
\
\   pe-header, \ Create the header data structure
\   ...
\   pe-code \ Start code and data
\   ...
\   pe-end \ End PE image, update header

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

00000020 constant pe-code
00000040 constant pe-data
00000080 constant pe-empty
20000000 constant pe-executable
40000000 constant pe-readable
80000000 constant pe-writable

( Data types )

: zeros, ( u -- )   here swap dup allot erase ;
: h+! ( u a -- )   dup h@ rot + swap h! ;
: w+! ( u a -- )   dup w@ rot + swap w! ;

( Data structures )

variable 'mzhdr
: mzhdr+ ( u -- a ) 'mzhdr @ + ;
: current-size   here 'mzhdr @ - ;

: opthdrsize! ( a -- ) 54 mzhdr+ h! ;
: pe-entry ( u -- ) 68 mzhdr+ w! ;
: img-size! ( u -- ) 90 mzhdr+ w! ;
: hdr-size! ( u -- ) 94 mzhdr+ w! ;

( MZ header )

: mzhdr,   here 'mzhdr !  ," MZ" 3A zeros, 40 w, ;

( PE header )

: pesig, ( -- )   ," PE" 0 h, ;
: pehdr, ( -- )   pesig,  x86 h,  10 zeros,  exe h, ;

( Optional header; required for executables )

: sig,   010B h, 01A zeros, ;
: base,   w, ;
: align,   1 w, 1 w, 8 zeros, ;
: major,   4 h, 12 zeros, ;
: subsys,   3 h, 01A zeros, ;
: dd,   80 zeros, ;
: opthdr,   sig,  400000 base,  align,  major,  subsys,  dd, ;

( Section header )

: #s+   1 46 mzhdr+ h+! ;
: sname, ( a u -- ) here 8 erase here swap cmove 8 allot ;
: shdr, ( a u -- ) #s+  sname, ( size ) 0 w, FF w, ( codesize ) 0 w,
   ( code ) 0 w, 0C zeros, 60000020 w, ;

( Lay down a PE header in the dictionary )

: pe-header,   mzhdr, pehdr, here opthdr, here swap - opthdrsize! ;

: pe-code   current-size dup hdr-size! pe-entry ;

: padding   148 current-size - dup 0> and zeros, ;
: pe-end   padding  current-size img-size! ;

base !
