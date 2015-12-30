\ Create a Portable Executable (PE) image file in memory.

\ Usage:
\
\   pe-header, \ Create the header data structure
\   ...
\   pe-extern, name \ Declare an external symbol
\   ...
\   pe-import, dll \ Import a dll
\   ...
\   name dll pe-symbol, \ Import an external symbol from a dll
\   ...
\   pe-code \ Start code and data
\   ...
\   pe-end \ End PE image, update header

require lib/mem.fth

also forth
base @ hex
hex

( Host data structures )

variable 'mzhdr
: mzhdr! ( a -- ) 'mzhdr ! ;
: mzhdr+ ( u -- a ) 'mzhdr @ + ;

variable extra  0 extra !
: pe-extra-bytes ( u -- ) extra ! ;

: file-offset ( a -- u ) >host t-image - ;
: rva ( a -- u ) 'mzhdr @ - ;

: get-name   >in @ parse-name rot >in ! ;
: (constant) ( x -- ) >in @ swap constant >in ! ;

previous

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

: file-aligned   1ff + -200 and ;
: file-align   here file-aligned here - allot ;
: section-aligned   fff + -1000 and ;

( Data types )

: zeros, ( u -- )   here swap dup allot erase ;
: h+! ( u a -- )   dup h@ rot + swap h! ;
: w+! ( u a -- )   dup w@ rot + swap w! ;

( PE data structures )

: current-size   here rva ;
: opthdrsize! ( a -- ) 54 mzhdr+ h! ;
: pe-entry ( a -- ) rva 68 mzhdr+ w! ;
: img-size! ( u -- ) rva extra @ + 90 mzhdr+ w! ;
: hdr-size! ( u -- ) rva 94 mzhdr+ w! ;
: #rva-and-sizes! ( u -- ) B4 mzhdr+ w! ;
: imports! ( a -- ) rva C0 mzhdr+ w! ;
: virt-size! ( a -- ) 140 mzhdr+ w! ;
: virt-start@ ( a -- ) 144 mzhdr+ w@ ;
: raw-size! ( a -- ) 148 mzhdr+ w! ;
: 'raw-start ( a -- ) 14C mzhdr+ ;
: code-start! ( a -- ) rva 'raw-start w! ;
: raw-start@ ( -- u ) 'raw-start w@ ;

: raw-end! ( a -- ) file-offset raw-start@ - raw-size! ;
: virt-end! ( a -- ) rva virt-start@ - virt-size! ;
: code-end! ( a -- ) dup raw-end!  extra @ + virt-end! ;

( MZ header )

: mzhdr,   here mzhdr!  ," MZ" 3A zeros, 40 w, ;

( PE header )

: pesig, ( -- )   ," PE" 0 h, ;
: pehdr, ( -- )   pesig,  x86 h,  10 zeros,  exe h, ;

( Optional header; required for executables )

: sig,   010B h, 01A zeros, ;
: base,   w, ;
: align,   1000 w, 200 w, 8 zeros, ;
: major,   4 h, 12 zeros, ;
: subsys,   3 h, 01A zeros, ;
: dd,   80 zeros, ;
: opthdr,   sig,  400000 base,  align,  major,  subsys,  dd, ;

( Section header )

: #s+   1 46 mzhdr+ h+! ;
: sname, ( a u -- ) here 8 erase here swap cmove 8 allot ;
: shdr, ( u1 a u2 -- ) #s+  sname, 0 w, 1000 w, 14 zeros,  w, ;
: .code,   E0000020 s" .code" shdr, ;

( Lay down a PE header in the dictionary )

: pe-header,   mzhdr, pehdr, here opthdr, here swap - opthdrsize! .code, ;

( Imports )

: z", ( a u -- ) move, 0 c, align ;
: func, ( "name" -- a ) here 0 h, get-name z", ;
: pe-extern ( "name" -- ) func,  here constant  rva w, 0 w, ;

: end,   5 cells zeros, ;
: pe-import ( "name" -- )
   10 #rva-and-sizes!
   current-size (constant)
   parse-name z",
   here imports! end, ;

: symbol, ( a u -- ) 0 w, 0 w, 0 w,  w,  rva w, ;
: pe-symbol ( a u -- ) -5 cells allot symbol, end, ;

( Start and end code )

: pe-code   file-align  here hdr-size!  here code-start!  here pe-entry ;
: pe-end   here img-size!  here code-end! ;

also forth base ! previous
