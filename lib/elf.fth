base @ hex

( Object types )

1 constant relocatable
2 constant executable
3 constant shared
4 constant core

( Machine types )

02 constant sparc
03 constant x86
04 constant m68k
14 constant ppc
15 constant ppc64
3E constant x64
28 constant arm
2B constant sparcv9
40 constant pdp10
41 constant pdp11
B7 constant arm64

[undefined] >host [if] : >host ; [then]

( Data types )

: h, ( x -- )   dup c,  8 rshift c, ;
: w, ( x -- )   , ; \ 32 bits
: a, ( x -- )   , ; \ 32 or 64 bits
: zeros, ( u -- )   here swap dup allot erase ;

( Data structures )

variable start
variable fsize
variable extra  0 extra !

: start- ( a -- u ) >host start @ >host - ;
: elf-extra-bytes   extra ! ;

: !file-size   fsize @ ! ;
: !mem-size   fsize @ 4 + ! ;

( ELF header )

54 constant entry-offset

: ident, ( -- )   7F c, ," ELF" 00010101 w, 8 zeros, ;
: type, ( u -- )   executable h, h, 1 w, ;
: entry, ( a -- a )   dup entry-offset + a, ;
: tables, ( -- )   34 h, 20 h, 1 h, 6 zeros, ;
: ehdr, ( a u -- a )   ident, type, entry, 34 a, 0 a, 0 w, tables, ;

( Program header )

: phdr32, ( a -- )   1 w, 0 a, dup a, a, here fsize ! 0 a, 0 a, 7 w, 1000 w, ;
: phdr64, ( a -- )   1 w, 5 w, 0 a, dup a, a, here fsize ! 0 a, 0 a, 1000 w, ;

( Section header )

\ shdr,   name type flags addr offset size link info align size

( Lay down an ELF header in the dictionary. )

: elf32-header, ( a u -- ) here start !  ehdr, phdr32, ;
: elf-end   align here start- dup !file-size  extra @ + !mem-size ;

base !
