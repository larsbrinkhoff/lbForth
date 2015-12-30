\ Create a Executale and Linkable Format (ELF) image file in memory.

\ Documentation:
\ elf-header, ( a u -- ) \ Create header loding at a with machine type u.
\ elf-start ( a -- ) \ Specify the start header in memory.
\ elf-entry-point ( a -- ) \ Set the entry point.
\ elf-extra-bytes ( u -- ) \ Add uninitialized bytes to the end of the image.
\ elf-end ( -- ) \ End image, update header.
\ elf-file-size ( u -- ) \ Set program file size.
\ elf-memory-size ( u -- ) \ Set program memory size.

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

: elf-start ( a -- ) dup start !  44 + fsize ! ;
: start- ( a -- u ) >host start @ >host - ;
: elf-extra-bytes   extra ! ;

: elf-entry-point   start @ 18 + ! ;
: elf-file-size   fsize @ ! ;
: elf-mem-size   fsize @ 4 + ! ;

( ELF header )

: ident,   7F c, ," ELF" 00010101 w, 8 zeros, ;
: type, ( u -- ) executable h, h, 1 w, ;
: entry,   0 a, ;
: tables,   34 h, 20 h, 1 h, 28 h, 4 zeros, ; \ 6 zeros, ;
: ehdr, ( a u -- a ) ident, type, entry, 34 a, 0 a, 0 w, tables, ;

( Program header )

: phdr32, ( a -- )   1 w, 0 a, dup a, a, here fsize ! 0 a, 0 a, 7 w, 1000 w, ;
: phdr64, ( a -- )   1 w, 5 w, 0 a, dup a, a, here fsize ! 0 a, 0 a, 1000 w, ;

( Section header )

\ shdr,   name type flags addr offset size link info align size

( Lay down an ELF header in the dictionary. )

: elf32-header, ( a u -- ) here elf-start  ehdr, phdr32, ;
: elf-end   align here start- dup elf-file-size  extra @ + elf-mem-size ;

base !
