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

decimal
002 constant sparc
003 constant x86
004 constant m68k
008 constant mips
020 constant ppc
021 constant ppc64
045 constant arc
052 constant coldfire
062 constant x64
040 constant arm
043 constant sparcv9
064 constant pdp10
065 constant pdp11
075 constant vax
083 constant avr
087 constant v850
092 constant openrisc
093 constant arccompact
094 constant xtensa
105 constant msp430
183 constant arm64
189 constant microblaze
220 constant z80
222 constant ft32
243 constant riscv
hex

[undefined] >host [if] : >host ; [then]

( Data types )

h-[defined] t-little-endian [if]
   t-little-endian [if]
      1 constant endian
      : h, ( x -- )   dup c,  8 rshift c, ;
      : w, ( x -- )   dup h,  10 rshift h, ;
   [else]
      2 constant endian
      : h, ( x -- )   dup 8 rshift c, c, ;
      : w, ( x -- )   dup 10 rshift h, h, ; \ 32 bits
   [then]
[then]
: a, ( x -- )   w, ; \ 32 or 64 bits
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

: ident,   7F c, ," ELF" 1 c, endian c, 1 c, 0 c, 8 zeros, ;
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
