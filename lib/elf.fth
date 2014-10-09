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

( Data types )

: h, ( x -- )   dup c,  8 rshift c, ;
: w, ( x -- )   , ; \ 32 bits
: a, ( x -- )   , ; \ 32 or 64 bits
: zeros, ( u -- )   here swap dup allot erase ;

( ELF header )

variable entry-offset
54 entry-offset !

: ident, ( -- )   7F c, s" ELF" string, 00010101 w, 8 zeros, ;
: type, ( u -- )   executable h, h, 1 w, ;
: entry, ( a -- a )   dup entry-offset @ + a, ;
: tables, ( -- )   34 h, 20 h, 1 h, 6 zeros, ;
: ehdr, ( a u -- a )   ident, type, entry, 34 a, 0 a, 0 w, tables, ;

( Program header )

: phdr32, ( a1 -- a2 )   1 w, 0 a, dup a, a, here 0 a, 0 a, 5 w, 1000 w, ;
: phdr64, ( a1 -- a2 )   1 w, 5 w, 0 a, dup a, a, here 0 a, 0 a, 1000 w, ;

( Section header )

\ shdr,   name type flags addr offset size link info align size

( Lay down an ELF header in the dictionary. )

: elf32, ( a1 u -- a2 a3 )   here -rot ehdr, phdr32, ;
: elf! ( a1 a2 -- )   align here rot - dup rot 2! ;

base !
