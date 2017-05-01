\ Create a PDP-11 Unix executable file in memory.

require lib/common.fth

also forth
base @
octal
: h] ] ;
previous

( Constants )

407 constant magic

0 constant flags

decimal

[undefined] >host [if] : >host ; [then]

( Data types )

: dp!   dp [ also forth h] ! [ previous h] ;

: w, ( x -- ) dup c, 8 rshift c, ;
: w! ( x a -- ) here >r dp! w, r> dp! ;
: zeros, ( u -- ) here swap dup allot erase ;

( Data structures )

also forth
variable start
variable extra  0 extra !

: extra@   extra @ ;
: aout-extra-bytes   extra ! ;

: aout-start   start ! ;
: aout-text   start @ 2 + w! ;
: aout-data   start @ 4 + w! ;
: aout-bss   start @ 6 + w! ;
: aout-entry-point   start @ 10 + w! ;
previous

( a.out header )

: aout,   magic w,  12 zeros,  flags w, ;

: aout-header,   here aout-start  aout, ;
: aout-end   here aout-data  extra@ aout-bss ;

also forth  base !  previous
