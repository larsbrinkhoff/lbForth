\ Memory access: different word sizes, endianness.

( Half word, 16 bits. )

: h@ ( a -- x )   dup 1+ c@ 8 lshift swap c@ + ;
: h! ( x a -- )   2dup c!  swap 8 rshift swap 1+ c! ;
: h, ( x -- )   here h!  2 allot ;

( Word, 32 bits. )

: w@ ( a -- x )   dup 2 + h@ 16 lshift swap h@ + ;
: w! ( x a -- )   2dup h!  swap 16 rshift swap 2 + h! ;
: w, ( x -- )   here w!  4 allot ;


: l2c@   dup 1+ c@ 8 lshift swap c@ + ;
: l2c!   2dup c!  swap 8 rshift swap 1+ c! ;
: l2c,   dup c, 8 rshift c, ;
: l2c+!   tuck l2c@ + swap l2c! ;

: l4c@   dup 2 + l2c@ 16 lshift swap l2c@ + ;
: l4c!   2dup l2c!  swap 16 rshift swap 2 + l2c! ;
: l4c,   dup l2c, 16 rshift l2c, ;
: l4c+!   tuck l4c@ + swap l4c! ;

: b2c@   dup 1+ c@ swap c@ 8 lshift + ;
: b2c!   2dup 1+ c!  swap 8 rshift swap c! ;
: b2c,   dup 8 rshift c, c, ;
: b2c+!   tuck b2c@ + swap b2c! ;

: b4c@   dup 2 + b2c@ swap b2c@ 16 lshift + ;
: b4c!   2dup 2 + b2c!  swap 16 rshift swap c! ;
: b4c,   dup 16 rshift b2c, b2c, ;
: b4c+!   tuck b4c@ + swap b4c! ;

\ targets/m68k/asm.fth		h w
\ targes/x16/meta.fth		h
\ targes/x86/asm.fth		h w
\ targes/msp430/asm.fth		h
\ test/test-*-asm.fth		w
\ lib/pe.fth			h w <- require lib/mem.fth
\ lib/elf.fth			h w
\ lib/gemdos.fth		w l
