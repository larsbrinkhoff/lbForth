\ Memory access: different word sizes, endianness.

( Half word, 16 bits. )

: h@ ( a -- x )   dup 1+ c@ 8 lshift swap c@ + ;
: h! ( x a -- )   2dup c!  swap 8 rshift swap 1+ c! ;
: h, ( x -- )   here h!  2 allot ;

( Word, 32 bits. )

: w@ ( a -- x )   dup 2 + h@ 16 lshift swap h@ + ;
: w! ( x a -- )   2dup h!  swap 16 rshift swap 2 + h! ;
: w, ( x -- )   here w!  4 allot ;


: 2l@   dup 1+ c@ 8 lshift swap c@ + ;
: 2l!   2dup c!  swap 8 rshift swap 1+ c! ;
: 2l,   dup c, 8 rshift c, ;
: 2l+!   tuck 2l@ + swap 2l! ;

: 4l@   dup 2 + 2l@ 16 lshift swap 2l@ + ;
: 4l!   2dup 2l!  swap 16 rshift swap 2 + 2l! ;
: 4l,   dup 2l, 16 rshift 2l, ;
: 4l+!   tuck 4l@ + swap 4l! ;

: 2b@   dup 1+ c@ swap c@ 8 lshift + ;
: 2b!   2dup 1+ c!  swap 8 rshift swap c! ;
: 2b,   dup 8 rshift c, c, ;
: 2b+!   tuck 2b@ + swap 2b! ;

: 4b@   dup 2 + 2b@ swap 2b@ 16 lshift + ;
: 4b!   2dup 2 + 2b!  swap 16 rshift swap c! ;
: 4b,   dup 16 rshift 2b, 2b, ;
: 4b+!   tuck 4b@ + swap 4b! ;

\ targets/m68k/asm.fth		h w
\ targes/x16/meta.fth		h
\ targes/x86/asm.fth		h w
\ targes/msp430/asm.fth		h
\ test/test-*-asm.fth		w
\ lib/pe.fth			h w <- require lib/mem.fth
\ lib/elf.fth			h w
\ lib/gemdos.fth		w l
