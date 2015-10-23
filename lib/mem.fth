\ Memory access: different word sizes, endianness.

( Half word, 16 bits. )

: h@ ( a -- x )   dup 1+ c@ 8 lshift swap c@ + ;
: h! ( x a -- )   2dup c!  swap 8 rshift swap 1+ c! ;
: h, ( x -- )   here h!  2 allot ;

( Word, 32 bits. )

: w@ ( a -- x )   dup 2 + h@ 16 lshift swap h@ + ;
: w! ( x a -- )   2dup h!  swap 16 rshift swap 2 + h! ;
: w, ( x -- )   here w!  4 allot ;
