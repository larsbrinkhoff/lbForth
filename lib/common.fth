: on   -1 swap ! ;
: off   0 swap ! ;

: c+!   dup >r c@ + r> c! ;
: c!+   tuck c! 1+ ;
: c@+   dup 1+ swap c@ ;
: "@+ ( a u -- a' u' c ) >r c@+ r> 1- swap ;

: place ( a u a2 ) 2dup c!  1+ swap cmove ;

: 2,   , , ;
: 3,   , , , ;
: 3@   @+ >r 2@ r> ;
: 3!   !+ 2! ;

: 4drop   2drop 2drop ;

\ Forth83
: @bits ( a u1 -- u2 ) swap @ and ;
: !bits ( u1 a u2 -- ) 2dup invert @bits >r rot and r> + swap ! ;

: octal   8 base ! ;
: binary   2 base ! ;

: ascii   char ;
: ascii   postpone [char] ; compile-only

: clear   begin depth while drop repeat ;
