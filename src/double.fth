( Double-Number words. )

: 2constant   create , ,  does> 2@ ;
: 2literal    swap postpone literal postpone literal ; compile-only
: 2variable   create 2 cells allot ;

\ d.
\ d.r

: d0<   nip 0< ;
: d2*   swap s>d negate swap 2* rot 2* rot + ;
: d2/   dup 1 and if [ 0 invert 1 rshift invert ] literal else 0 then
   swap 2/ rot 1 rshift rot + swap ;

: d<    rot > if 2drop -1 else u< then ;
: du<   rot u> if 2drop -1 else u< then ;

: d0=   or 0= ;
: d=    rot = -rot = and ;
: d>s   drop ;

: 4dup   2>r 2>r 2r@ 2r> 2r@ 2swap 2r> ;
: 2nip   2>r 2drop 2r> ;
: dmax   4dup d< if 2drop else 2nip then ;
: dmin   4dup d< if 2nip else 2drop then ;

: d-        dnegate d+ ;
: m+        s>d d+ ;

\ m*/ ( d n1 n2 -- d*n1/n2 )

( Double-Number extension words. )

: 2rot   2>r 2swap 2r> 2swap ;

( Forth12 Double-Number extension words. )

: value    create ['] ! , ,  does> cell+ @ ;
: 2value   create ['] 2! , , ,  does> cell+ 2@ ;

: to   ' >body dup ['] 2value < if ! else dup cell+ swap @ execute then ;
: to   ' >body dup ['] 2value < if postpone literal postpone !
   else dup cell+ postpone literal @ compile, then ; compile-only

( ... )

: 2tuck   over >r rot >r rot over r> r> rot ;
