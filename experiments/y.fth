\ Address of an xt.
variable 'xt
\ Make room for an xt.
: xt, ( -- ) here 'xt !  1 cells allot ;
\ Store xt.
: !xt ( xt -- ) 'xt @ ! ;
\ Compile fetchign the xt.
: @xt, ( -- ) 'xt @ postpone literal postpone @ ;
\ Compile the Y combinator.
: y, ( xt1 -- xt2 ) >r :noname @xt, r> compile, postpone ; ;
\ Make a new instance of the Y combinator.
: y1 ( xt1 -- xt2 ) xt, y, dup !xt ;

: y2 ( xt "name" -- )    >in @ create >in ! , ' ,  does> 2@ execute ;

:noname ( u xt -- )   swap dup . 1- ?dup if swap execute else drop then ;
y2 count
10 count

:noname ( u1 xt -- u2 )   over ?dup if 1- swap execute * else 2drop 1 then ;
y1 6 swap execute .
