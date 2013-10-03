require lib/macros.fth

: y1 ( xt1 -- xt2 )   here tuck 2>r  1 cells allot
   :noname r> ]] literal @ [[ r> compile, ]] ; [[  dup rot ! ;

: y2 ( xt "name" -- )    >in @ create >in ! , ' ,  does> 2@ execute ;

:noname ( u xt -- )   swap dup . 1- ?dup if swap execute else drop then ;
y2 count
10 count

:noname ( u1 xt -- u2 )   over ?dup if 1- swap execute * else 2drop 1 then ;
y1 6 swap execute .
