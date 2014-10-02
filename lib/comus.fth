: on   -1 swap ! ;
: off   0 swap ! ;

: 2,   , , ;

\ Forth83
: @bits ( a u1 -- u2 ) swap @ and ;
: !bits ( u1 a u2 -- ) 2dup invert @bits >r rot and r> + swap ! ;
