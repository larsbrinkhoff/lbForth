\ -*- forth -*- Copyright 2004, 2013 Lars Brinkhoff

( Exception words. )

\ From dpANS A.9:

variable handler   0 handler !  \ last exception handler

: catch  ( xt -- exc# | 0 )
    sp@ >r  handler @ >r  rp@ handler !
    execute
    r> handler !  r> drop  0 ;

' catch catcher !

: throw  ( ex# -- <no return> )
    ?dup if  handler @ rp!  r> handler ! r> swap >r  sp! drop r>  then ;

( Exception extension words. )

:redefine abort   -1 throw ;

:redefine (abort")   cr type cr -2 throw ;
