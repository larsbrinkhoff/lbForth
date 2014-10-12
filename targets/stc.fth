\ Target definitions for subroutine threaded code.

: does! ( a -- ) call, ;
: code! ( a1 a2 -- ) jump! ;
: does, ( a -- ) call, ;
: ?code, ( -- ) ;
: compile, ( xt -- ) call, ;
