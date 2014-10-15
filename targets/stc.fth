\ Target definitions for subroutine threaded code.

: code! ( a -- ) latestxt >code jump! ;
: does! ( a -- ) latestxt >code call! ;
: does, ( a -- ) call, ;
: ?code, ( -- ) ;
: compile, ( xt -- ) call, ;
