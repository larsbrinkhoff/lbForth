\ Target definitions for direct threaded code.

: code! ( a -- ) latestxt >code jump! ;
: does! ( a -- ) latestxt >code call! ;
: does, ( a -- ) call, ;
: ?code, ( -- ) ;
: compile, ( xt -- ) , ;
