\ Target definitions for direct threaded code.

: does! ( a -- ) call, ;
: code! ( a1 a2 -- ) jump! ;
: does, ( a -- ) call, ;
: ?code, ( -- ) ;
: compile, ( xt -- ) , ;
