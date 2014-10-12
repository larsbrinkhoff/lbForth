\ Target definitions for indirect threaded code.

: does! ( a -- ) latestxt >code ! ;
: code! ( a1 a2 -- ) ! ;
: does, ( a -- ) call, ;
: ?code, ( -- ) here cell+ , ;
: compile, ( xt -- ) , ;

\ DOES! -- Rewrite the latest definition to use DOES> code.
\ CODE! -- Write a1 to the code field at a2.
\ DOES, -- Compile a call instruction if the target requires it for DOES>.
\ ?CODE, -- Compile a code field for CODE.
