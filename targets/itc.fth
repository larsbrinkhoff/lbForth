\ Target definitions for indirect threaded code.

: code! ( a -- ) latestxt >code ! ;
: does! ( a -- ) code! ;
: does, ( a -- ) call, align ;
: ?code, ( -- ) here cell+ , ;
: compile, ( xt -- ) , ;

\ CODE! -- Rewrite the code field of the latest definition.
\ DOES! -- Rewrite the code field of the latest definition to use DOES> code.
\ DOES, -- Compile a call instruction if the target requires it for DOES>.
\ ?CODE, -- Compile a code field for CODE.
