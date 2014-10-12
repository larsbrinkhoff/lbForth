\ Target definitions for call threaded code (the C target).

: >does ( a -- a' ) TO_DOES + ;
: does! ( a -- ) latestxt >does ! ;
: code! ( a1 a2 -- ) ! ;
: does, ( a -- ) drop ;
: ?code, ( -- ) here cell+ , ;
: compile, ( xt -- ) , ;
