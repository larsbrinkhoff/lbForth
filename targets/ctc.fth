\ Target definitions for call threaded code (the C target).

: >does ( a -- a' ) TO_DOES + ;
: code! ( a -- ) latestxt >code ! ;
: does! ( a -- ) 'dodoes code!  latestxt >does ! ;
: does, ( a -- ) drop ;
: ?code, ( -- ) here cell+ , ;
: compile, ( xt -- ) , ;
