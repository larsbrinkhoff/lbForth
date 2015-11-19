require search.fth

\ Forward references for cross compilation.

vocabulary forward-refs

\ Create a new forward reference.
also forth
: chain ( a x1 -- x2 ) over @ >r  swap !  r> ;
previous
: chain, ( a -- ) here chain , ;
: chain! ( x a -- x a' ) 2dup @ >r  swap !  r> ;
: does-forward   does> chain, ;
also forth
: f-create   get-current ['] forward-refs set-current create set-current ;
: forward: ( "name" -- ) f-create 0 , does-forward ;


\ Resolve forward references.
: ?found   0= if cr ." Unresolved forward reference: " type cr abort then ;
: xresolving ( xt -- x ) >name find-name ?found >body @ ;
: chains! ( x a -- ) begin dup while chain! repeat 2drop ;
: resolve ( xt -- f ) dup xresolving  swap >body @  chains!  -1 ;
: resolve-all-forward-refs   ['] forward-refs ['] resolve traverse-wordlist ;
previous
