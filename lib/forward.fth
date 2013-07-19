create forward-references 0 ,

: forward: ( "name" -- )   create immediate 0 ,
   latestxt forward-references chain,
   does> here swap chain, ;

: ?found   0= if cr ." Unresolved forward reference: " type cr abort then ;

: resolve ( xt -- )   dup >name find-name ?found  swap >body @
   \ TODO: merge with LEAVE resolution.
   begin dup while 2dup @ >r swap ! r> repeat 2drop ;

: resolve-all-forward-references   forward-references
  begin @ ?dup while dup resolve  >body cell+ repeat ;


(* Usage:
forward: foo
: bar          foo 1+ . ;
: foo   42 ;
resolve-all-forward-references
bar *)
