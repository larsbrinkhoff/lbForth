4 constant cell-size
16 constant next-offset
20 constant does-offset
24 constant code-offset
28 constant body-offset

\ This target holds a special primitive ID in the code field.
\ The inner interpreter looks up this ID to make a dispatch jump.
: code@   code-offset + @ ;

1024 constant load-address
: exe-header ;
: entry-point 0 ;
: exe-code ;
: extra-bytes ;
: exe-end ;
