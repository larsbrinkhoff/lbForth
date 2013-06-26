( File Access words. )

\ bin
\ create-file
\ delete-file
\ file-position
\ file-size
\ r/w
\ read-line
\ reposition-file
\ resize-file
\ w/o
\ write-file
\ write-line

\ ----------------------------------------------------------------------

( File Access extension words. )

\ file-status
\ flush-file
\ rename-file

( Forth12 )

: i? ( 0 a u nt -- 0 a u 1 | -1 x x 0 )   count 2over compare
   if 1 else 3drop -1 0 0 0 then ;
: included? ( a u -- ? )   0 -rot ['] included-files ['] i?
   traverse-wordlist 2drop ;
: required   2dup included? if 2drop else included then ;
: include   parse-name included ;
: require   parse-name required ;
