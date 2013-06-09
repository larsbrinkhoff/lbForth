\ "Assembler" for C target.

\ Adds to FORTH vocabulary: ASSEMBLER CODE.
\ Creates ASSEMBLER vocabulary.

s" search.fth" included

vocabulary assembler
also assembler definitions

create code-line 128 allot

: c-function ( a -- )
   ." xt_t * REGPARM " count type ." _code (xt_t *IP, struct word *word)" cr
   ." {" cr ;

: c-line ( -- a u flag )    refill 0= abort" End of file inside CODE."
   code-line dup 128 accept  2dup s" end-code" compare ;

previous definitions  also assembler

: code ( "name" -- )   here 0 header, reveal  c-function
   begin c-line while type cr repeat  2drop ." }" cr ;

previous
