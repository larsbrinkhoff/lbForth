\ "Assembler" for C target.

\ Adds to FORTH vocabulary: ASSEMBLER CODE.
\ Creates ASSEMBLER vocabulary.

s" search.fth" included

vocabulary assembler
also assembler definitions

create code-line 128 allot

previous definitions  also assembler

: code ( "name" -- )
   here 0 header,
   ." xt_t * REGPARM "
   count type
   ." _code (xt_t *IP, struct word *word)" cr
   ." {" cr
   begin
      code-line 128 accept
      code-line over s" end-code" compare 0=
   while
      type
   repeat
   ." }" cr ;

: C ( "word" -- )
   bl word
   \ save string, return addres
   t-string, ;

previous
