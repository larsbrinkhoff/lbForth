\ Intel HEX format.

16 constant /line

variable sum
: .2h   dup sum +!  0 <# # # #> type ;
: .4h   dup 8 rshift .2h .2h ;
: 0sum   0 sum ! ;
: .sum   sum @ negate .2h ;

variable addr
: addr! ( a1 a2 u -- a2 u ) addr ! ;
: .addr   addr @ .4h  /line addr +! ;

: .:   ." :" ;
: .len   /line .2h ;
: .type   0 .2h ;
: .start   .: .len .addr .type ;
: .data   /line bounds do i c@ .2h loop ;
: .line   0sum .start .data .sum cr ;
: .lines ( a u -- ) bounds do i .line /line +loop ;
: .end   ." :00000001FF" cr ;

\ Type an Intel HEX file with target address at a1, and data in a2 u.
: type-hex ( a1 a2 u -- ) rot addr!  base @ >r hex .lines r> base !  .end ;
