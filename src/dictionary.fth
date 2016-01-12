\ Definitions that are common to both the kernel and the metacompiler.
\ Dictionary: DP HERE ALLOT ALIGN CURRENT.
\ Word fields: >NAME >LFA >CODE >BODY.
\ Compiling words: , COMPILE, HEADER,.

variable dp
: here      dp @ ;
: allot     dp +! ;
: align     dp @ aligned dp ! ;

: ,       here !  cell allot ;
: c,      here c!  1 allot ;
: move,   here swap dup allot cmove ;
: ",      move, align ;

include jump.fth

: >lfa     TO_NEXT + ;
: >code    TO_CODE + ;
: >body    TO_BODY + ;
: >nextxt   >lfa @ ;

include threading.fth

variable current

\ Compile the contents of a, then store x in a.
: chain, ( x a -- )   dup @ , ! ; 

: latest! ( a1 a2 -- ) to latest to latestxt ;
: link,   dup latest!  current @ >body @ , ;
: reveal   latest ?dup if current @ >body ! then ;

: cells   [ cell 1 > ] [if] dup + [then]
   [ cell 2 > ] [if] dup + [then]
   [ cell 4 > ] [if] dup + [then] ;
