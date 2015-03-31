\ Decompiler.

\ For code words.
\ s" targets/x86/asm.fth" included
: disassemble   2drop ." " ;

: postpone?     over @ ['] compile, = ;
: see-tick      postpone? if ." postpone " id. cell+ else ." ['] " id. then ;
: body>         [ 0 >body ] literal - ;
: to?           body> xt? ;
: see-to        ." to " body> id. cell+ ;
: see-literal   @+ dup xt? if see-tick else
                dup to? if see-to else . then then ;
: see-branch    dup @+ rot < if ." again" else ." ahead" then ;
: see-0branch   dup @+ rot < if ." until" else ." if" then ;
: .s"           [char] s emit [char] " emit bl emit ;
: .."           [char] . emit [char] " emit bl emit ;
: type?         dup @ ['] type = if .." cell+ else .s" then ;
: see-s"        @+ 2dup + aligned type? >r type [char] " emit r> ;

create see-xts
   \ n (+loop) until unloop => n loop
   \ n r+ again unloop => n loop
   ' (literal) ,  ' see-literal ,
   ' branch ,     ' see-branch ,
   \ 0branch (sliteral) ... (abort") => abort"
   ' 0branch ,    ' see-0branch ,
   ' (sliteral) , ' see-s" ,
 \ ' 2>r ,        ' see-do , \ check for 2>r (?do)
 \ ' (+loop) ,    ' see-loop ,
 \ ' r+ ,         ' see-loop ,
 \ ' exit ,       ' see-exit ,
here constant see-end

: ?postponed ( xt -- )   immediate? 0> if ." postpone " then ;
: .xt ( xt -- )   dup xt? if dup ?postponed id. else . then ;

: see-xt ( addr -- addr' )
    @+ see-end see-xts do
       dup i @ = if drop i cell+ @ execute unloop exit then
    2 cells +loop .xt ;

: see-line ( addr -- addr' )    cr dup .addr  see-xt ;
: see-cell ( x -- )             dup xt? if ." ' " id. else . then ;
: see-data ( end start -- )     do cr i .addr i @ see-cell ."  ," cell +loop ;
: see-thread ( end start --)    begin see-line 2dup = until 2drop ;
\ TODO: end of does>
: does-bounds ( xt--end start)  >does @ dup 40 + swap ;
: .immediate ( xt -- )          immediate? 0> if ."  immediate" then ;
: .; ( xt -- )                  ."  ;" immediate? 0> if ."  immediate" then ;
: see-colon ( xt -- )           ." : " dup id.  dup bounds' see-thread  .; ;
: colon? ( xt -- f )            >does @ [ ' : >does @ ] literal = ;

: see-does ( xt -- )
   dup colon? if see-colon exit then
   ." create " dup id.
   dup bounds' see-data
   cr ." does> "
   dup does-bounds see-thread
   .; ;

\ TODO: end of code?
: see-code ( xt -- )
   ." code " dup id. cr
   >code @ dup 100 + disassemble
   ." end-code" ;

create see-codes
   [defined] enter [if] ' enter >code @ ,  ' see-colon , [then]
   ' dodoes >code @ ,  ' see-does ,
here constant see-end

: see2' ( xt -- )
    dup >code @ ( xt cfa )
    see-end see-codes do
       dup i @ = if drop i cell+ @ execute unloop exit then
    2 cells +loop drop see-code ;

: see2   ' see2' cr ;
