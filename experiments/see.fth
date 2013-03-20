\ Decompiler.

\ For code words.
\ s" experiments/dis-x86.fth" included
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
   \ 0branch (s") ... (abort") => abort"
   ' 0branch ,    ' see-0branch ,
   ' (s") ,       ' see-s" ,
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
: see-data ( end start -- )     do cr i .addr i @ see-cell ."  ," /cell +loop ;
: see-thread ( end start --)    begin see-line 2dup = until 2drop ;
: xt-bounds ( xt -- end start)  dup >end swap >body ;
\ TODO: end of does>
: does-bounds ( xt--end start)  >does @ dup 40 + swap ;
: .immediate ( xt -- )          immediate? 0> if ."  immediate" then ;
: .; ( xt -- )                  ."  ;" immediate? 0> if ."  immediate" then ;
: see-colon ( xt -- )           ." : " dup id.  dup xt-bounds see-thread  .; ;

: see-does ( xt -- )
   ." create " dup id.
   dup xt-bounds see-data
   cr ." does> "
   dup does-bounds see-thread
   .; ;

\ TODO: end of code?
: see-code ( xt -- )
   ." code " dup id. cr
   >code @ dup 100 + disassemble
   ." end-code" ;

create see-codes
   ' enter >code @ ,  ' see-colon ,
   ' dodoes >code @ ,  ' see-does ,
here constant see-end

: SEE-XT ( xt -- )
    dup >code @ ( xt cfa )
    see-end see-codes do
       dup i @ = if drop i cell+ @ execute unloop exit then
    2 cells +loop drop see-code ;

: SEE   ' SEE-XT cr ;

(
   if x else y then
1: 0branch		<6> "if"
2: <6>
3: x			"x"
4: branch		<6 7> "ahead"
5: <7>
6: y			<7> "y" "[ 1 cs-roll ] then"
7:			"then"

   begin x again
1: x			"begin" "x"
2: branch		"again"
3: <1>

   begin x until
1: x			"begin" "x"
2: 0branch		"until"
3: <1>

   begin x while y repeat
1: x			"begin" "x"
2: 0branch		<7> "if"
3: <7>
4: y			"y"
5: branch		"again"
6: <1>			"then"
7:

 1: (literal)		"<n>"	"[char] x"
 2: <n>
 3: (literal)		"['] word"
 4: <xt>
 5: (literal)		"postpone word" (default)
 6: <xt>
 7: compile,
 8: (s")		s" ..."
 9: <u>
10: ccc...
11: (0branch)		"abort" ...""
12: (s")
13: 

1: (does>)		"does>"
2: <latstxt>		"recurse"
3: exit			";" (last in body)
)
