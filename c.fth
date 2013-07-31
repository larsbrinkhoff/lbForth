variable dp
: here      dp @ ;
: allot     dp +! ;
: aligned   cell + 1 - cell negate nand invert ;
: align     dp @ aligned dp ! ;

: ,          here !  cell allot ;
: c,         here c!  1 allot ;
: compile,   , ;
: string,    here over allot align  swap cmove ;

variable current

\ Compile the contents of a, then store x in a.
: chain, ( x a -- )   dup @ , ! ; 

forward: latestxt
forward: >body
: link, ( nt -- )      to latestxt  current @ >body @ , ;
: reveal               latestxt  current @ >body ! ;
: #name ( -- u )       NAME_LENGTH 1 - ;
: name, ( a u -- )     #name min c,  #name string, ;
: header, ( a u -- )   align here >r name, r> link, 0 , ;



: >name    count cabs ;
: >lfa     TO_NEXT + ;
: >does    TO_DOES + ;
: >code    TO_CODE + ;
: >body    TO_BODY + ;
: >nextxt   >lfa @ ;



: lowercase? ( c -- flag )   dup [char] a < if drop 0 exit then [char] z 1+ < ;
: upcase ( c1 -- c2 )   dup lowercase? if [ char A char a - ] literal + then ;
: c<> ( c1 c2 -- flag )   upcase swap upcase <> ;

: name= ( ca1 u1 ca2 u2 -- flag )
   2>r r@ <> 2r> rot if 3drop 0 exit then
   bounds do
      dup c@ i c@ c<> if drop unloop 0 exit then
      1+
  loop drop -1 ;
: nt= ( ca u nt -- flag )   >name name= ;

: immediate?   c@ 127 > if 1 else -1 then ;

\ TODO: nt>string nt>interpret nt>compile
\ Forth83: >name >link body> name> link> n>link l>name

: traverse-wordlist ( wid xt -- ) ( xt: nt -- continue? )
   >r >body @ begin dup while
      r@ over >r execute r> swap
      while >nextxt
   repeat then r> 2drop ;

: ?nt>xt ( -1 ca u nt -- 0 xt i? 0 | -1 ca u -1 )
   3dup nt= if >r 3drop 0 r> dup immediate? 0
   else drop -1 then ;
: search-wordlist ( ca u wl -- 0 | xt 1 | xt -1 )
   2>r -1 swap 2r> ['] ?nt>xt traverse-wordlist
   rot if 2drop 0 then ;



0 [if]
variable state
: [   0 state !  ['] execute interpreters !  previous ; immediate
: ]   1 state !  ['] compile, interpreters !
   also ['] compiler-words context ! ;

variable csp

: .latest   latestxt >name type ;
forward: abort
: !csp   csp @ if ." Nested definition: " .latest cr abort then  sp@ csp ! ;
: ?csp   sp@ csp @ <> if ." Unbalanced definition: " .latest cr abort then
   0 csp ! ;

: :   parse-name header, postcode enter ] !csp ;
: ;   reveal postpone exit postpone [ ?csp ; immediate



( From core.fth )

: immediate   latestxt dup c@ negate swap c! ;
: create    parse-name header, postcode dodoes reveal (does>) ;
: constant   create , does> @ ;
: variable   create cell allot ;

: '   parse-name find-name 0branch [ >mark ] exit [ >resolve ]
   [ char U ] literal emit  [ char n ] literal emit
   [ char d ] literal emit  [ char e ] literal emit
   [ char f ] literal emit  [ char i ] literal emit
   [ char n ] literal emit  [ char e ] literal emit
   [ char d ] literal emit  bl emit
   [ char w ] literal emit  [ char o ] literal emit
   [ char r ] literal emit  [ char d ] literal emit
   [ char : ] literal emit  bl emit
   count type cr abort ;

( From tools.fth )

: [undefined]   parse-name find-name if drop 0 else 2drop -1 then ; immediate
: [defined]     postpone [undefined] 0= ; immediate
[then]
