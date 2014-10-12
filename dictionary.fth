\ Definitions that are common to both the kernel and the metacompiler.
\ Word fields: >NAME >LFA >DOES >CODE >BODY.
\ Compiling words: , COMPILE, HEADER,.
\ Defining words: : ; IMMEDIATE CREATE CONSTANT VARIABLE.
\ Wordlist: TRAVERSE-WORDLIST SEARCH-WORDLIST.

variable dp
: here      dp @ ;
: allot     dp +! ;
: aligned   cell + 1 - cell negate nand invert ;
: align     dp @ aligned dp ! ;

: ,          here !  cell allot ;
: c,         here c!  1 allot ;
: string,    here over allot align  swap cmove ;

include targets/ctc.fth

variable current

\ Compile the contents of a, then store x in a.
: chain, ( x a -- )   dup @ , ! ; 

: >name    count cabs ;
: >lfa     TO_NEXT + ;
: >code    TO_CODE + ;
: >body    TO_BODY + ;
: >nextxt   >lfa @ ;

: link, ( nt -- )      to latestxt  current @ >body @ , ;
: reveal               latestxt  current @ >body ! ;
: #name ( -- u )       NAME_LENGTH 1 - ;
: name, ( a u -- )     #name min c,  #name string, ;
: header, ( a u -- )   align here >r name, r> link, 0 , ;



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
: (find) ( ca u wl -- ca u 0 | xt 1 | xt -1 )
   2>r -1 swap 2r> ['] ?nt>xt traverse-wordlist rot if 0 then ;
: search-wordlist ( ca u wl -- 0 | xt 1 | xt -1 )
   (find) ?dup 0= if 2drop 0 then ;

: cells   [ cell 1 > ] [if] dup + [then]
   [ cell 2 > ] [if] dup + [then]
   [ cell 4 > ] [if] dup + [then] ;
