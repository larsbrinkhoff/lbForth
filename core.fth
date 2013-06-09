: immediate   lastxt @ dup c@ negate swap c! ;

: \   source nip >in ! ; immediate



\ Copyright 2004, 2013 Lars Brinkhoff



: char \ ( "word" -- char )
    bl-word here 1+ c@ ;

: >mark      here 0 , ;
: >resolve   here swap ! ;
: <mark      here ;
: <resolve   , ;

: '   bl-word here find 0branch [ >mark ] exit [ >resolve ]
      [ char U ] literal emit
      [ char n ] literal emit
      [ char d ] literal emit
      [ char e ] literal emit
      [ char f ] literal emit
      [ char i ] literal emit
      [ char n ] literal emit
      [ char e ] literal emit
      [ char d ] literal emit
      bl emit
      [ char w ] literal emit
      [ char o ] literal emit
      [ char r ] literal emit
      [ char d ] literal emit
      [ char : ] literal emit
      bl emit
      count type cr abort ;

: here!   here - allot ;
: >h      here >r here! ;
: h>      r> here! ;

: code!     lastxt @ >code ! ;
: does!     lastxt @ >does ! ;
: does,     ;
: (does>)   r> does! ;
: does>     [ ' (does>) ] literal compile, does, ; immediate
: create    [ ' dodoes >code @ ] literal header, reveal (does>) ;

: postpone,   [ ' literal , ' compile, ] literal compile, ;
\ Same as:    postpone literal  postpone compile, ;

: finders   create ' , ' , ' ,   does> swap 1+ cells + @ execute ;

finders postpone-xt   postpone, abort compile,

: word \ ( char "<chars>string<char>" -- caddr )
    drop bl-word here ;

: postpone   bl word find postpone-xt ; immediate

: chars \ ( n1 -- n2 )
    ;

: unresolved   postpone postpone  postpone >mark ; immediate
: ahead        unresolved branch ; immediate
: if           unresolved 0branch ; immediate
: then         >resolve ; immediate

: resolving   postpone postpone  postpone <resolve ; immediate
: begin       <mark ; immediate
: again       resolving branch ; immediate
: until       resolving 0branch ; immediate

: else     postpone ahead swap postpone then ; immediate
: while    postpone if swap ; immediate
: repeat   postpone again postpone then ; immediate

: recurse   lastxt @ compile, ; immediate

: pad \ ( -- addr )
    here 1024 + ;

: parse \ ( char "string<char>" -- addr n )
    pad >r  begin
	source? if <source 2dup <> else 0 0 then
    while
	r@ c!  r> 1+ >r
    repeat  2drop  pad r> over - ;

: [char]   char  postpone literal ; immediate

: 1-   -1 + ;

: (   begin [char] ) parse 2drop
      source drop >in @ 1- + c@ [char] ) <> while
      refill while repeat then ; immediate

: hide   lastxt @ >nextxt  current @ >body ! ;

: link   current @ >body @  lastxt @ >lfa ! ;

: compile-only   hide  current @  [ ' compiler-words ] literal current !
                 link  reveal  current ! ;

: string, ( addr n -- )
    here over allot align  swap cmove ;

: (s") ( -- addr n ) ( R: ret1 -- ret2 )
    r> dup @ swap cell+ 2dup + aligned >r swap ;

create squote   128 allot

: s"   [char] " parse  >r squote r@ cmove  squote r> ;
: s"   postpone (s")  [char] " parse  dup ,  string, ; immediate compile-only

: (abort") ( ... addr n -- ) ( R: ... -- )
    cr type cr abort ;

: abort" ( ... x "string<quote>" -- ) ( R: ... -- )
    postpone if  postpone s"  postpone (abort")  postpone then ; immediate

\ ----------------------------------------------------------------------

( Core words. )

\ TODO: #
\ TODO: #>
\ TODO: #s

: and   nand invert ;

: 2*   dup + ;

: *   1 2>r 0 swap begin r@ while
         2r> 2dup 2* 2>r and if swap over + swap then 2*
      repeat 2r> 3drop ;

\ TODO: */mod

: msb   1 2 begin ?dup while nip dup 2* repeat postpone literal ; immediate

: 0<   msb and if -1 else 0 then ;

: under   postpone >r ' compile, postpone r> ; immediate

: bits/cell   0 1 begin ?dup while 2* under 1+ repeat
              postpone literal ; immediate

: rshift   >r 0 begin r> dup bits/cell < while 1+ >r
           2* over 0< if 1+ then under 2* repeat drop nip ;

\ Since "an ambiguous condition exists if u is greater than or equal
\ to the number of bits in a cell", this would be acceptable.
\ : rshift   0 bits/cell rot do 2* over 0< if 1+ then under 2* loop nip ;

: u/mod ( n d -- r q )
    ?dup 0= abort" Division by zero"
    0 >r 2>r		\ R: q n
    0 1 begin ?dup while dup 2* repeat
    r> 0 begin		\ S: [...1<<i...] d r
      2*		\ r <<= 1
      r@ 0< if 1+ then	\ if n&msb then r++
      r> 2* >r		\ n <<= 1
      2dup > if rot drop else \ if r>=d
        over -		      \ r -= d
        rot r> r> rot + >r >r \ q += 1<<i
      then
      2>r ?dup 2r> rot 0= until
    nip r> drop r> ;

: /mod   dup 0= abort" Division by zero"
         dup 0< if negate recurse negate else
         over 0< if under negate u/mod negate else u/mod then then ;

: space   bl emit ;

: ?.-  dup 0< if [char] - emit negate then ;

: digit   dup 9 > if [ char a 10 - ] literal else [char] 0 then + emit ;

: (.)   base @ u/mod  ?dup if recurse then  digit ;

: ." ( "string<quote>" -- )   postpone s"  postpone type ; immediate

: . ( x -- )   ?.- (.) space ;

: postpone-number   ." Undefined: " count type cr abort ;
' postpone-number  ' postpone-xt >body cell+ !

: /     /mod nip ;
: mod   /mod drop ;

\ This could probably be done faster with something similar to rshift.
: 2/   dup 0< if 1- then 2 / ;

: 2@      dup cell+ @ swap @ ;
: 2!      swap over ! cell+ ! ;
: 2over   >r >r 2dup r> rot rot r> rot rot ;
: 2swap   >r rot rot r> rot rot ;

\ TODO: <#

: abs ( n -- |n| )
    dup 0< if negate then ;

: c, ( n -- )
    here c!  1 chars allot ;

: char+ ( n1 -- n2 )
    1+ ;

: constant   create , does> @ ;

: decimal ( -- )
    10 base ! ;

: depth   data_stack 100 cells +  sp@  - cell /  1- ;

: variable   create cell allot ;

variable leaves

: >mark!      here swap dup @ , ! ;
: >resolve@   @ begin ?dup while dup @ here rot ! repeat ;

: r+   r> r> rot + >r >r ;

: do      leaves @  0 leaves !  postpone 2>r  postpone begin  0 ; immediate
: leave   postpone branch  leaves >mark! ; immediate
: +loop   ?dup if swap postpone r+ postpone again postpone then
          else postpone (+loop) postpone until then
          leaves >resolve@  leaves !  postpone unloop ; immediate
: loop    1 postpone literal postpone +loop ; immediate

: j   rp@ 3 cells + @ ;

: environment?   2drop 0 ;

\ TODO: evaluate
\ TODO: fill
\ TODO: fm/mod )
\ TODO: hold

: lshift   begin ?dup while 1- under 2* repeat ;

: max ( x y -- max[x,y] )
    2dup > if drop else nip then ;

\ TODO:   mod
\ TODO:   move

: (quit) ( R: ... -- )
    return_stack 100 cells + rp!  0 csp !
    0 'source-id !  tib ''source !  #tib ''#source !
    postpone [
    begin
	[ ' refill ] literal catch if ." Exception" cr -1 then
    while
	interpret  state @ 0= if ."  ok" cr then
    repeat
    bye ;

' (quit)  ' quit >body cell+  !

\ TODO: s>d
\ TODO: sign
\ TODO: sm/rem

: spaces ( n -- )
    0 do space loop ;

: u.   (.) space ;

: xor ( x y -- x^y )    2dup nand >r r@ nand swap r> nand nand ;

: u<   2dup 0< swap 0< over <> if nip nip else drop - 0< then ;

\ TODO: um/mod

: [']   ' postpone literal ; immediate

: accept ( caddr u1 -- u2 )
    2dup bounds do
       source? if <source i c!
       else drop i swap - unloop exit then
    loop nip ;
