\ -*- forth -*- Copyright 2004, 2013 Lars Brinkhoff

( Tools words. )

: .s   [char] < emit  depth (.)  ." > "
       sp@ cell - >r r@ depth 1- cells +
       begin dup r@ <> while
         dup @ . cell -
       repeat r> 2drop ;

: .s"   postpone s" postpone type postpone .s postpone cr ; immediate

: empty   begin depth while drop repeat ;

: ?   @ . ;

: ?dot         dup 32 127 within 0= if drop [char] . then ;
: dump-chars   dup 16 + swap do i c@ ?dot emit loop ;
: dump-cells   dup 16 + swap do i @ 11 u.r space cell +loop ;
: dump-line    dup u. space  dup dump-cells  space dump-chars cr ;
: dump         bounds do i dump-line 16 +loop ;

: c?      c@ . ;
: cdump   bounds do i c? loop cr ;

: id. ( xt -- )   >name ?dup if type else ." :noname@" 1- (.) then ;

: -rot   swap under swap ;

: execute-xt       nip swap dup >r execute r> swap dup ;
: traverse-order   context >r begin r@ @ ?dup while
                      1 swap ['] execute-xt traverse-wordlist
                      while cell r+
                   repeat then r> 2drop ;

: ?nt>end   2dup < if rot drop swap -1 else drop 0 then ;
: >end   here swap context @ ['] ?nt>end traverse-wordlist drop ;

: .nt     id. space -1 ;
: words   context @ ['] .nt traverse-wordlist ;

: body?      dup >body swap >end within ;
: .offset    2dup id. swap >body - ."  +" . ;
: ?.offset   2dup body? if .offset 0 else drop -1 then ;
:noname      return_stack 256 cells + rp@ do ."  > " i ?
             i @ context @ ['] ?.offset traverse-wordlist cr drop
             cell +loop ;
is backtrace

: xt??   nip over <> dup ;
: xt?    1 ['] xt?? traverse-order nip 0= ;

: disassemble ( x -- )
    dup xt? if
        dup c@ 127 > if ." ( postpone ) " then
        id.
    else
        .
    then ;

: .addr   ."     ( " u. ." ) " ;

: see-line ( addr -- )
    cr dup .addr  @ disassemble ;

: see-xt ( xt -- )
    dup >end swap
    >r ." : " r@ id.
    r@ >body do i see-line cell +loop
    ."  ;" r> c@ 127 > if ."  immediate" then ;

: see   ' see-xt cr ;

: #body   ' dup >end swap >body - ;

\ ----------------------------------------------------------------------

( Tools extension words. )

\ cs-pick

\ cs-roll

\ editor

: forget   ' dup >nextxt context @ >body !  dp !  reveal ;

: must-refill refill 0= abort" End of file when parsing word" ;
: next-word ( -- a u )
           begin parse-name dup 0= while 2drop must-refill repeat ;
: name<>   name= 0= ;
: >[then]  begin
              next-word 2dup s" [then]" name<>
           while
              s" [if]" name= if recurse then
           repeat 2drop ;
: [else]   begin
              next-word 2dup s" [then]" name<>
           while
              2dup s" [else]" name<>
           while
              s" [if]" name= if >[then] then
           repeat then 2drop ; immediate
: [if]     0= if postpone [else] then ; immediate
: [then]   ; immediate

\ ----------------------------------------------------------------------

( Forth2012 tools extension words. )

\ TODO: synonym

: [undefined]   parse-name find-name if drop 0 else 2drop -1 then ; immediate
: [defined]     postpone [undefined] 0= ; immediate

\ ----------------------------------------------------------------------

: (*   begin next-word s" *)" compare 0= until ;

: @+ ( addr -- a' x )   dup cell+ swap @ ;
: !+ ( x addr -- a' )   tuck ! cell+ ;

: (redefine-does>)   [ ' dodoes >code @ ] literal over >code !
                     r> swap >does ! ;
: redefine   tuck >body !  (redefine-does>) @ execute ;
: (redefi)   immediate redefine ;
finders redefine-xt   redefine redefine (redefi)
: re:        : latestxt dup count find-name redefine-xt ;

: xt-bounds ( xt -- end start)   dup >end swap >body ;
: .nt ( nt -- nt )   dup id. space ;
: xref-foo ( xt nt -- xt flag )
   dup xt-bounds do over i @ = if .nt leave then cell +loop drop 1 ;
: xref ( "name" -- )   ' context @ ['] xref-foo traverse-wordlist drop ;
