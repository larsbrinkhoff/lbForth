\ -*- forth -*- Copyright 2004, 2013-2016 Lars Brinkhoff

( Tools words. )

: .s   [char] < emit  depth (.)  ." > "
       sp@ cell - >r r@ depth 1- cells +
       begin dup r@ <> while
         dup @ . cell -
       repeat r> 2drop ;

: ?   @ . ;

8 cells 2 + 3 / constant #digits
: ?dot         dup 32 127 within 0= if drop [char] . then ;
: dump-chars   dup 16 + swap do i c@ ?dot emit loop ;
: dump-cells   dup 16 + swap do i @ #digits u.r space cell +loop ;
: dump-line    dup u. space  dup dump-cells  space dump-chars cr ;
: dump         bounds do i dump-line 16 +loop ;

: c?      c@ 0 <# # #s #> type space ;
: cdump   bounds do i c? loop cr ;

: id. ( xt -- )   >name ?dup if type else ." :noname@" 1- (.) then space ;

: -rot   swap under swap ;

: execute-xt       nip swap dup >r execute r> swap dup ;
: traverse-order   context >r begin r@ @ ?dup while
                      1 swap ['] execute-xt traverse-wordlist
                      while cell r+
                   repeat then r> 2drop ;

: ?nt>end   2dup < if rot drop swap -1 else drop 0 then ;
: >end   here swap context @ ['] ?nt>end traverse-wordlist drop ;

: .nt     id. -1 ;
: words   context @ ['] .nt traverse-wordlist ;

: body?      dup >body swap >end within ;
: .offset    2dup id. swap >body - ." +" . ;
: ?.offset   2dup body? if .offset 0 else drop -1 then ;
: .'         context @ ['] ?.offset traverse-wordlist drop ;
: .bt        ."  > " dup ? @ .' cr ;
:noname      rp0 rp@ do i .bt cell +loop ;
is backtrace

: xt?? ( xt x nt -- xt f f )   nip over <> dup ;
: xt? ( xt -- f )              1 ['] xt?? traverse-order nip 0= ;
: wid-xt? ( xt wid -- f )      1 swap ['] xt?? traverse-wordlist nip 0= ;

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

: see' ( xt -- )
    dup >end swap
    >r ." : " r@ id.
    r@ >body do i see-line cell +loop
    ."  ;" r> c@ 127 > if ."  immediate" then ;

: see   ' see' cr ;

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

: c-o ( xt -- )   alias compile-only ;
finders ]alias  drop undef c-o
: com ( a u a u "name" -- )   2drop ] ]alias ;
: def ( a u xt "name" -- )   >in @ swap alias >in !  2dup com ;
: imm ( a u xt "name" -- )   alias immediate 2drop ;
finders [alias  def com imm
: [find ( a u "name" -- )   2dup postpone [ [alias ;
: next-name ( -- u1 a u2 )   >in @ parse-name 2drop parse-name 2>r
   >in @ swap >in ! 2r> ;
: state! ( flag -- )   if ] else postpone [ then ;
: synonym ( "name1" "name2" -- )   state @ next-name [find >in ! state! ;

: [undefined]   parse-name find-name if drop 0 else 2drop -1 then ; immediate
: [defined]     postpone [undefined] 0= ; immediate

\ ----------------------------------------------------------------------

: (*   1 begin next-word 2dup 2>r s" (*" compare 0= -
   2r> s" *)" compare 0= +  dup 0= until drop ; immediate

: !+ ( x addr -- a' )   tuck ! cell+ ;
