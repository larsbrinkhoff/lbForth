\ -*- forth -*- Copyright 2004, 2013 Lars Brinkhoff

( Tools words. )

: .s   [char] < emit  depth (.)  ." > "
       sp@ /cell - >r r@ depth 1- cells +
       begin dup r@ <> while
         dup @ . /cell -
       repeat r> 2drop ;

: .s"   postpone s" postpone type postpone .s postpone cr ; immediate

: empty   begin depth while drop repeat ;

: ?   @ . ;

: c?   c@ . ;

: dump   bounds do i ? /cell +loop cr ;

: cdump   bounds do i c? loop cr ;

: .xt ( xt -- )   >name ?dup if type else ." :noname@" 1- (.) then ;

: -rot   swap under swap ;

: execute-xt   -rot 2>r r@ execute 2r> rot if nip -1 swap then over ;
: traverse-order   context begin dup @ ?dup while swap >r
                   0 -rot ['] execute-xt traverse-wordlist
                   r> rot if cell+ else 2drop exit then
         	   repeat 2drop ;

: ?nt>end   2dup < if rot drop swap -1 else drop 0 then ;
: >end   here swap context @ ['] ?nt>end traverse-wordlist drop ;

: .nt   .xt space -1 ;
: words   context @ ['] .nt traverse-wordlist ;

: body?   dup >body swap >end within ;
: .offset   dup .xt ."  +" >body - . ;
: ?.offset   2dup body? if .offset 0 else drop -1 then ;
: backtrace   return_stack 100 cells + rp@ do ."  > " i ?
              i @ context @ ['] ?.offset traverse-wordlist cr drop
              /cell +loop ;

: xt??   rot drop over <> tuck ;
: xt?    0 swap context @ ['] xt?? traverse-wordlist drop 0= ;

: disassemble ( x -- )
    dup xt? if
        dup c@ 127 > if ." postpone " then
        .xt
    else
        .
    then ;

: .addr  dup . ;

: see-line ( addr -- )
    cr ."     ( " .addr ." ) "  @ disassemble ;

: see-word ( end xt -- )
    >r ." : " r@ .xt
    r@ >body do i see-line /cell +loop
    ."  ;" r> c@ 127 > if ."  immediate" then ;

: see   ' dup >end swap see-word cr ;

: #body   ' dup >end swap >body - ;

\ ----------------------------------------------------------------------

( Tools extension words. )

\ ;code

\ assembler

\ in kernel: bye

\ code

\ cs-pick

\ cs-roll

\ editor

: forget   ' dup >nextxt context @ >body !  'here !  reveal ;

\ [else]

\ [if]

\ [then]

\ ----------------------------------------------------------------------

( Forth2012 tools extension words. )

: n>r ( x1 ... xn n -- ) ( R: -- x1 ... xn n )
   r> over >r swap begin ?dup while
     rot r> 2>r 1-
   repeat >r ;

: nr> ( -- x1 ... xn n ) ( R: x1 ... xn n -- )
   r> r@ begin ?dup while
      2r> >r -rot 1-
   repeat r> swap >r ;

\ TODO: synonym

: [undefined]   bl-word find nip 0= ; immediate

: [defined]   postpone [undefined] invert ; immediate

\ ----------------------------------------------------------------------

: @+ ( addr -- addr+/cell x )   dup cell+ swap @ ;
: !+ ( x addr -- addr+/cell )   tuck ! cell+ ;

: (redefine-does>)   [ ' dodoes >code @ ] literal over >code !
		     r> swap >does ! ;
: redefine   tuck >body !  (redefine-does>) @ execute ;
: (redefi)   immediate redefine ;
finders redefine-xt   redefine redefine (redefi)
: :redefine   : lastxt @ dup find redefine-xt ;
