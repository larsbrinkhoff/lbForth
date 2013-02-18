\ -*- forth -*- Copyright 2004, 2013 Lars Brinkhoff

( Tools words. )

: .s   [char] < emit  depth (.)  ." > "
       sp@ /cell - >r r@ depth 1- cells +
       begin dup r@ <> while
         dup @ . /cell -
       repeat r> 2drop ;

: empty   begin depth while drop repeat ;

: ?   @ . ;

: c?   c@ . ;

: dump   bounds do i ? /cell +loop cr ;

: cdump   bounds do i c? loop cr ;

: see-find ( caddr -- end xt )
    >r here lastxt @
    begin
	dup 0= abort" Undefined word"
	r@ over word= if r> drop exit then
	nip dup >nextxt
    again ;

: xt. ( xt -- )
    ( >name ) count cabs type ;

: xt? ( xt -- flag )
    >r lastxt @ begin
	?dup
    while
	dup r@ = if r> 2drop -1 exit then
	>nextxt
    repeat r> drop 0 ;

: disassemble ( x -- )
    dup xt? if
        ( >name ) count
        dup 127 > if ." postpone " then
        cabs type
    else
        .
    then ;

: .addr  dup . ;

: see-line ( addr -- )
    cr ."     ( " .addr ." ) "  @ disassemble ;

: see-word ( end xt -- )
    >r ." : " r@ xt.
    r@ >body do i see-line /cell +loop
    ."  ;" r> c@ 127 > if ."  immediate" then ;

: see   bl word see-find see-word cr ;

: #body   bl word see-find >body - ;

: type-word ( end xt -- flag )
    xt. space drop 0 ;

: traverse-dictionary ( in.. xt -- out.. )
    \ xt execution: ( in.. end xt2 -- in.. 0 | in.. end xt2 -- out.. true )
    >r  here lastxt @  begin
	?dup
    while
	r> 2dup >r >r execute
	if r> r> 2drop exit then
	r> dup >nextxt
    repeat r> 2drop ;

\ traverse-wordlist ( x*i wid xt -- x'*i )
\   xt execution: ( x*i nt -- x'*i flag[0=stop] )

: words ( -- )
    ['] type-word traverse-dictionary cr ;

: find-xt   begin dup xt? 0= while /cell - repeat ;

: -rot   swap under swap ;

: in-body?   rot 2dup 2>r -rot >body swap within 2r> rot
  	     if dup rot dup xt. ."  +" >body - . -1 else nip 0 then ;

: backtrace   return_stack 100 cells + rp@ do ."  > " i ?
              i @ ['] in-body? traverse-dictionary cr drop
              /cell +loop ;

\ ----------------------------------------------------------------------

( Tools extension words. )

\ ;code

\ assembler

\ in kernel: bye

\ code

\ cs-pick

\ cs-roll

\ editor

: forget   ' dup >nextxt lastxt !  'here !  reveal ;

\ Kernel: state

\ [else]

\ [if]

\ [then]

\ ----------------------------------------------------------------------

( Forth2012 tools extension words. )

\ TODO: n>r

\ TODO: nr>

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
