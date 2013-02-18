\ -*- forth -*-

( Tools words. )

: .s ( -- )
    [char] < emit  depth (.)  ." > "
    'SP @ >r r@ depth 1- cells +
    begin
	dup r@ <>
    while
	dup @ .
	/cell -
    repeat r> 2drop ;

: ?   @ . ;

: c?   c@ . ;

: dump   over + swap do i ? 4 +loop cr ;

: cdump   over + swap do i c? loop cr ;

: see-find ( caddr -- end xt | 0 )
    >r here lastxt @
    begin
	dup
    while
	r@ over word= if r> drop exit then
	nip dup >nextxt
    repeat nip r> drop ;

: cabs ( char -- |char| )   dup 127 > if 256 swap - then ;

: xt. ( xt -- )
    ( >name ) count cabs type ;

: xt?   dup 134500000 > swap 134600000 < and ;

: disassemble ( x -- )
    dup xt? if
        ( >name ) count
        dup 127 > if ." postpone " then
        cabs type
    else
        .
    then ;

: see-line ( addr -- )
    cr ."     ( " dup . ." ) "  @ disassemble ;

: see-word ( end xt -- )
    >r ." : " r@ xt.
    r@ >body do i see-line /cell +loop
    ."  ;" r> c@ 127 > if ."  immediate" then ;

: see ( "word" -- )
    bl word see-find ?dup if see-word else ." Undefined word" then cr ;

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

: words ( -- )
    ['] type-word traverse-dictionary cr ;

\ ----------------------------------------------------------------------

( Tools extension words. )

\ ;code

: ahead ( C: -- orig )
    here 0 , ;

\ assembler

\ in kernel: bye

\ code

\ cs-pick

\ cs-roll

\ editor

: forget ( "word" -- )
    ' dup >nextxt lastxt !  'here ! ;

\ Kernel: state

\ [else]

\ [if]

\ [then]
