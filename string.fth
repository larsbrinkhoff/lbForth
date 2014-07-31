\ -*- forth -*- Copyright 2004, 2013-2014 Lars Brinkhoff

( String words. )

: -trailing   begin dup while 2dup + 1- c@ bl = while 1- repeat then ;

: blank   bl fill ;

: cmove> ( addr1 addr2 n -- )
    tuck + >r
    dup under +
    begin
	?dup
    while
	1-
	r> 1- >r
	under 1-
	over c@ r@ c!
    repeat r> 2drop ;

: move   >r 2dup u< if r> cmove> else r> cmove then ;

: sliteral   postpone (sliteral) s, ; compile-only

: /string ( caddr u n -- caddr+1 u-1 )   swap over - under + ;

: compare ( a1 u1 a2 u2 -- n )
    bounds ?do
       dup 0= if 2drop -1 unloop exit then
       over c@ i c@ - dup 0<>
       if 0< 2* 1+ under 2drop unloop exit else drop then
       1 /string
    loop
    nip 0> negate ;

: search   2>r begin 2dup r@ min 2r@ compare while dup while
   1 /string repeat 0 else -1 then 2r> 2drop ;
