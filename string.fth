\ -*- forth -*- Copyright 2004, 2013 Lars Brinkhoff

( String words. )

\ -trailing
\ /string
\ blank
\ in kernel: cmove

: cmove> ( addr1 addr2 n -- )
    swap over + >r
    swap over + swap
    begin
	?dup
    while
	1-
	r> 1- >r
	under 1-
	over c@ r@ !
    repeat r> 2drop ;

\ compare
\ search
\ sliteral
