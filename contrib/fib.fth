\ Note: This is incorrect ("n fib" produces the result for fib(n+1)),
\ but we do not change it to ensure that future timing results are
\ comparable to older timing results.

: fib ( n1 -- n2 )
    dup 2 < if
	drop 1
    else
	dup
	1- recurse
	swap 2 - recurse
	+
    then ;

: main 34 fib drop ;

\ The problem with fib is that it really is O(1)...
0 [IF]
    5e fsqrt fdup 1/f fconstant /sqrt5
    1e f+ f2/ fln     fconstant gbase
    : fib ( n -- f )
	dup s>f gbase f* fdup fexp fswap fnegate fexp
	1 and IF f+ ELSE f- THEN  /sqrt5 f* ;
[THEN]