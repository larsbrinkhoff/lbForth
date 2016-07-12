include lib/elapsed.fth

: file   parse-name 2dup type space ;
: run   0elapsed s" main" evaluate .elapsed cr ;
: benchmark   file included run ;

benchmark contrib/fib.fth
benchmark contrib/bubble.fth
benchmark contrib/matrix.fth
benchmark contrib/siev.fth

bye

        sieve bubble matrix   fib   fft
gforth  0.116  0.148  0.060 0.108 0.048
lbForth 1.027  1.263  8.423 0.438
