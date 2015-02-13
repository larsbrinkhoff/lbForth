\ Solution to Project Euler problem 2.

variable sum
: tally   dup sum +! ;
: end?   dup 4000000 > ;
: add   tuck + ;
: fib   begin tally add add add end? until ;
: answer   sum off  1 2 fib 2drop  sum ? ;
