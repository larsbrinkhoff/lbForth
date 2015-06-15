\ Solution to Project Euler problem 1.

variable sum
: multiple?  mod 0= ;
: sum?   dup 3 multiple? swap 5 multiple? or ;
: iterations   1 do i sum? if i sum +! then loop ;
: answer   sum off   1000 iterations  sum ? ;
