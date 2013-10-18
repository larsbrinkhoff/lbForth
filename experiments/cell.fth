\ Apply xt on u1, and repeat on the result as long as it
\ keeps getting larger.  Return the next last result.
: maximise ( u1 xt -- u2 )
   >r 0 swap begin 2dup u< while
      nip dup  r@ execute
   repeat r> 2drop ;

\ Assume there is no :noname, so define these separately.
: 2*1+   2* 1+ ;
variable temp
: c1+ ( c -- c+1 )   1+ temp c!  temp c@ ;

\ Compute the maximum unsigned value for a cell.
: ucell-max ( -- u )
   \ First, fill with binary ones.
   1 ['] 2*1+ maximise
   \ Second, make it work on grisly non-binary machines.
   ['] 1+ maximise ;

\ Compute the maximum (unsigned?) value for a cell.
: char-max ( -- u )   1 ['] c1+ maximise ;

\ Assume there is no um/mod, so simulate plain single-word division.
: u/ ( u1 u2 -- u1/u2 )   u/mod nip ;

\ Start with ucell-max, and repeatedly divide by char-max+1.
: cell ( -- u )
   \ Doesn't work when cell-max = ucell-max, so check for that explicitly.
   char-max ucell-max = if 1 exit then
   char-max 1+ >r
   1 ucell-max begin dup r@ u> while
      r@ u/  swap 1+ swap
   repeat r> 2drop ;
