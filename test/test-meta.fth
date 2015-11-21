: noop ;

create data_stack   110 cells allot
create return_stack   256 cells allot

variable x
: foo   70 x !  x @ emit ;

forward: bar
: warm   foo s" tjo" + 1- c@ emit foo bar bye ;
: bar   16 cells 1+ 2 or 1 xor emit 10 emit ;

code cold  also meta
   then,

   ' warm >body # I mov,
   ' data_stack >body 100 cells + # S mov,
   ' return_stack >body 256 cells + # R mov,

   next,
end-code
