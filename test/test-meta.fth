: noop ;

create data_stack   110 cells allot
create return_stack   256 cells allot

variable x
: foo   66 x !  x @ emit ;

forward: bar
: warm   foo foo bar bye ;
: bar   16 cells 1+ 2 or 1 xor emit 10 emit ;

code cold
   then,

   ' warm >body # I mov,
   ' data_stack 100 cells + # S mov,
   ' return_stack 256 cells + # R mov,

   next,
end-code
