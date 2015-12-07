\ -*- forth -*- Copyright 2013, 2015 Lars Brinkhoff

(*
   Nucleus for the PDP-10.

   Direct threaded.  Does not use extended addressing.
*)

8 base !

\ Registers
1 constant T
2 constant W
15 constant IP
16 constant SP
17 constant RP

: next,   IP ) IP aoja, ;

code cold
   sp0 SP movei,
   rp0 RP movei,
   dp0 dp movei,
   warm ] IP movei,
   next,
end-code

\ Code field for : is "enter RP pushj,"
code enter
   RP ) IP exch,
   next,
end-code

code exit
   IP RP pop,
   next,
end-code

\ Code field for CREATE/VARIABLE is "dovar SP pushj,"
code dovar
   SP ) T exch,
   next,
end-code

\ Code field for DOES> child is "parent SP pushj,"
\ Code after DOES> is "dodoes RP pushj,"
code dodoes
   RP ) IP exch,
   SP ) T exch,
   next,
end-code

\ HEADER  "constant"
\ CODE    enter RP pushj,
\ XT      create
\ XT      ,
\ XT      (does>)
\ CODE    dodoes RP pushj,  <- x
\ XT      @
\ XT      exit

\ HEADER  "fourtytwo"
\ CODE    x SP pushj,
\ DATA    42

: does,     dodoes RP pushj, ;
: does!     SP pushj, ;
: (does>)   latestxt >code >h  r> does!  h> ;

\ Code field for CONSTANT is "docon W jsp,"
code docon
   T SP push,
   W ) T move,
   next,
end-code

\ Code field for DEFER is "1 +. @ jrst,"

code execute
   T W move,
   T SP pop,
   W ) jsrt,
end-code

code 0branch
   0 T caie,
    IP aosa,
     IP ) IP move,
   T SP pop,
   next,
end-code

code branch
   IP ) IP move,
   next,
end-code

code (literal)
   T SP push,
   IP ) T move,
   1 IP addi,
   next,
end-code

code !
   T ) SP pop,
   T SP pop,
   next,
end-code

code @
   T ) T move,
   next,
end-code

code +
   W SP pop,
   W T add,
   next,
end-code

code >r
   T RP push,
   T SP pop,
   next,
end-code

code r>
   T SP push,
   T RP pop,
   next,
end-code

code nand
   W SP pop,
   W T orcb,
   next,
end-code

code c!
   \ same as !
end-code

code c@
   \ same as @
end-code

code bye
   halt,
   next,
end-code

code emit
end-code

code close-file
end-code

code open-file
end-code

code read-file
end-code

code drop
   T SP pop,
   next,
end-code

code 2drop
   SP )1 T move,
   2 SP subi,
   next,
end-code

code nip
   1 SP addi,
   next,
end-code

code dup
   T SP push,
   next,
end-code

code ?dup
   0 T caie,
    T SP push,
   next,
end-code

code 2dup
   T SP push,
   SP )1 W move,
   W SP push,
   next,
end-code

code swap
   SP ) T exch,
   next,
end-code

code over
   T SP push,
   SP )1 T move,
   next,
end-code

code tuck
   SP ) T exch,
   T SP push,
   SP )1 T move,
   next,
end-code

code rot
   SP ) T exch,
   SP )1 T exch,
   next,
end-code
