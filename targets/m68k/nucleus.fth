\ -*- forth -*- Copyright 2013, 2015 Lars Brinkhoff

(*
   Nucleus for the M68000.

   Direct threaded.
*)

16 base !

\ Registers
%a0 constant T  \ Haven't checked whether data or address
%a1 constant W  \ register would be appropriate.
%a4 constant DP
%a5 constant IP
%a6 constant SP
%a7 constant RP

: next,   IP )+ jmp, ;

code cold
   sp0 SP movei,
   rp0 RP movei,
   dp0 dp movei,
   2 +. IP movei,
   next,
   warm ,
end-code

\ Code field for : is "enter jsr,"
code enter
   RP ) W move,
   IP RP ) move,
   W IP move,
   next,
end-code

code exit
   RP )+ IP move,
   next,
end-code

\ Code field for CREATE/VARIABLE is "dovar jsr,"
code dovar
   T SP -) move,
   RP )+ T move,
   next,
end-code

\ Code field for DOES> child is "parent jsr," (data)
\ Code after DOES> is "dodoes jsr," (code)
code dodoes
   RP )+ W move,
   RP ) T move,
   IP RP ) move,
   W IP move,
   next,
end-code

\ HEADER  "constant"
\ CODE    enter jsr,
\ XT      create
\ XT      ,
\ XT      (does>)
\ CODE    dodoes jsr,  <- x
\ XT      @
\ XT      exit

\ HEADER  "fourtytwo"
\ CODE    x jsr,
\ DATA    42

: does,     dodoes jsr, ;
: does!     jsr, ;
: (does>)   latestxt >code >h  r> does!  h> ;

\ Code field for CONSTANT is "docon jsr,"
code docon
   T SP -) move,
   RP )+ T move,
   T ) T move,
   next,
end-code

\ Code field for DEFER is "0 jsr,"

code execute
   T W move,
   SP -) T move,
   W ) jmp,
end-code

code 0branch
   T tst,
   6 +. beq.s,
   IP 4 # addq,
   2 +. bra.s,
   IP ) IP move,
   T SP )+ move,
   next,
end-code

code branch
   IP ) IP move,
   next,
end-code

code (literal)
   T SP -) move,
   IP ) T move,
   4 # IP addq,
   next,
end-code

code !
   W SP )+ move,
   T ) W move,
   T SP )+ move,
   next,
end-code

code @
   T ) T move,
   next,
end-code

code +
   T SP )+ add,
   next,
end-code

code >r
   T RP -) move,
   SP )+ T move,
   next,
end-code

code r>
   T SP -) move,
   RP )+ T move,
   next,
end-code

code nand
   SP )+ W move,
   W T and,
   T not,
   next,
end-code

code c!
   W SP )+ move,
   T ) W move.b,
   T SP )+ move,
end-code

code c@
   T ) T move.b,
end-code

code bye
   . jmp,
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
   SP )+ T move,
   next,
end-code

code 2drop
   SP )1 T move,
   SP 8 # addq,
   next,
end-code

code nip
   SP 4 # addq,
   next,
end-code

code dup
   T SP -) move,
   next,
end-code

code ?dup
   T tst,
   2 +. beq,
   T SP -) move,
   next,
end-code

code 2dup
   T SP -) move,
   SP )4 W move,
   W SP -) move,
   next,
end-code

code swap
   SP ) W move,
   T SP ) move,
   W T move,
   next,
end-code

code over
   T SP -) move,
   SP )4 T move,
   next,
end-code

code tuck
   SP ) SP -) move,
   T SP )4 move,
   next,
end-code

code rot
   SP ) W move,
   SP )4 SP ) move,
   T SP )4 move,
   W T move,
   next,
end-code
