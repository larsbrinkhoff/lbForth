\ -*- forth -*- Copyright 2016 Lars Brinkhoff

\ Nucleus for PDP-11.

include targets/pdp11/next.fth

hex
target
exe-code

code sp@
  S push,
  next,
end-code

code sp!
  S pop,
  next,
end-code

code rp@
  R push,
  next,
end-code

code rp!
  R pop,
  next,
end-code

code exit
   I rpop,
   next,
end-code

code docol
   I rpush,
   W I mov,
   body-offset # I add,
   next,
end-code

code dovar
   body-offset # W add,
   W push,
   next,
end-code

code docon
   body-offset W )# push,
   next,
end-code

code dodef
   body-offset W )# W mov,
   execute,
end-code

code dodoes
   body-offset # W add,
   I rpush,
   r0 I mov,
   W S ) mov,
   next,
end-code

code execute
   W pop,
   execute,
end-code

code r@
   R ) S -) mov,
   next,
end-code

code 0branch
   W fetch,
   S )+ tst,
   0=, if,
     W I mov,
   then,
   next,
end-code

code branch
   I ) I mov,
   next,
end-code

code (literal)
   I )+ S -) mov,
   next,
end-code

code (sliteral)
   W fetch,
   I push,
   W push,
   W I add,
   1 # I add,
   1 # I bic,
   next,
end-code

code !
   W pop,
   S )+ W ) mov,
   next,
end-code

code @
   S ) W mov,
   W ) S ) mov,
   next,
end-code

code +
   W pop,
   W S ) add,
   next,
end-code

code +!
   W pop,
   T pop,
   T W ) add,
   next,
end-code

code >r
   S )+ R -) mov,
   next,
end-code

code r>
   R )+ S -) mov,
   next,
end-code

code 2>r
   4 # R sub,
   S )+ R ) mov,
   S )+ 2 R )# mov,
   next,
end-code

code (loop)
   r1 clr,
   R ) r0 mov,
   1 # r0 add,
   2 R )# r0 cmp,
   0=, if,
     -1 # r1 mov,
   then,
   r1 S -) mov,
   r0 R ) mov,
   next,
end-code

code 2rdrop
   4 # R add,
   next,
end-code

code nand
   W pop,
   W com,
   W S ) bic,
   S ) com,
   next,
end-code

code c!
   W pop,
   T pop,
   T W ) movb,
   next,
end-code

code c@
   S ) W mov,
   W ) W movb,
   ff00 # W bic,
   W S ) mov,
   next,
end-code

code r@
   R ) S -) mov,
   next,
end-code

code drop
   2 # S add,
   next,
end-code

code 2drop
   4 # S add,
   next,
end-code

code nip
   W pop,
   W S ) mov,
   next,
end-code

code dup
   S ) W mov,
   W push,
   next,
end-code

code ?dup
   S ) W mov,
   0<>, if,
     W push,
   then,
   next,
end-code

code 2dup
   2 S )# W mov,
   W push,
   2 S )# W mov,
   W push,
   next,
end-code

code swap
   S ) W mov,
   2 S )# S ) mov,
   W 2 S )# mov,
   next,
end-code

code over
   2 S )# W mov,
   W push,
   next,
end-code

code tuck
   S ) W mov,
   2 S )# T mov,
   T S ) mov,
   W 2 S )# mov,
   W push,
   next,
end-code

decimal
