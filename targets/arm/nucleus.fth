\ -*- forth -*- Copyright 2016 Lars Brinkhoff

\ Nucleus for ARM.

include targets/arm/next.fth

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
   body-offset # W I add,
   next,
end-code

code dovar
   body-offset # W W add,
   W push,
   next,
end-code

code docon
   body-offset W )# W ldr,
   W push,
   next,
end-code

code dodef
   body-offset W )# W ldr,
   execute,
   next,
end-code

code dodoes
   body-offset # W W add,
   W push,
   I rpush,
   lr I mov,
   next,
end-code

code execute
   W pop,
   execute,
end-code

code r@
   R ) W ldr,
   W push,
   next,
end-code

code 0branch
   W fetch,
   T pop,
   T T tst,
   W I eq mov,
   next,
end-code

code branch
   I ) I ldr,
   next,
end-code

code (literal)
   W fetch,
   W push,
   next,
end-code

code (sliteral)
   W fetch,
   I push,
   W push,
   W I I add,
   3 # I I add,
   3 # I I bic,
   next,
end-code

code !
   W pop,
   T pop,
   W ) T str,
   next,
end-code

code @
   S ) W ldr,
   W ) W ldr,
   S ) W str,
   next,
end-code

code +
   W pop,
   S ) T ldr,
   T W W add,
   S ) W str,
   next,
end-code

code +!
   W pop,
   T pop,
   W ) r12 ldr,
   r12 T T add,
   W ) T str,
   next,
end-code

code >r
   W pop,
   W rpush,
   next,
end-code

code r>
   W rpop,
   W push,
   next,
end-code

code 2>r
   8 # R R sub,
   W pop,
   R ) W str,
   W pop,
   4 R )# W str,
   next,
end-code

code (loop)
   R ) W ldr,
   1 # W W add,
   4 R )# T ldr,
   T W cmp,
   0 # T ne mov,
   0 # T eq mvn,
   T push,
   R ) W str,
   next,
end-code

code 2rdrop
   8 # R R add,
   next,
end-code

code and
   W pop,
   S ) T ldr,
   T W W and,
   S ) W str,
   next,
end-code

code nand
   W pop,
   S ) T ldr,
   T W W and,
   W W mvn,
   S ) W str,
   next,
end-code

code c!
   W pop,
   T pop,
   W ) T strb,
   next,
end-code

code c@
   S ) W ldr,
   W ) W ldrb,
   S ) W str,
   next,
end-code

code drop
   4 # S S add,
   next,
end-code

code 2drop
   8 # S S add,
   next,
end-code

code nip
   W pop,
   S ) W str,
   next,
end-code

code dup
   S ) W ldr,
   W push,
   next,
end-code

code ?dup
   S ) W ldr,
   W W tst,
   -4 S )#! W ne str,
   next,
end-code

code 2dup
   4 S )# W ldr,
   W push,
   4 S )# W ldr,
   W push,
   next,
end-code

code swap
   S ) W ldr,
   4 S )# T ldr,
   S ) T str,
   4 S )# W str,
   next,
end-code

code over
   4 S )# W ldr,
   W push,
   next,
end-code

code tuck
   S ) W ldr,
   4 S )# T ldr,
   S ) T str,
   4 S )# W str,
   W push,
   next,
end-code

code rshift
   W pop,
   S ) T ldr,
   T W lsr T mov,
   S ) T str,
   next,
end-code

decimal
