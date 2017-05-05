\ -*- forth -*- Copyright 2017 Lars Brinkhoff

\ Nucleus for RISC-V.

include targets/riscv/next.fth

hex
target
exe-code

code sp@
  S T mv,
  T push,
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
   body-offset # W I addi,
   next,
end-code

code dovar
   body-offset # W W addi,
   W push,
   next,
end-code

code docon
   body-offset W )# W lw,
   W push,
   next,
end-code

code dodef
   body-offset W )# W lw,
   execute,
   next,
end-code

code dodoes
   body-offset # W W addi,
   W push,
   I rpush,
   x1 I mv,
   next,
end-code

code execute
   W pop,
   execute,
end-code

code r@
   R ) W lw,
   W push,
   next,
end-code

code 0branch
   W fetch,
   T pop,
   T 0=, if,
     W I mv,
   then,
   next,
end-code

code branch
   I ) I lw,
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
   3 # I I addi,
   3 invert # I I andi,
   next,
end-code

code !
   W pop,
   T pop,
   W ) T sw,
   next,
end-code

code @
   S ) W lw,
   W ) W lw,
   S ) W sw,
   next,
end-code

code +
   W pop,
   S ) T lw,
   T W W add,
   S ) W sw,
   next,
end-code

code +!
   W pop,
   T pop,
   W ) x7 lw,
   x7 T T add,
   W ) T sw,
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
   -8 # R R addi,
   W pop,
   R ) W sw,
   W pop,
   4 R )# W sw,
   next,
end-code

code (loop)
   0 # x7 li,
   R ) W lw,
   1 # W W addi,
   4 R )# T lw,
   T W =, if,
     -1 # x7 x7 addi,
   then,
   x7 push,
   R ) W sw,
   next,
end-code

code 2rdrop
   8 # R R addi,
   next,
end-code

code and
   W pop,
   S ) T lw,
   T W W and,
   S ) W sw,
   next,
end-code

code nand
   W pop,
   S ) T lw,
   T W W and,
   W W not,
   S ) W sw,
   next,
end-code

code c!
   W pop,
   T pop,
   W ) T sb,
   next,
end-code

code c@
   S ) W lw,
   W ) W lbu,
   S ) W sw,
   next,
end-code

code drop
   4 # S S addi,
   next,
end-code

code 2drop
   8 # S S addi,
   next,
end-code

code nip
   W pop,
   S ) W sw,
   next,
end-code

code dup
   S ) W lw,
   W push,
   next,
end-code

code ?dup
   S ) W lw,
   W 0<>, if,
     W push,
   then,
   next,
end-code

code 2dup
   4 S )# W lw,
   W push,
   4 S )# W lw,
   W push,
   next,
end-code

code swap
   S ) W lw,
   4 S )# T lw,
   S ) T sw,
   4 S )# W sw,
   next,
end-code

code over
   4 S )# W lw,
   W push,
   next,
end-code

code tuck
   S ) W lw,
   4 S )# T lw,
   S ) T sw,
   4 S )# W sw,
   W push,
   next,
end-code

code lshift
   W pop,
   S ) T lw,
   W T T sll,
   S ) T sw,
   next,
end-code

decimal
