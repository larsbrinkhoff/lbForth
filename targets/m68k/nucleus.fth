\ -*- forth -*- Copyright 2013, 2015-2016 Lars Brinkhoff

\ Nucleus for Motorola 68000.

include targets/m68k/next.fth

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
   body-offset W )# I lea,
   next,
end-code

code dovar
   body-offset # W .l addq,
   W push,
   next,
end-code

code docon
   body-offset W )# W .l move,
   W push,
   next,
end-code

code dodef
   body-offset W )# W .l move,
   execute,
   next,
end-code

code dodoes
   body-offset # W .l addq,
   I rpush,
   S ) I .l move,
   2 # I .l addq,
   W S ) .l move,
   next,
end-code

code execute
   W pop,
   execute,
end-code

code r@
   R ) W .l move,
   W push,
   next,
end-code

code 0branch
   W fetch,
   S )+ .l tst,
   0=, if,
     W I .l move,
   then,
   next,
end-code

code branch
   I ) I .l move,
   next,
end-code

code (literal)
   I )+ S -) .l move,
   next,
end-code

code (sliteral)
   d0 fetch,
   I push,
   d0 push,
   I d0 .l add,
   3 # d0 .l addq,
   -4 # d0 .l andi,
   d0 I .l move,
   next,
end-code

code !
   W pop,
   S )+ W ) .l move,
   next,
end-code

code @
   S ) W .l move,
   W ) S ) .l move,
   next,
end-code

code +
   d0 pop,
   d0 S ) .l add,
   next,
end-code

code +!
   W pop,
   d0 pop,
   d0 W ) .l add,
   next,
end-code

code >r
   S )+ R -) .l move,
   next,
end-code

code r>
   R )+ S -) .l move,
   next,
end-code

code 2>r
   8 # R .l subq,
   S )+ R ) .l move,
   S )+ 4 R )# .l move,
   next,
end-code

code (loop)
   0 # d1 moveq,
   R ) d0 .l move,
   1 # d0 .l addq,
   4 R )# d0 .l cmp,
   0=, if,
     -1 # d1 .l move,
   then,
   d1 S -) .l move,
   d0 R ) .l move,
   next,
end-code

code 2rdrop
   8 # R .l addq,
   next,
end-code

code nand
   d0 pop,
   S ) d0 .l and,
   d0 .l not,
   d0 S ) .l move,
   next,
end-code

code c!
   W pop,
   d0 pop,
   d0 W ) .b move,
   next,
end-code

code c@
   S ) W .l move,
   0 # d0 moveq,
   W ) d0 .b move,
   d0 S ) .l move,
   next,
end-code

code h!
   W pop,
   d0 pop,
   d0 W ) .w move,
   next,
end-code

code r@
   R ) S -) .l move,
   next,
end-code

code drop
   4 # S .l addq,
   next,
end-code

code 2drop
   8 # S .l addq,
   next,
end-code

code nip
   d0 pop,
   d0 S ) .l move,
   next,
end-code

code dup
   S ) d0 .l move,
   d0 push,
   next,
end-code

code ?dup
   S ) d0 .l move,
   d0 .l tst,
   0<>, if,
     d0 push,
   then,
   next,
end-code

code 2dup
   4 S )# d0 .l move,
   d0 push,
   4 S )# d0 .l move,
   d0 push,
   next,
end-code

code swap
   S ) d0 .l move,
   4 S )# S ) .l move,
   d0 4 S )# .l move,
   next,
end-code

code over
   4 S )# d0 .l move,
   d0 push,
   next,
end-code

code tuck
   S ) d0 .l move,
   4 S )# d1 .l move,
   d1 S ) .l move,
   d0 4 S )# .l move,
   d0 push,
   next,
end-code

code rshift
   S )+ d0 .l move,
   S ) d1 .l move,
   d0 d1 .l lsr,
   d1 S ) .l move,
   next,
end-code

decimal
