\ Nucleus for x86.  Copyright 2014-2015 Lars Brinkhoff.

hex

\ Jump to COLD.
also assembler
ahead,

host also meta also assembler

\ Register allocation.
: I   eax ;
: S   esp ;
: R   esi ;
: W   edx ;

\ Macros.
: op>r     postpone >r postpone 2>r ; compile-only
: opr>     postpone 2r> postpone r> ; compile-only
: fetch,   op>r I ) opr> mov,  4 # I add, ;
: rpop,    op>r R ) opr> mov,  4 # R add, ;
: rpush,   4 # R sub,  R ) mov, ;
: execute, code-offset W )# indirect-jmp, ;

\ Next.
: next,   W fetch,  execute, ;

\ Check eax for error.  Store ( result error ) at stack offset u.
: error-check, ( u -- )
   >r
   ebx ebx xor,
   eax eax test,
   0<, if,
     eax ebx xchg,
   then,
   ebx r@ S )# mov,
   eax r> 4 + S )# mov, ;

target

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
   body-offset W )# W lea,
   W push,
   next,
end-code

code docon
   body-offset W )# W mov,
   W push,
   next,
end-code

code dodef
   body-offset W )# W mov,
   execute,
   next,
end-code

code dodoes
   I rpush,
   does-offset W )# I mov,
   body-offset W )# W lea,
   W push,
   next,
end-code

code 0branch
   W fetch,
   ecx pop,
   ecx ecx test,
   W I cmove,
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
   W I add,
   3 # I add,
   -4 # I and,
   next,
end-code

code !
   ecx pop,
   W pop,
   W ecx ) mov,
   next,
end-code

code @
   S ) W mov,
   W ) W mov,
   W S ) mov,
   next,
end-code

code +
   W pop,
   W S ) add,
   next,
end-code

code +!
   W pop,
   ecx pop,
   ecx W ) add,
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
   W pop,
   8 # R sub,
   W R ) mov,
   W pop,
   W 4 R )# mov,
   next,
end-code

code (loop)
   1 # W mov,
   R ) W add,
   4 R )# W cmp,
   ecx ecx sbb,
   ecx not,
   ecx push,
   W R ) mov,
   next,
end-code

code 2rdrop
   8 # R add,
   next,
end-code

code nand
   W pop,
   S ) W and,
   W not,
   W S ) mov,
   next,
end-code

code c!
   W pop,
   ecx pop,
   cl W ) mov,
   next,
end-code

code c@
   S ) W mov,
   W ) W movzx,
   W S ) mov,
   next,
end-code

code emit
   S W mov,
   eax push,
   ebx push,
   ecx push,

   4 # eax mov,
   1 # ebx mov,
   W ecx mov,
   1 # edx mov,
   80 # int,

   ecx pop,
   ebx pop,
   eax pop,
   4 # S add,
   next,
end-code

code bye ( ... -- <no return> )
   1 # eax mov,
   0 # ebx mov,
   80 # int,
end-code

also assembler
   label tmp
   100 allot
previous

code open-file ( addr u mode -- fileid ior )
   eax push,
   ebx push,
   ecx push,

   esi push,
   18 S )# esi mov,
   tmp edi lea,
   14 S )# ecx mov,
   rep, movsb,
   cl edi ) mov,
   esi pop,

   5 # eax mov,
   tmp ebx lea,
   0C S )# ecx mov,
   octal 644 hex # edx mov,
   80 # int,
   10 error-check,

   ecx pop,
   ebx pop,
   eax pop,
   4 # esp add,
   next,
end-code

code read-file ( addr u1 fileid -- u2 ior )
   eax push,
   ebx push,
   ecx push,

   3 # eax mov,
   0C S )# ebx mov,
   14 S )# ecx mov,
   10 S )# edx mov,
   80 # int,
   10 error-check,

   ecx pop,
   ebx pop,
   eax pop,
   W pop,
   next,
end-code

code write-file ( addr u1 fileid -- u2 ior )
   eax push,
   ebx push,
   ecx push,

   4 # eax mov,
   0C S )# ebx mov,
   14 S )# ecx mov,
   10 S )# edx mov,
   80 # int,
   10 error-check,

   ecx pop,
   ebx pop,
   eax pop,
   W pop,
   next,
end-code

code close-file ( fileid -- ior )
   eax push,
   ebx push,

   6 # eax mov,
   8 S )# ebx mov,
   80 # int,
   eax 8 S )# mov,

   ebx pop,
   eax pop,
   next,
end-code

code branch
   I ) I mov,
   next,
end-code

code execute
   W pop,
   execute,
end-code

code r@
   R ) W mov,
   W push,
   next,
end-code

code 0=
   1 # S ) cmp,
   W W sbb,
   W S ) mov,
   next,
end-code

code 0<>
   S ) W mov,
   W neg,
   W W sbb,
   W S ) mov,
   next,
end-code

code 0<
   S ) W mov,
   1F # W sar,
   W S ) mov,
   next,
end-code

code drop
   4 # S add,
   next,
end-code

code dup
   S ) W mov,
   W push,
   next,
end-code

code ?dup
   S ) W mov,
   W W test,
   0<>, if,
     W push,
   then,
   next,
end-code

code nip
   W pop,
   W S ) mov,
   next,
end-code

code swap
   S ) W mov,
   4 S )# ecx mov,
   ecx S ) mov,
   W 4 S )# mov,
   next,
end-code

code over
   4 S )# W mov,
   W push,
   next,
end-code

\ code tuck ( x1 x2 -- x2 x1 x2 )
\ code rot
\ code -rot

code 2drop
   8 # S add,
   next,
end-code

code 2dup
   4 S )# W mov,
   W push,
   4 S )# W mov,
   W push,
   next,
end-code

\ code 2nip
\ code 2swap
\ code 2over
\ code 2tuck
\ code 2rot

code negate
   S ) neg,
   next,
end-code

code -
   W pop,
   W S ) sub,
   next,
end-code

code =
   W pop,
   S ) W sub,
   1 # W sub,
   W W sbb,
   W S ) mov,
   next,
end-code

code <>
   W pop,
   S ) W sub,
   1 # W sub,
   W W sbb,
   W not,
   W S ) mov,
   next,
end-code

code 1+
   1 # S ) add,
   next,
end-code

code 1-
   -1 # S ) add,
   next,
end-code

code cell+
   4 # S ) add,
   next,
end-code

code cells
   S ) W mov,
   2 # W shl,
   W S ) mov,
   next,
end-code

\ code >
\ code <
\ code <=
\ code >=
\ code abs
\ code max
\ code min
\ code *
\ code /
\ code mod

code invert
   S ) not,
   next,
end-code

code or
   W pop,
   W S ) or,
   next,
end-code

code xor
   W pop,
   W S ) xor,
   next,
end-code

code and
   W pop,
   W S ) and,
   next,
end-code

decimal
