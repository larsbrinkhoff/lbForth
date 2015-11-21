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

\ Next.
: fetch,   >r 2>r I ) 2r> r> mov,  4 # I add, ;
: next,   W fetch,  code-offset W )# indirect-jmp, ;

target

code exit
   R ) I mov,
   4 # R add,
   next,
end-code

code docol  also meta
   4 # R sub,
   I R ) mov,
   body-offset W )# I lea,
   next,
end-code

code dovar  also meta
   body-offset W )# W lea,
   W push,
   next,
end-code

code dodoes  also meta
   4 # R sub,
   I R ) mov,
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

code >r
   W pop,
   4 # R sub,
   W R ) mov,
   next,
end-code

code (sliteral)
   W fetch,
   I push,
   W push,
   W I add,
   4 # I add,
   -4 # I and,
   next,
end-code

code r>
   R ) W mov,
   4 # R add,
   W push,
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

code open-file ( addr u mode -- fileid ior )
   eax push,
   ebx push,
   ecx push,

   5 # eax mov,
   14 S )# ebx mov,
   0C S )# ecx mov,
   80 # int,

   ebx ebx xor,
   eax eax test,
   0<, if,
     eax ebx xchg,
   then,
   ebx 10 S )# mov,
   eax 14 S )# mov,

   ecx pop,
   ebx pop,
   eax pop,
   W pop,
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

   ebx ebx xor,
   eax eax test,
   0<, if,
     eax ebx xchg,
   then,
   ebx 10 S )# mov,
   eax 14 S )# mov,

   ecx pop,
   ebx pop,
   eax pop,
   W pop,
   next,
end-code

code branch
   I ) I mov,
   next,
end-code

code execute  also meta
   W pop,
   code-offset W )# indirect-jmp,
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
   W pop,
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
\ code 2drop
\ code 2dup
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
   W not,
   W W sbb,
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
