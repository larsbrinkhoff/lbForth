\ -*- forth -*- Copyright 2015 Lars Brinkhoff

\ Nucleus for Emacs bytecodes.

\ Constant vector elements:
\  0 - String representing the dictionary, which is also the bytecodes.
\  1 - A list representing the return stack.

code cold
   here 3 + goto, \ Patch this instruction for computed goto.
   \ (warm) goto,
end-code

code gosub ( S: a ret -- ) ( R: -- ret )
   \ Save return address.
   1 varref,
   cons,
   1 varset,

   \ Set first byte of target address.
   0 constant,
   (1) constant,
   2 stack-ref,
   aset,

   \ Set second byte of target address.
   0 constant,
   (2) constant,
   \ Shift return address.
   (ash) constant,
   2 stack-ref,
   (-8) constant,
   2 call,
   aset,

   2 discardN,
   0 goto,
end-code

code ret ( R: ret -- )
   ?
end-code

code exit ( R: ret -- )
   1 varref,
   dup,
   cdr,
   1 varset,
   car,
   \ TODO
end-code

code dodoes ( -- addr ) ( R: -- ret )
end-code

code 0branch ( x -- )
   (0) constant,
   eq,
   (label) goto,
end-code

code (literal) ( -- n )
   (n) constant,
end-code

code ! ( x a -- )
   \ TODO
end-code

code @ ( a -- x )
   \ TODO
end-code

code + ( x y -- x+y )
   plus,
end-code

code >r  ( x -- ) ( R: -- x )
   1 varref,
   cons,
   1 varset,
end-code

code r> ( -- x ) ( R: x -- )
   1 varref,
   dup,
   cdr,
   1 varset,
   car,
end-code

code invert ( u -- ~u )
   (lognot) constant,
   1 stack-ref,
   1 call,
   1 discard-preserving-tos,
end-code

code and ( u1 u2 -- u3 )
   (logand) constant,
   2 stack-ref,
   2 stack-ref,
   2 call,
   2 discard-preserving-tos,
end-code

code c! ( c addr -- )
   0 constant,
   1 stack-ref,
   3 stack-ref,
   aset,
   3 discardN,
end-code

code c@ ( addr -- c )
   0 constant,
   1 stack-ref,
   aref,
   1 discard-preserving-tos,
end-code

code emit ( c -- )
   \ TODO
end-code

code bye ( ... -- <no return> )
   return,
end-code

code close-file ( fileid -- ior )
   \ TODO
end-code

code open-file ( addr u mode -- fileid ior )
   \ TODO
end-code

code read-file ( addr u1 fileid -- u2 ior )
   \ TODO
end-code

code dup
   dup,
end-code

code drop
   discard,
end-code

code nip ( x1 x2 -- x2 )
   1 discard-preserving-tos,
end-code

code swap ( x1 x2 -- x2 x1 )
   dup,
   2 stack-ref,
   2 stack-set,
   2 stack-set,
end-code

code over ( x1 x2 -- x1 x2 x1 )
   1 stack-ref,
end-code

code rot ( x1 x2 x3 -- x2 x3 x1 )
   dup,
   3 stack-ref,
   3 stack-ref,
   5 stack-set,
   2 stack-set,
   2 stack-set,
end-code

code -rot ( x1 x2 x3 -- x3 x1 x2 )
   dup,
   3 stack-ref,
   3 stack-ref,
   3 stack-set,
   3 stack-set,
   3 stack-set,
end-code

code tuck ( x1 x2 -- x2 x1 x2 )
   dup,
   dup,
   3 stack-ref,
   3 stack-set,
   3 stack-set,
end-code

code 2drop
   2 discardN,
end-code

code 2dup ( x1 x2 -- x1 x2 x1 x2 )
   1 stack-ref,
   1 stack-ref,
end-code

code 2nip ( x1 x2 x3 x4 -- x3 x4 )
   2 stack-set,
   2 stack-set,
end-code

code 2swap ( x1 x2 x3 x4 -- x3 x4 x1 x2 )
   3 stack-ref,
   3 stack-ref,
   3 stack-ref,
   3 stack-ref,
   6 stack-set,
   6 stack-set,
   2 stack-set,
   2 stack-set,
end-code

code 2over ( x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 )
   3 stack-ref,
   3 stack-ref,
end-code

code =
   eq,
end-code

code 1-
   sub1,
end-code

code 1+
   add1,
end-code

code >
   gtr,
end-code

code <
   lss,
end-code

code <=
   leq,
end-code

code >=
   geq,
end-code

code -
   diff,
end-code

code negate
   negate,
end-code

code max
   max,
end-code

code min
   min,
end-code

code *
   mult,
end-code

code /
   quo,
end-code

code mod
   rem,
end-code
