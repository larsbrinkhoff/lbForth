\ I/O for Linux m68k.  Copyright 2016 Lars Brinkhoff.

code emit
   1 # d1 moveq,
   S d2 .l move,
   3 # d2 .l addq,
   1 # d3 moveq,
   4 # d0 moveq,
   0 # trap,
   4 # S .l addq,
   next,
end-code

code bye ( ... -- <no return> )
   0 # d1 moveq,
   1 # d0 moveq,
   0 # trap,
end-code

also assembler
   label tmp
   100 allot
previous

code open-file ( addr u mode -- fileid ior )
   S )+ d2 .l move,
   S )+ d0 .l move,
   S )+ a0 .l move,

   tmp a1 lea,
   begin,
     a0 )+ d3 .b move,
     d3 a1 )+ .b move,
     1 # d0 .l subq,
   0=, until,
   a1 ) .b clr,

   tmp # d1 .l move,
   octal 644 hex # d3 .l move,
   5 # d0 moveq,
   0 # trap,

   0 # d1 moveq,
   d0 .l tst,
   0<, if,
     d0 d1 .l move,
     0 # d0 moveq,
   then,

   d0 S -) .l move,
   d1 S -) .l move,

   next,
end-code

code read-file ( addr u1 fileid -- u2 ior )
   S )+ d1 .l move,
   S )+ d3 .l move,
   S )+ d2 .l move,
   3 # d0 moveq,
   0 # trap,

   0 # d1 moveq,
   d0 .l tst,
   0<, if,
     d0 d1 .l move,
     0 # d0 moveq,
   then,

   d0 S -) .l move,
   d1 S -) .l move,

   next,
end-code

code write-file ( addr u1 fileid -- u2 ior )
   S )+ d1 .l move,
   S )+ d3 .l move,
   S )+ d2 .l move,
   4 # d0 moveq,
   0 # trap,

   0 # d1 moveq,
   d0 .l tst,
   0<, if,
     d0 d1 .l move,
     0 # d0 moveq,
   then,

   d0 S -) .l move,
   d1 S -) .l move,

   next,
end-code

code close-file ( fileid -- ior )
   S ) d1 .l move,
   6 # d0 moveq,
   0 # trap,
   d0 S ) .l move,
   next,
end-code
