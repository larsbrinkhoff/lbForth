\ I/O for PDP-11 Unix  Copyright 2016 Lars Brinkhoff.

decimal

\ Make apout happy.  Apout is an emulator for PDP-11 a.out binaries.
\ It wants a particular instruction at location 2.
also assembler
  0 org
  halt,
  0 sp )# r0 mov,
previous

code emit
   S here 10 + mov,
   1 # r0 mov,
   4 # trap,
   0 ,
   1 ,
   2 # S add,
   next,
end-code

code bye
   r0 clr,
   1 # trap,
end-code

also assembler
   label tmp
   100 allot
previous

code open-file ( addr u mode -- fileid ior )
   S )+ here 28 + mov,
   S )+ r2 mov,
   S )+ r0 mov,
   
   tmp # r1 mov,
   begin,
     r0 )+ r1 )+ movb,
     1 # r2 sub,
   0=, until,
   r1 ) clrb,

   r3 clr,
   5 # trap,
   tmp ,
   0 ,

   carry, if,
     r0 r3 mov,
     r0 clr,
   then,
   r0 push,
   r3 push,

   next,
end-code

code read-file ( addr u1 fileid -- u2 ior )
   r3 clr,
   S )+ r0 mov,
   S )+ here 12 + mov,
   S )+ here 6 + mov,
   3 # trap,
   0 ,
   0 ,

   carry, if,
     r0 r3 mov,
     r0 clr,
   then,
   r0 push,
   r3 push,

   next,
end-code

code write-file ( addr u1 fileid -- u2 ior )
   \ 4 {3, 1}
   halt,
   next,
end-code

code close-file ( fileid -- ior )
   S ) r0 mov,
   6 # trap,
   r0 S ) mov,
   next,
end-code

hex
