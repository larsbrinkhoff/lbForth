\ I/O for Linux ARM.  Copyright 2016 Lars Brinkhoff.

decimal

code emit
   1 # r0 mov,
   S r1 mov,
   1 # r2 mov,
   4 # r7 mov,
   0 # svc,
   4 # S S add,
   next,
end-code

code bye ( ... -- <no return> )
   0 # r0 mov,
   248 # r7 mov,
   0 # svc,
end-code

also assembler
   label tmp
   100 allot
previous

code open-file ( addr u mode -- fileid ior )
   8 S )# r0 ldr,
   tmp r1 adr,
   4 S )# r2 ldr,
   begin,
     1 r0 #) r12 ldrb,
     1 r1 #) r12 strb,
     1 # r2 r2 subs,
   0=, until,
   0 # r12 mov,
   r1 ) r12 strb,

   tmp r0 adr,
   S ) r1 ldr,
   octal 644 # decimal r2 mov,
   5 # r7 mov,
   0 # svc,

   r0 r0 tst,
   0 # r1 pl mov,
   r0 r1 mi mov,
   0 # r0 mi mov,

   4 S )#! r1 str,
   4 S )# r0 str,
   next,
end-code

code read-file ( addr u1 fileid -- u2 ior )
   S ) r0 ldr,
   8 S )# r1 ldr,
   4 S )# r2 ldr,
   3 # r7 mov,
   0 # svc,

   r0 r0 tst,
   0 # r1 pl mov,
   r0 r1 mi mov,
   0 # r0 mi mov,

   4 S )#! r1 str,
   4 S )# r0 str,
   next,
end-code

code write-file ( addr u1 fileid -- u2 ior )
   S ) r0 ldr,
   8 S )# r1 ldr,
   4 S )# r2 ldr,
   4 # r7 mov,
   0 # svc,

   r0 r0 tst,
   0 # r1 pl mov,
   r0 r1 mi mov,
   0 # r0 mi mov,

   4 S )#! r1 str,
   4 S )# r0 str,
   next,
end-code

code close-file ( fileid -- ior )
   S ) r0 ldr,
   6 # r7 mov,
   0 # svc,
   S ) r0 str,
   next,
end-code

hex
