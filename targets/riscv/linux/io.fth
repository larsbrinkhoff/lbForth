\ I/O for Linux RISC-V.  Copyright 2017 Lars Brinkhoff.

decimal

code emit
   1 # x10 li,
   S x11 mv,
   1 # x12 li,
   64 # x17 li, \ write
   ecall,
   4 # S S addi,
   next,
end-code

code bye
   0 # x10 li,
   93 # x17 li, \ exit
   ecall,
end-code

also assembler
   label tmp
   100 allot
previous

code open-file ( addr u mode -- fileid ior )
   0 # x18 li,

   8 S )# x5 lw,
   tmp # x6 lui,
   tmp # x6 x6 addi,
   4 S )# x7 lw,
   begin,
     x5 ) x8 lb,
     x6 ) x8 sb,
     1 # x5 x5 addi,
     1 # x6 x6 addi,
     -1 # x7 x7 addi,
   x7 0=, until,
   0 # x8 li,
   x6 ) x8 sb,

   -100 # x10 li, \ AT_FDCWD
   tmp # x11 lui,
   tmp # x11 x11 addi,
   S ) x12 lw,
   octal 644 # decimal x13 li,
   56 # x17 li, \ open
   ecall,

   x10 0<, if,
     x10 x18 mv,
     0 # x10 li,
   then,

   4 # S S addi,
   4 S )# x18 sw,
   S ) x10 sw,
   next,
end-code

code read-file ( addr u1 fileid -- u2 ior )
   0 # x18 li,

   S ) x10 lw,
   8 S )# x11 lw,
   4 S )# x12 lw,
   63 # x17 li, \ read
   ecall,

   x10 0<, if,
     x10 x18 mv,
     0 # x10 li,
   then,

   4 # S S addi,
   4 S )# x18 sw,
   S ) x10 sw,
   next,
end-code

code write-file ( addr u1 fileid -- u2 ior )
   0 # x18 li,

   S ) x10 lw,
   8 S )# x11 lw,
   4 S )# x12 lw,
   64 # x17 li, \ write
   ecall,

   x10 0<, if,
     x10 x18 mv,
     0 # x10 li,
   then,

   4 # S S addi,
   4 S )# x18 sw,
   S ) x10 sw,
   next,
end-code

code close-file ( fileid -- ior )
   S ) x10 lw,
   57 # x17 li, \ close
   ecall,
   S ) x10 sw,
   next,
end-code

hex
