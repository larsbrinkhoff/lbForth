900 org

also assembler ahead, previous \ Jump to COLD.

code bye
   a7 -) .w clr,
   1 # trap, \ Pterm0
   next,
end-code

code emit
   d0 pop,
   d0 a7 -) .w move,
   2 # a7 -) .w move,
   1 # trap, \ Cconout
   4 # a7 .l addq,
   next,
end-code

also assembler
   label tmp
   100 allot
previous

code open-file ( addr u mode -- fileid ior )
   \ Fopen ( mode/w name 3D -- )
   S )+ d1 .l move,
   S )+ d0 .l move,
   S )+ a0 .l move,

   tmp a1 lea,
   begin,
     a0 )+ d2 .b move,
     d2 a1 )+ .b move,
     1 # d0 .l subq,
   0=, until,
   a1 ) .b clr,

   d1 a7 -) .w move,
   tmp pea,
   0 # d1 moveq,
   003D # a7 -) .w move,
   1 # trap, \ Fopen
   8 # a7 .l addq,

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
   \ Fread ( handle/w u addr 3F -- )

   S )+ d1 .l move,
   S )+ d0 .l move,
   S )+ a0 .l move,

   a0 a7 -) .l move,
   d0 a7 -) .l move,
   d1 a7 -) .w move,
   0 # d1 moveq,
   003F # a7 -) .w move,
   1 # trap,
   000C a7 )# a7 lea,

   d0 .l tst,
   0<, if,
     d0 d1 .l move,
     0 # d0 moveq,
   then,

   d0 S -) .l move,
   d1 S -) .l move,
   next,
end-code

code write-file
   \ Fwrite ( handle/w u addr 40 -- )
   d6 d6 .b move,
   illegal,
   next,
end-code

code close-file
   d0 pop,
   d0 a7 -) .w move,
   3E # a7 -) .w move,
   1 # trap, \ Fclose
   4 # a7 .l addq,
   S -) .l clr,
   next,
end-code
