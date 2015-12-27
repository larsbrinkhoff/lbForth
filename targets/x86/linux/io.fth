\ I/O for Linux x86.  Copyright 2015 Lars Brinkhoff.

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
