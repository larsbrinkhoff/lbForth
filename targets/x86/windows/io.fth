\ Windows I/O.  Copyright 2015 Lars Brinkhoff.

also forth definitions

pe-extern ExitProcess
pe-extern GetStdHandle
pe-extern CreateFileA
pe-extern ReadFile
pe-extern WriteFile
pe-extern CloseHandle

pe-import kernel32.dll
ExitProcess kernel32.dll pe-symbol
GetStdHandle kernel32.dll pe-symbol
CreateFileA kernel32.dll pe-symbol
ReadFile kernel32.dll pe-symbol
WriteFile kernel32.dll pe-symbol
CloseHandle kernel32.dll pe-symbol

target

also assembler
label written  0 ,
previous

code get-std-handle ( u1 -- u2 )
   eax ebx mov,
   GetStdHandle indirect-call,
   eax push,
   ebx eax mov,
   next,
end-code

code emit
   S W mov,
   eax push,

   decimal -11 # push, hex \ STD_OUTPUT_HANDLE
   GetStdHandle indirect-call,

   0 # push,
   written # push,
   1 # push,
   W push,
   eax push,
   WriteFile indirect-call,

   eax pop,
   4 # S add,
   next,
end-code

code bye ( ... -- <no return> )
   0 # push,
   ExitProcess indirect-call,
end-code

also assembler
   label tmp
   100 allot
previous

code open-file ( addr u desired-access creation-disposition -- fileid ior )
   S ebx mov,
   eax push,
   esi push,
   C ebx )# esi mov,
   tmp edi lea,
   8 ebx )# ecx mov,
   rep, movsb,
   cl edi ) mov,

   0 # push,
   80 # push,
   ebx ) eax mov,  eax push,
   0 # push,
   0 # push,
   4 ebx )# eax mov,  eax push,
   tmp # push,
   CreateFileA indirect-call,

   ebp ebp xor,
   -1 # eax cmp,
   0=, if,
     eax ebp xchg,
   then,
   ebp 8 ebx )# mov,
   eax C ebx )# mov,

   esi pop,
   eax pop,
   8 # esp add,
   next,
end-code

code read-file ( addr u1 fileid -- u2 ior )
   S ebx mov,
   eax push,

   0 # push,
   written # push,
   4 ebx )# eax mov,
   eax push,
   8 ebx )# eax mov,
   eax push,
   ebx ) eax mov,
   eax push,
   ReadFile indirect-call,

   W W xor,
   eax eax test,
   0=, if,
     -1 # W mov,
   then,
   W 4 ebx )# mov,
   written W mov,
   W 8 ebx )# mov,

   eax pop,
   4 # esp add,
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
   S ) W mov,
   eax push,
   W push,
   CloseHandle indirect-call,
   W W xor,
   eax eax test,
   0=, if,
     -1 # W mov,
   then,
   eax pop,
   W S ) mov,
   next,
end-code
