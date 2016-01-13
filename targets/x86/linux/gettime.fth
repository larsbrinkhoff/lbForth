require targets/x86/asm.fth
require targets/x86/next.fth

code gettime ( -- nanos seconds )
   8 # esp sub,
   esp ecx mov,
   eax push,
   ebx push,

   \ Call clock_gettime
   hex
   109 # eax mov,
   2 # ebx mov,
   80 # int,
   decimal

   ebx pop,
   eax pop,

   \ Next
   eax ) edx mov,
   4 # eax add,
   edx ) indirect-jmp,
end-code
