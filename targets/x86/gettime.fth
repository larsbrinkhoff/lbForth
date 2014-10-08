code gettime ( -- nanos seconds )
   eax push,
   ebx push,

   \ Call clock_gettime
   hex
   109 # eax mov,
   2 # ebx mov,
   -8 esp )# ecx lea,
   80 # int,
   decimal

   \ Push to Forth stack
   also forth SP previous edx mov,
   8 # edx sub,
   edx also forth SP previous mov,
   -4 esp )# ecx mov,
   ecx 4 edx )# mov,
   -8 esp )# ecx mov,
   ecx edx ) mov,

   ebx pop,
   eax pop,
   ret,
end-code
