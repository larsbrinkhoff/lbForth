require targets/x86/asm.fth
require lib/elf.fth

hex

: output ( a1 -- )   ( dup here over - dump )  here swap do i c@ emit loop ;

here
0 >body entry-offset +!
08048000 x86 elf32,

code main
   1 # eax mov,
   2A # ebx mov,
   80 # int,
end-code

elf!
output
