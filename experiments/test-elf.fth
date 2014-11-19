require targets/x86/asm.fth
require lib/elf.fth

hex

here
0 >body entry-offset +!
08048000 x86 elf32,

code main
   1 # eax mov,
   2A # ebx mov,
   80 # int,
end-code

elf!
here over - type bye
