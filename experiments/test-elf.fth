require targets/x86/asm.fth

also assembler
require lib/elf.fth
previous

hex

08048000 constant entry-point

also assembler
target-image
0 org

entry-point x86 elf32,

entry-point org

   1 # eax mov,
   2A # ebx mov,
   80 # int,

;elf

target-region type bye
