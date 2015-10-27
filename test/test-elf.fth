require targets/x86/asm.fth

also assembler
require lib/elf.fth
previous

hex

08048000 constant load-address
load-address 54 + constant entry-point

also assembler
target-image
0 org

load-address x86 elf32,

entry-point org

   1 # eax mov,
   2A # ebx mov,
   80 # int,

;elf

target-region type bye
