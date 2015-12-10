require search.fth

vocabulary cross
only forth also cross definitions
include lib/image.fth
include targets/x86/asm.fth
include lib/elf.fth


hex only forth definitions also cross

08048000 constant load-address
load-address 54 + constant entry-point

also assembler
target-image
0 org

load-address x86 elf32-header,

entry-point org

   1 # eax mov,
   2A # ebx mov,
   80 # int,

elf-end

target-region type bye
