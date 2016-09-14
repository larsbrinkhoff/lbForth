require search.fth

1 constant t-little-endian

vocabulary cross
only forth also cross definitions
include lib/image.fth
include targets/x86/asm.fth
include lib/elf.fth
t-little-endian 4 section: target-image


hex only forth definitions also cross

08048000 constant load-address
256 t-allot

also assembler
target-image
load-address org

load-address x86 elf32-header,

here elf-entry-point

   1 # eax mov,
   2A # ebx mov,
   80 # int,

elf-end

target-region type bye
