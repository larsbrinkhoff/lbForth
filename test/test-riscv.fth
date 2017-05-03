require search.fth

1 constant t-little-endian

vocabulary cross
only forth also cross definitions
include lib/image.fth
include targets/riscv/asm.fth
include lib/elf.fth


hex only forth definitions also cross

08048000 constant load-address

also assembler
target-image
load-address org

load-address riscv elf32-header,

here elf-entry-point

   02A # x10 li,
   05D # x17 li,
   ecall,

elf-end

target-region type bye
