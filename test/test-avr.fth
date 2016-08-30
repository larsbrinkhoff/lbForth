require search.fth

vocabulary cross
only forth also cross definitions
1 constant t-little-endian
include lib/image.fth
include targets/avr/asm.fth

256 t-allot

hex only forth definitions also cross

also assembler
target-image
0 org

  nop,
  nop,
  nop,
  break,

host-image
save-target
