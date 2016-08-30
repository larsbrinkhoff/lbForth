require search.fth

vocabulary cross
only forth also cross definitions
0 constant t-little-endian
0 constant t-endian
include lib/image.fth
include targets/m68k/asm.fth
include lib/gemdos.fth


hex only forth definitions also cross

: z", ( a u -- ) move, 0 c, align ;

0 constant load-address

also assembler
target-image
load-address org

gemdos-header,

  ahead,
  label hello  s" hello world " z",
  then,

  hello pc) pea,
  9 # a7 -) .w move,
  1 # trap,

  a7 -) .w clr,
  1 # trap,

gemdos-end

save-target bye
