require search.fth

vocabulary cross
only forth also cross definitions
include lib/image.fth
true 2 section: target-image
include targets/pdp11/asm.fth
include lib/aout.fth

only forth definitions also cross

: ?padding,   begin here 48 < while 0 c, repeat ;

0 constant load-address
256 t-allot

also assembler
target-image
17000 org
aout-header,

  0 org
  here aout-entry-point

  r0 clr,
  0 sp )# r0 mov,
  here 4 + r5 jsr,
  here 4 + jmp,
  here 2 + br,
  halt,
  reset,

  ?padding,

aout-end
host-image
save-target
