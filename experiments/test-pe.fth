require targets/x86/asm.fth
require lib/pe.fth

hex

here
0 >body entry-offset +!
pe,

code main
   2A # eax mov,
   ret,
end-code

pe!
here over - type
