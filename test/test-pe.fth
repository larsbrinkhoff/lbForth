require targets/x86/asm.fth
require lib/pe.fth

hex

here
pe-header,

pe-code

also assembler
ahead,

code main
   then,
   2A # eax mov,
   ret,
end-code

pe-end

here over - type
