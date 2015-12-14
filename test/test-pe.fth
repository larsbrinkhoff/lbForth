require search.fth

vocabulary cross
only forth also cross definitions
include lib/image.fth
require targets/x86/asm.fth
require lib/pe.fth

target-image hex 400000 org

pe-header,

pe-code

code main
   here pe-entry
   2A # eax mov,
   ret,
end-code

pe-end

target-region type
