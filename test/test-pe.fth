require search.fth

vocabulary cross
only forth also cross definitions
include lib/image.fth
require targets/x86/asm.fth
require lib/pe.fth

-11 constant std-out-handle

target-image hex 400000 org

pe-header,

pe-code
401000 org

pe-extern ExitProcess
pe-extern GetStdHandle
pe-extern WriteFile

pe-import kernel32.dll
GetStdHandle kernel32.dll pe-symbol
ExitProcess kernel32.dll pe-symbol
WriteFile kernel32.dll pe-symbol

also assembler
label hello  s" hello world " ",
label written  0 ,

decimal

code main
   here pe-entry

   std-out-handle # push,
   GetStdHandle indirect-call,

   0 # push,
   written # push,
   12 # push,
   hello # push,
   eax push,
   WriteFile indirect-call,

   42 # push,
   ExitProcess indirect-call,
end-code

pe-end

target-region type
bye
