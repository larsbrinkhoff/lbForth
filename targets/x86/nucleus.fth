\ Nucleus for x86.  Copyright 2014 Lars Brinkhoff.

\	Legacy	ITC	DTC	STC
\ IP	eax	eax		pc
\ W	edx	edx
\ SP	memory	esp	esp
\ RP	memory			esp
\ T

also assembler definitions
: next,    ret, ;
: sp   [ also forth ] sp [ previous ] ;
previous definitions

hex

code cold
  \ Initialise dp, IP, SP, RP.
  \ Jump to WARM.
end-code

code exit
   RP edx mov,
   edx ) eax mov,
   4 # edx add,
   edx RP mov,
   next,
end-code

code dodoes
   SP ecx mov,
   ebx push,
   -4 ecx )# ebx lea,
   ebx SP mov,
   1C edx )# ebx lea,
   ebx -4 ecx )# mov,
   RP ecx mov,
   -4 ecx )# ebx lea,
   ebx RP mov,
   eax -4 ecx )# mov,
   14 edx )# eax mov,
   ebx pop,
   next,
end-code

code 0branch
   SP edx mov,
   ebx push,
   eax ) ecx mov,
   4 # eax add,
   edx ) ebx mov,
   4 # edx add,
   edx SP mov,
   ebx ebx test,
   ecx eax cmove,
   ebx pop,
   next,
end-code

code (literal)
   SP edx mov,
   -4 edx )# ecx lea,
   ecx SP mov,
   eax ) ecx mov,
   4 # eax add,
   ecx -4 edx )# mov,
   next,
end-code

code !
   SP edx mov,
   ebx push,
   4 edx )# ebx mov,
   edx ) ecx mov,
   8 # edx add,
   edx SP mov,
   ebx ecx ) mov,
   ebx pop,
   next,
end-code

code @
   SP edx mov,
   edx ) ecx mov,
   ecx ) ecx mov,
   ecx edx ) mov,
   next,
end-code

code +
   8 # esp sub,
   SP edx mov,
   ebx esp ) mov,
   esi 4 esp )# mov,
   4 edx )# ebx mov,
   4 edx )# esi lea,
   edx ) ecx mov,
   esi SP mov,
   4 esp )# esi mov,
   ebx ecx add,
   esp ) ebx mov,
   ecx 4 edx )# mov,
   8 # esp add,
   next,
end-code

code >r
   SP edx mov,
   ebx push,
   edx ) ecx mov,
   4 # edx add,
   edx SP mov,
   RP edx mov,
   -4 edx )# ebx lea,
   ebx RP mov,
   ecx -4 edx )# mov,
   ebx pop,
   next,
end-code

code r>
   RP edx mov,
   ebx push,
   edx ) ecx mov,
   4 # edx add,
   edx RP mov,
   SP edx mov,
   -4 edx )# ebx lea,
   ebx SP mov,
   ecx -4 edx )# mov,
   ebx pop,
   next,
end-code

code nand
   8 # esp sub,
   SP ecx mov,
   ebx esp ) mov,
   esi 4 esp )# mov,
   ecx ) ebx mov,
   4 ecx )# esi lea,
   4 ecx )# edx mov,
   esi SP mov,
   4 esp )# esi mov,
   ebx edx and,
   esp ) ebx mov,
   edx not,
   edx 4 ecx )# mov,
   8 # esp add,
   next,
end-code

code c!
   SP edx mov,
   ebx push,
   4 edx )# ebx mov,
   edx ) ecx mov,
   8 # edx add,
   edx SP mov,
   bl ecx ) mov,
   ebx pop,
   next,
end-code

code c@
   SP edx mov,
   edx ) ecx mov,
   ecx ) ecx movzx,
   ecx edx ) mov,
   next,
end-code

\ code emit ( c -- )
\ code bye ( ... -- <no return> )
\ code close-file ( fileid -- ior )
\ code open-file ( addr u mode -- fileid ior )
\ code read-file ( addr u1 fileid -- u2 ior )

decimal
