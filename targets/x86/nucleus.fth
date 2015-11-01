\ Nucleus for x86.  Copyright 2014-2015 Lars Brinkhoff.

\	Legacy	ITC	DTC	STC
\ IP	eax			pc
\ W	edx	edx
\ SP	memory	esp	esp	esi?
\ RP	memory	esi?		esp
\ T		eax	eax	eax

hex

host also assembler definitions
: SP   [ also forth ] sp [ previous ] ;
: IP   eax ;
: W   edx ;
: next,   
   IP ) W mov,
   4 # IP add,
   18 W )# indirect-jmp, ;

target

\ Jump straight to BYE.
also assembler
ahead,
previous

code cold
  \ Initialise dp, IP, SP, RP.
  \ Jump to WARM.
end-code

code exit
   RP W mov,
   W ) IP mov,
   4 # W add,
   W RP mov,
   next,
end-code

code dodoes
   RP ecx mov,
   4 # ecx sub,
   IP ecx ) mov,
   ecx RP mov,
   14 W )# IP mov,
   SP ecx mov,
   4 # ecx sub,
   1C W )# W lea,
   W ecx ) mov,
   ecx SP mov,
   next,
end-code

code 0branch
   IP ) W mov,
   4 # IP add,
   SP ecx mov,
   4 # ecx add,
   ecx SP mov,
   -4 ecx )# ecx mov,
   ecx ecx test,
   W IP cmove,
   next,
end-code

code (literal)
   SP edx mov,
   -4 edx )# ecx lea,
   ecx SP mov,
   IP ) ecx mov,
   4 # IP add,
   ecx -4 edx )# mov,
   next,
end-code

code !
   SP esp xchg,
   ecx pop,
   edx pop,
   edx ecx ) mov,
   SP esp xchg,
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
   SP W mov,
   W ) ecx mov,
   4 # W add,
   W ) ecx add,
   ecx W ) mov,
   W SP mov,
   next,
end-code

code >r
   SP edx mov,
   edx ) ecx mov,
   4 # edx add,
   edx SP mov,
   RP edx mov,
   4 # edx sub,
   edx RP mov,
   ecx edx ) mov,
   next,
end-code

code r>
   RP edx mov,
   ebx push,
   edx ) ecx mov,
   4 # edx add,
   edx RP mov,
   SP edx mov,
   4 # edx sub,
   edx SP mov,
   ecx edx ) mov,
   ebx pop,
   next,
end-code

code nand
   SP W mov,
   W ) ecx mov,
   4 # W add,
   W ) ecx and,
   ecx not,
   ecx W ) mov,
   W SP mov,
   next,
end-code

code c!
   SP W mov,
   8 # SP add,
   4 W )# ecx mov,
   W ) W mov,
   cl W ) mov,
   next,
end-code

code c@
   SP W mov,
   W ) ecx mov,
   ecx ) ecx movzx,
   ecx W ) mov,
   next,
end-code

\ code emit ( c -- )

code bye ( ... -- <no return> )
   then,
   1 # eax mov,
   2A # ebx mov,
   80 # int,
end-code

\ code close-file ( fileid -- ior )
\ code open-file ( addr u mode -- fileid ior )
\ code read-file ( addr u1 fileid -- u2 ior )

code branch
   IP ) IP mov,
   next,
end-code

code execute
   SP ecx mov,
   ecx ) W mov,
   4 # ecx add,
   ecx SP mov,
   18 W )# indirect-jmp,
end-code

code r@
   RP edx mov,
   edx ) ecx mov,
   SP edx mov,
   4 # edx sub,
   ecx edx ) mov,
   edx SP mov,
   next,
end-code

code 0=
   SP W mov,
   1 # W ) cmp,
   ecx ecx sbb,
   ecx W ) mov,
   next,
end-code

code 0<>
   SP W mov,
   W ) ecx mov,
   ecx neg,
   ecx ecx sbb,
   ecx W ) mov,
   next,
end-code

code 0<
   SP W mov,
   W ) ecx mov,
   1F # ecx sar,
   ecx W ) mov,
   next,
end-code

code drop
   SP W mov,
   4 # W add,
   W SP mov,
   next,
end-code

code dup
   SP W mov,
   W ) ecx mov,
   4 # W sub,
   ecx W ) mov,
   W SP mov,
   next,
end-code

code ?dup
   SP W mov,
   W ) ecx mov,
   ecx ecx test,
   0<>, if,
     4 # W sub,
     ecx W ) mov,
     W SP mov,
   then,
   next,
end-code

code nip
   SP W mov,
   W ) ecx mov,
   4 # W add,
   ecx W ) mov,
   W SP mov,
   next,
end-code

code swap
   SP W mov,
   W ) ecx mov,
   ecx 4 W )# xchg,
   ecx W ) mov,
   next,
end-code

code over
   SP W mov,
   4 W )# ecx mov,
   4 # W sub,
   ecx W ) mov,
   W SP mov,
   next,
end-code

\ code tuck ( x1 x2 -- x2 x1 x2 )
\ code rot
\ code -rot
\ code 2drop
\ code 2dup
\ code 2nip
\ code 2swap
\ code 2over
\ code 2tuck
\ code 2rot

code negate
   SP W mov,
   W ) ecx mov,
   ecx neg,
   ecx W ) mov,
   next,
end-code

code -
   SP W mov,
   4 W )# ecx mov,
   W ) ecx sub,
   W 4 # add,
   ecx W ) mov,
   W SP mov,
   next,
end-code

code =
   SP W mov,
   W ) ecx mov,
   W 4 # add,
   W ) ecx sub,
   1 # ecx sub,
   ecx ecx sbb,
   ecx W ) mov,
   W SP mov,
   next,
end-code

code <>
   SP W mov,
   W ) ecx mov,
   W 4 # add,
   W ) ecx sub,
   ecx neg,
   ecx ecx sbb,
   ecx W ) mov,
   W SP mov,
   next,
end-code

code 1+
   SP W mov,
   W ) ecx mov,
   1 # ecx add,
   ecx W ) mov,
   W SP mov,
   next,
end-code

code 1-
   SP W mov,
   W ) ecx mov,
   -1 # ecx add,
   ecx W ) mov,
   W SP mov,
   next,
end-code

code cell+
   SP W mov,
   W ) ecx mov,
   4 # ecx add,
   ecx W ) mov,
   W SP mov,
   next,
end-code

code cells
   SP W mov,
   W ) ecx mov,
   2 # ecx shl,
   ecx W ) mov,
   W SP mov,
   next,
end-code

\ code >
\ code <
\ code <=
\ code >=
\ code abs
\ code max
\ code min
\ code *
\ code /
\ code mod

code invert
   SP W mov,
   W ) ecx mov,
   ecx not,
   ecx W ) mov,
   next,
end-code

code or
   SP W mov,
   W ) ecx mov,
   4 # W add,
   W ) ecx or,
   ecx W ) mov,
   W SP mov,
   next,
end-code

code xor
   SP W mov,
   W ) ecx mov,
   4 # W add,
   W ) ecx xor,
   ecx W ) mov,
   W SP mov,
   next,
end-code

code and
   SP W mov,
   W ) ecx mov,
   4 # W add,
   W ) ecx and,
   ecx W ) mov,
   W SP mov,
   next,
end-code

decimal
