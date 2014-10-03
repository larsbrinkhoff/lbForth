\ Test assembler and nucleus by loading into running Forth.
\ The existing CODE words will be patched to point to the
\ new nucleus.

require targets/x86/asm.fth

: fail? ( c a -- a' f ) 1- tuck c@ <> ;
: .fail   cr ." FAIL: " source 5 - type cr ;
: ?fail   fail? if .fail abort then ;
: check   here begin depth 1- while ?fail repeat drop ;

.( Assembler test: )
code assembler-test
   hex

   eax ebx mov,             8B D8  check \ 89 C3
   ebx ecx mov,             8B CB  check \ 89 D9
   ecx ebx mov,             8B D9  check \ 89 CB
   ecx ) edx mov,           8B 11  check
   edx ) ecx mov,           8B 0A  check
   ecx edx ) mov,           89 0A  check
   edx ecx ) mov,           89 11  check
   -80 eax )# eax mov,      8B 40 80  check
   edi 7F edi )# mov,       89 7F 7F  check
   eax -81 eax )# mov,      89 80 7F FF FF FF  check
   edi 80 edi )# mov,       89 BF 80 00 00 00  check
   10203040 eax mov,        8B 05 40 30 20 10  check  \ A1 40 30 20 10
   edi 10203040 mov,        89 3D 40 30 20 10  check

   bl ecx ) mov,            88 19  check
   ebx esp ) mov,           89 1C 24  check
   esi 4 esp )# mov,        89 74 24 04  check

   42 # al mov,             B0 42  check
   42 # ax mov,             66 B8 42 00  check
   42 # eax mov,            B8 42 00 00 00  check
 \ 42 # eax ) mov,          \ C7 00 42 00 00 00  check

   42 # al add,             82 C0 42  check  \ 04 42
   42 # bh add,             82 C7 42  check
   42 # cx add,             66 83 C1 42  check
   42 # edx add,            83 C2 42  check
   10203040 # esi add,      81 C6 40 30 20 10  check
 \ 10203040 # eax add,      \ 05 42 40 30 20 10

   -4 ecx )# ebx lea,       8D 59 FC  check
   8 # esp sub,             83 EC 08  check
   eax ebx ) test,          85 03  check
   eax ) ebx xchg,          87 18  check
   eax push,                50  check
   ebx pop,                 5B  check
   ret,                     C3  check
   nop,                     90  check
   clc,                     F8  check
   std,                     FD  check

   ecx ) ecx movzx,         0F B6 09  check
   ecx eax cmove,           0F 44 C1  check
   10203040 call,           E8 40 30 20 10  check
   10203040 jmp,            E9 40 30 20 10  check
   edx not,                 F7 D2  check
   ecx neg,                 F7 D9  check
   42 # int,                CD 42  check

   decimal
end-code
.( PASS ) cr

\ Redefine CODE and END-CODE to overwrite the code field of the
\ previously defined code word with the same name.
: code   source type  >in @ ' >code swap >in !  code  here ;
also assembler definitions
: end-code   here over - hex cr cdump  end-code  latestxt >code @ swap ! ;

only forth definitions
vocabulary newcleus
also newcleus definitions

include targets/x86/nucleus.fth
only forth definitions
