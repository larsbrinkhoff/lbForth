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
   ecx ) bl mov,            8A 19  check
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
   here call,               E8 FB FF FF FF  check
   here jmp,                E9 FB FF FF FF  check
   edx not,                 F7 D2  check
   ecx neg,                 F7 D9  check
   42 # int,                CD 42  check
   42 # push,               6A 42  check
   10203040 # push,         68 40 30 20 10  check
   42 # edx ) test,         F7 02 42 00 00 00  check

   1 # edx shr,             D1 EA  check
   2 # eax sar,             C1 F8 02  check
   ecx esi ) shl,           D3 26  check

   create l \ label
   l jo,                    70 FE  check
   l jc,                    72 FC  check
   l je,                    74 FA  check
   l ja,                    77 F8  check
   l js,                    78 F6  check
   l jp,                    7A F4  check
   l jl,                    7C F2  check
   l jg,                    7F F0  check

   ahead, then,             E9 00 00 00 00  check
   0=, if, then,            75 00  check
   begin, again,            E9 FB FF FF FF  check
   begin, 0<>, until,       74 FE  check

   eax indirect-jmp,        FF E0  check
   18 edx )# indirect-jmp,  FF 62 18  check

   decimal
end-code
.( PASS ) cr

\ The tests below involve running assembled code, so doesn't work
\ in 64-bit mode.
cell 4 = [if]

.( Test ;CODE )
: test-;code   create , ;code
   also forth sp previous esp xchg,
   0 >body edx )# edx mov,
   edx push,
   also forth sp previous esp xchg,
   ret,
end-code

42 test-;code test
: check-;code   test 42 <> abort" FAIL" ;
check-;code
.( PASS ) cr

\ Redefine CODE and END-CODE to overwrite the code field of the
\ previously defined word with the same name.
: code   source type  >in @ ' >code swap >in !  code  here ;
: set-code   latestxt >code @ swap ! ;

also assembler definitions
: end-code   here over - hex cr cdump  end-code  set-code ;

only forth definitions
vocabulary newcleus
also newcleus definitions

include targets/x86/nucleus.fth
only forth definitions

[then]
