\ Assembler for x86.

\ Adds to FORTH vocabulary: ASSEMBLER CODE ;CODE.
\ Creates ASSEMBLER vocabulary with: END-CODE and x86 opcodes.

include comus.fth
require search.fth

vocabulary assembler
also assembler definitions

' here  defer here  is here
' c,    defer c,    is c,

: h,   dup c,  8 rshift c, ;
: ,   dup h,  16 rshift h, ;

: (;code)      r> code! ;
: end-code     align previous ;

base @  hex

variable d
variable s
variable mod/reg/rm
variable dir?
variable 'disp
variable disp
variable 'imm
variable imm
variable 'reg

: !op8    0 s ! ;
: !op32   1 s ! ;
: !op16   s @ 0= if 66 c, !op32 then ;

: !reg   dir? @ if 2 d ! then dir? off ;
: !mem   dir? off ;

: imm8    imm @ c, ;
: disp8   disp @ c, ;
: imm32   imm @ , ;
: disp32   disp @ , ;

: byte?   -81 80 rot within ;
: disp!   'disp ! disp ! ;
: !disp8   ['] disp8 disp! ;
: !disp32   ['] disp32 disp! ;
: !disp   dup byte? if !disp8 40 else !disp32 80 then ;

: mod     mod/reg/rm @ c0 and ;
: mod/rm     mod/reg/rm @ c7 and ;
: mod/reg     mod/reg/rm @ f8 and ;
: rm<<3   mod/reg/rm @ 3 and 3 lshift ;
: reg1   3 lshift mod/rm + mod/reg/rm !  !reg ;
: reg2   mod/reg + mod/reg/rm !  !mem ;
: ind   mod/reg/rm @ 38 and + mod/reg/rm !  !mem ;
: ind-off   swap !disp + ind ;
: addr   !disp32  05 ind ;

: reset   imm off  ['] nop dup 'disp ! 'imm !  c0 mod/reg/rm !
   d off  s off  dir? on   ['] reg1 'reg ! ;
: start-code   also assembler reset ;

: addr?   dup -1 = ;
: op   addr? if drop execute else addr then ;

: ?sib, ;
: ?disp,   'disp perform ;
: ?imm,    'imm perform ;

: mod/reg/rm,   mod/reg/rm @ c, ;
: suffixes,   ?sib, ?disp, ?imm, reset ;
: ds   d @ s @ + ;
: 0op   create ,  does> @ c, reset ;
: 1reg   create ,  does> @ >r 2drop r> + c, reset ;
: 2op   create ,  does> @ >r op op r> ds + c, mod/reg/rm, suffixes, ;
: 2op-no-d   create ,  does> @ >r op op r> s @ + c, mod/reg/rm, suffixes, ;
: 2op-no-ds   create ,  does> @ >r op op r> c, mod/reg/rm, suffixes, ;

00 2op add,
08 2op or,
\ 0F44 cmove,
\ 0FB6 movzbl,
10 2op adc,
18 2op sbb,
20 2op and,
26 0op es,
28 2op sub,
2E 0op cs,
30 2op xor,
36 0op ss,
38 2op cmp,
3E 0op ds,
50 1reg push,
58 1reg pop,
64 0op fs,
65 0op gs,
\ 66 op size prefix
\ 67 address size prefix
\ 68 push imm32
\ 6A push imm8
\ 70 jcc
\ 80 immediate add/or/adc/sbb/and/sub/xor/cmp
84 2op-no-d test,
86 2op-no-d xchg,
88 2op mov,
8D 2op-no-ds lea,
90 0op nop,
\ B0 immediate mov
\ A8 test
C3 0op ret,
\ E8 call,
\ E9 jmp,
F0 0op lock,
F2 0op rep,
F3 0op repz,

: #   ['] nop -1  ['] imm32 'imm ! ;
: )   2drop ['] ind -1  ['] reg1 'reg ! ;
: )#   2drop  ['] ind-off -1  ['] reg1 'reg ! ;

: reg@   'reg @  ['] reg2 'reg ! ;

: reg8    create ,  does> @ reg@ -1 !op8 ;
: reg16   create ,  does> @ reg@ -1 !op16 ;
: reg32   create ,  does> @ reg@ -1 !op32 ;
: reg:    dup reg8 dup reg16 dup reg32 1+ ;

0
reg: al ax eax   reg: cl cx ecx   reg: dl dx edx   reg: bl bx ebx
reg: ah sp esp   reg: ch bp ebp   reg: dh si esi   reg: bh di edi
drop

base !  only forth definitions  also assembler

: code    create  latestxt >body code!  start-code  ;
: ;code   postpone (;code) reveal postpone [ ?csp start-code ; immediate

previous

: fail? ( c a -- a' f ) 1- tuck c@ <> ;
: .fail   cr ." FAIL: " source 5 - type cr ;
: ?fail   fail? if .fail abort then ;
: check   here begin depth 1- while ?fail repeat drop ;

.( Assembler test: )
code assembler-test
   hex

   eax ebx mov,             89 C3  check
   ebx ecx mov,             89 D9  check
   ecx ebx mov,             89 CB  check
   ecx ) edx mov,           8B 11  check
   edx ) ecx mov,           8B 0A  check
   ecx edx ) mov,           89 0A  check
   edx ecx ) mov,           89 11  check
   -80 eax )# eax mov,      8B 40 80  check
   edi 7F edi )# mov,       89 7F 7F  check
   eax -81 eax )# mov,      89 80 7F FF FF FF  check
   edi 80 edi )# mov,       89 BF 80 00 00 00  check
   10203040 eax mov,        8B 05 40 30 20 10  check
   edi 10203040 mov,        89 3D 40 30 20 10  check

   eax ebx ) test,          85 03  check
   eax ) ebx xchg,          87 18  check
   eax push,                50  check     
   ebx pop,                 5B  check
   nop,                     90  check

   decimal
end-code
.( OK ) cr

: %sp sp ;

: foobar   create , ;code
   0 >body edx )# edx mov,
   %sp ecx mov,
   cell negate ecx )# ecx lea,
   ecx %sp mov,
   edx ecx ) mov,
   ret,
end-code

code my-fetch
   %sp edx mov,
   edx ) ecx mov,
   ecx ) ecx mov,
   ecx edx ) mov,
   ret,
end-code
