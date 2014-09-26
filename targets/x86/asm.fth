\ Assembler for x86.

\ Adds to FORTH vocabulary: ASSEMBLER CODE ;CODE.
\ Creates ASSEMBLER vocabulary with: END-CODE and x86 opcodes.

include lib/comus.fth
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

variable opcode
variable d
variable s
variable mod/reg/rm
variable dir?
variable 'disp
variable disp
variable 'imm2
variable 'imm
variable imm
variable 'reg

: !reg   dir? @ if 2 d ! then dir? off ;
: !mem   dir? off ;

: imm8    imm @ c, ;
: disp8   disp @ c, ;
: imm16   imm @ dup c, 8 rshift c, ;
: imm32   imm @ , ;
: disp32   disp @ , ;

: !op8    0 s !  ['] imm8 'imm2 ! ;
: !op32   1 s !  ['] imm32 'imm2 ! ;
: !op16   s @ 0= if 66 c, !op32 then  ['] imm16 'imm2 ! ;

: byte?   -81 80 rot within ;
: disp!   'disp ! disp ! ;
: !disp8   ['] disp8 disp! ;
: !disp32   ['] disp32 disp! ;
: !disp   dup byte? if !disp8 40 else !disp32 80 then ;

: mod!   mod/reg/rm c0 !bits ;
: reg@   mod/reg/rm 38 @bits ;
: reg!   mod/reg/rm 38 !bits ;
: rm!   mod/reg/rm 7 !bits ;

: reg1   3 lshift reg! !reg ;
: reg2   rm! !mem ;
: ind   dup mod! rm! !mem ;
: ind-off   swap !disp + ind ;
: addr   !disp32  05 ind ;

: alu?   -1 38 opcode @ within ;
: reg>>3   reg@ 3 rshift rm! ;
: ?sign-extended   d off  imm @ byte? if 2 d !  ['] imm8 'imm ! then ;
: ?>imm-op   alu? if reg>>3 opcode @ reg! 80 opcode ! ?sign-extended then ;
: imm-op   imm !  'imm2 @ 'imm !  ?>imm-op ;

: 0reg   ['] reg1 'reg ! ;
: 0imm   imm off  ['] nop 'imm ! ;
: 0disp   ['] nop 'disp ! ;
: 0ds   d off  s off ;
: 0mod/reg/rm   c0 mod/reg/rm ! ;
: reset   0imm 0disp 0reg 0ds  0mod/reg/rm  dir? on ;
: start-code   also assembler reset ;

: addr?   dup -1 = ;
: op   addr? if drop execute else addr then ;

: ?sib, ;
: ?disp,   'disp perform ;
: ?imm,    'imm perform ;

: ds   d @ s @ + ;
: opcode!   @ opcode ! ;
: opcode,   opcode @ ds + c, ;
: mod/reg/rm,   mod/reg/rm @ c, ;
: suffixes,   ?sib, ?disp, ?imm, reset ;

: 0op   create ,  does> @ c, reset ;
: 1reg   create ,  does> @ >r 2drop r> + c, reset ;
: reg-imm   create ,  does> @ >r 2drop r> + s @ 3 lshift + c, op ?imm, reset ;
: 2op   create ,  does> opcode! op op opcode, mod/reg/rm, suffixes, ;
: 2op-no-d   create ,  does> opcode! op op d off opcode, mod/reg/rm, suffixes, ;
: 2op-no-ds   create ,  does> opcode! op op 0ds opcode, mod/reg/rm, suffixes, ;

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
B0 reg-imm movi,
88 2op mov,
8D 2op-no-ds lea,
90 0op nop,
\ B0 immediate mov to reg8
\ B8 immediate mov to reg8/16
\ A8 test
C3 0op ret,
\ C6/0 immediate mov to r/m
\ C7/0 immediate mov to r/m
\ E8 call,
\ E9 jmp,
F0 0op lock,
F2 0op rep,
F3 0op repz,

: #   ['] imm-op -1 ;
: )   2drop ['] ind -1  0reg ;
: )#   2drop  ['] ind-off -1  0reg ;

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
   10203040 eax mov,        8B 05 40 30 20 10  check  \ A1 40 30 20 10
   edi 10203040 mov,        89 3D 40 30 20 10  check

   42 # al movi,            B0 42  check
   42 # ax movi,            66 B8 42 00  check
   42 # eax movi,           B8 42 00 00 00  check
 \ 42 # eax ) movi,         \ C7 00 42 00 00 00  check

   42 # al add,             82 C0 42  check  \ 04 42
   42 # bh add,             82 C7 42  check
   42 # cx add,             66 83 C1 42  check
   42 # edx add,            83 C2 42  check
   10203040 # esi add,      81 C6 40 30 20 10  check
   \ 10203040 # eax add,    \ 05 42 40 30 20 10

   eax ebx ) test,          85 03  check
   eax ) ebx xchg,          87 18  check
   eax push,                50  check
   ebx pop,                 5B  check
   ret,                     C3  check
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
