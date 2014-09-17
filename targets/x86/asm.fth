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

variable ds
variable dir?
variable 'disp
variable disp
variable 'imm
variable imm
variable mod/reg/rm

: !op8   ;
: !op32   ds @ 1 or ds ! ;
: !op16   ds @ 1 and 0= if 66 c, !op32 then ;

: !reg   dir? @ if ds @ 2 or ds ! then dir? off ;
: !mem   dir? off ;

: imm8    imm @ c, ;
: disp8   disp @ c, ;
: imm32   imm @ , ;
: disp32   disp @ , ;

: reset   imm off  ['] nop dup 'disp ! 'imm !  c0 mod/reg/rm !
   ds off  dir? on ;
: start-code   also assembler reset ;

: mod     mod/reg/rm @ c0 and ;
: rm<<3   mod/reg/rm @ 3 and 3 lshift ;
: reg   mod + rm<<3 + mod/reg/rm !  !reg ;
: ind   rm<<3 + mod/reg/rm !  !mem ;
: ind-off   80 + ind ;
: addr   05 + ind ;

: addr?   dup -1 = ;
: op   addr? if drop execute else addr then ;

: ?sib, ;
: ?disp,   'disp perform ;
: ?imm,    'imm perform ;

: mod/reg/rm,   mod/reg/rm @ c, ;
: suffixes,   ?sib, ?disp, ?imm, reset ;
: 2op   create ,  does> @ >r op op r> ds @ + c, mod/reg/rm, suffixes, ;

00 2op add,
08 2op or,
\ cmove, 0f 44
\ movzbl, 0f b6
18 2op sbb,
20 2op and,
28 2op sub,
30 2op xor,
38 2op cmp,
: push,   2drop 50 + c, ;
: pop,   2drop 54 + c, ;
84 2op test,
86 2op xchg,
88 2op mov,
8D 2op lea,
: nop,     90 c, ;
: ret,     c3 c, ;
: call,    e8 c, , ;
: jump,    e9 c, , ;
: repz,    f3 c, ;
\ not, f6 10

: #   ['] nop -1  ['] imm32 'imm ! ;
: )   2drop ['] ind -1 ;
: )#   ['] ind  swap disp ! -1  ['] disp32 'disp ! ;

: reg8    create ,  does> @ ['] reg -1 !op8 ;
: reg16   create ,  does> @ ['] reg -1 !op16 ;
: reg32   create ,  does> @ ['] reg -1 !op32 ;
: reg:    dup reg8 dup reg16 dup reg32 1+ ;

0
reg: al ax eax   reg: cl cx ecx   reg: dl dx edx   reg: bl bx ebx
reg: ah sp esp   reg: ch bp ebp   reg: dh si esi   reg: bh di edi
drop

base !  only forth definitions  also assembler

: code    create  latestxt >body code!  start-code  ;
: ;code   postpone (;code) reveal postpone [ ?csp start-code ; immediate

previous

hex
code test-mov
  eax ebx mov,		\ 8B D8
  ebx ecx mov,		\ 8B CB
  ecx ebx mov,		\ 8B D9
  ecx ) edx mov,	\ 8B 11
  edx ) ecx mov,	\ 8B 0A
  ecx edx ) mov,	\ 89 11
  edx ecx ) mov,	\ 89 0A
end-code
' test-mov >body 10 cdump
decimal

\ : bar   create 42 , ;code  base @ hex
\    8b c, 52 c, 1c c, \ mov 28(%edx),%edx
\    next,
\ end-code  base !
