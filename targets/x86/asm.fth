\ Copyright 2013-2014 Lars Brinkhoff

\ Assembler for x86.

\ Adds to FORTH vocabulary: ASSEMBLER CODE ;CODE.
\ Creates ASSEMBLER vocabulary with: END-CODE and x86 opcodes.

\ Conventional prefix syntax: "<source> <destination> <opcode>,".
\ Addressing modes:
\ - immediate: "n #"
\ - direct: n
\ - register: <reg>
\ - indirect: "<reg> )"
\ - indirect with displacement: "n <reg> )#"
\ - indexed: not supported yet

require lib/comus.fth
require search.fth

vocabulary assembler
also assembler definitions

\ Access to target memory.
' align   defer align is align
' here   defer here  is here
' c,   defer c,    is c,
: h,   dup c,  8 rshift c, ;
: ,   dup h,  16 rshift h, ;

base @  hex

\ Assembler state.
variable opcode
variable d
variable s
variable dir?
variable mrrm   defer ?mrrm,
variable sib    defer ?sib,
variable disp   defer ?disp,
variable imm    defer ?imm,
variable 'imm
defer immediate-opcode
defer reg

\ Set opcode.  And destination: register or memory.
: opcode!   3@ is immediate-opcode >r opcode ! ;
: !reg   dir? @ if 2 d ! then dir? off ;
: !mem   dir? off ;

\ Set bits in mod/reg/rm byte.
: -mrrm   ['] nop is ?mrrm, ;
: mod!   mrrm c0 !bits ;
: reg@   mrrm 38 @bits ;
: reg!   mrrm 38 !bits ;
: rm@   mrrm 7 @bits ;
: rm!   rm@ 3 lshift reg!  mrrm 7 !bits ;
: reg>opcode   rm@ opcode 07 !bits ;
: opcode>reg   opcode @ dup 3 rshift rm!  8 rshift opcode ! ;

\ Write parts of instruction to memory.
: ds   d @ s @ + ;
: ?twobyte   dup FF > if dup 8 rshift c, then ;
: opcode,   opcode @ ?twobyte ds + c, ;
: mrrm,   mrrm @ c, ;
: sib,   sib @ c, ;
: imm8,   imm @ c, ;
: disp8,   disp @ c, ;
: imm16,   imm @ h, ;
: imm32,   imm @ , ;
: disp32,   disp @ , ;

\ Set operand size.
: !op8    0 s !  ['] imm8, 'imm ! ;
: !op32   'imm @ 0= if 1 s !  ['] imm32, 'imm ! then ;
: !op16   'imm @ 0= if 1 s !  ['] imm16, 'imm ! 66 c, then ;

\ Set SIB byte.
: !sib   ['] sib, is ?sib, ;
: sib!   3 lshift + sib !  !sib ;

\ Set displacement.
: byte?   -81 80 rot within ;
: disp!   is ?disp, disp ! ;
: !disp8   ['] disp8, disp! ;
: !disp32   ['] disp32, disp! ;
: !disp ( a -- u ) dup byte? if !disp8 40 else !disp32 80 then ;

\ Set immediate operand.
: imm!   imm !  'imm @ is ?imm, ;

\ Implements addressing modes: register, indirect, indexed, and direct.
: reg1   rm! !reg ;
: reg2   3 lshift reg! ;
: ind   dup mod! rm! !mem  ['] reg2 is reg ;
: ind#   swap !disp + ind ;
: idx   04 ind  sib! ;
: idx#   rot !disp 04 + ind  sib! ;
: addr   !disp32  05 ind ;

\ Reset assembler state.
: 0ds   d off  s off ;
: 0reg   ['] reg1 is reg ;
: 0mrrm   c0 mrrm !  ['] mrrm, is ?mrrm, ;
: 0sib   ['] nop is ?sib, ;
: 0disp   ['] nop is ?disp, ;
: 0imm   imm off  ['] nop is ?imm,  'imm off ;
: 0asm   0imm 0disp 0reg 0ds 0mrrm 0sib  dir? on ;

\ Enter and exit assembler mode.
: start-code   also assembler 0asm ;
: end-code     align previous ;

\ Implements addressing mode: immediate.
: ?sign-extended   d off  imm @ byte? if 2 d !  ['] imm8, is ?imm, then ;
: alu#   opcode @ reg! 80 opcode ! ?sign-extended ;
: mov#   B0 s @ 3 lshift + rm@ + opcode ! 0ds -mrrm ;
: imm-op   imm! immediate-opcode ;

\ Process one operand.  All operands except a direct address
\ have the stack picture ( n*x xt -1 ).
: addr?   dup -1 <> ;
: op   addr? if addr else drop execute then ;

\ Write opcode and all applicable instruction suffixes to memory.
: instruction,   opcode! opcode, ?mrrm, ?sib, ?disp, ?imm, 0asm ;

\ Instruction formats.
: mnemonic ( u a "name" -- ) create ['] nop 3,  does> instruction, ;
: format:   create ] !csp  does> mnemonic ;
format: 0op   -mrrm ;
format: 1reg   op 0ds reg>opcode -mrrm ;
format: 1op   opcode>reg op d off ;
format: 2op   op op ;
format: 2op-d   op op d off ;
format: 2op-ds   op op 0ds ;
format: 1addr   op -mrrm ;

: immediate:   ' latestxt >body ! ;

\ Instruction mnemonics.
00 2op add,  immediate: alu#
08 2op or,   immediate: alu#
0F44 2op-ds cmove,  \ Todo: other condition codes.
0FB6 2op-ds movzx,
0FBE 2op-ds movsx,
10 2op adc,  immediate: alu#
18 2op sbb,  immediate: alu#
20 2op and,  immediate: alu#
26 0op es,
28 2op sub,  immediate: alu#
2E 0op cs,
30 2op xor,  immediate: alu#
36 0op ss,
38 2op cmp,  immediate: alu#
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
84 2op-d test,
86 2op-d xchg,
88 2op mov,  immediate: mov#
8D 2op-ds lea,
\ 8F/0 pop, rm
90 0op nop,
\ B0 immediate mov to reg8
\ B8 immediate mov to reg8/16
\ A8 immediate test
C3 0op ret,
\ C6/0 immediate mov to r/m
\ C7/0 immediate mov to r/m
E8 1addr call,
E9 1addr jmp,
\ EB jmp rel8
F0 0op lock,
F2 0op rep,
F3 0op repz,
F4 0op hlt,
F5 0op cmc,
F610 1op not,
F618 1op neg,
F8 0op clc,
F9 0op stc,
FA 0op cli,
FB 0op sti,
FC 0op cld,
FD 0op std,
\ FE 0 inc rm
\ FF 1 dec rm
\ FF 2 call rm
\ FF 4 jmp rm
\ FF 6 push rm

: sp?   dup 4 = ;

\ Addressing mode syntax: immediate, indirect, and displaced indirect.
: #   ['] imm-op -1 ;
: )   2drop  sp? if 4 ['] idx else ['] ind then -1  0reg ;
: )#   2drop  sp? if 4 ['] idx# else ['] ind# then -1  0reg ;

\ Define registers.
: reg8    create ,  does> @ ['] reg -1 !op8 ;
: reg16   create ,  does> @ ['] reg -1 !op16 ;
: reg32   create ,  does> @ ['] reg -1 !op32 ;
: reg:    dup reg8 dup reg16 dup reg32 1+ ;

\ Register names.
0
reg: al ax eax   reg: cl cx ecx   reg: dl dx edx   reg: bl bx ebx
reg: ah sp esp   reg: ch bp ebp   reg: dh si esi   reg: bh di edi
drop

\ Runtime for ;CODE.  CODE! is defined elsewhere.
: (;code)      r> code! ;

base !  only forth definitions  also assembler

\ Standard assembler entry points.
: code    create  latestxt >body code!  start-code  ;
: ;code   postpone (;code) reveal postpone [ ?csp start-code ; immediate

0asm
previous
