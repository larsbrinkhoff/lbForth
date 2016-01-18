\ Generic instruction format:
\ oooo rrr d ss mmm rrr
\ oooo = opcode
\ d = direction: 0 = do reg, 1 = to mem
\ ss = size: 00 = byte, 01 = word, 10 = long
\ mmm = mode
\ rrr = register
\
\ Immediate instructions:
\ 0000 oooo ss mmm rrr
\ oooo = opcode
\        rrr1 BTST/BCHG/BCLR/BSET
\        0000 ORI
\        0010 ANDI
\        0100 SUBI
\        0110 ADDI
\        100000 BTST
\        100001 BCHG
\        100010 BCLR
\        100011 BTST
\        1010 EORI
\        1100 CMPI
\        1010 EORI
\        1110 MOVES

\ Addressing modes:
\ 000rrr = Dn
\ 001rrr = An
\ 010rrr = (An)
\ 011rrr = (An)+
\ 100rrr = -(An)
\ 101rrr = (disp16,An)
\ 110rrr = (disp8,An,Xn.Size*Scale)
\ 111000 = absolute short
\ 111001 = absolute long
\ 111010 = (disp16,PC)
\ 111100 = immediate

\ Opcodes:
\ 0000 = bit/MOVEP/immediate
\ 0001 = MOVE.B, 
\ 0010 = MOVE.L, MOVEA.L
\ 0011 = MOVE.W, MOVEA.W
\ 0100 = misc
\ 0101 = ADDQ/SUBQ/Scc/DBcc/TRAPcc
\ 0110 = Bcc/BSR/BRA
\ 0111 = MOVEQ
\ 1000 = OR/DIV/SBCD
\ 1001 = SUB/SUBX
\ 1010 = reserved
\ 1011 = CMP/EOR
\ 1100 = AND/MUL/ABCD/EXG
\ 1101 = ADD/ADDX
\ 1110 = shift/rotate/bit
\ 1111 = coprocessor/extensions

\ R1 OP,
\ N # R1 OP,
\ R1 R2 OP,	R1 -> EA, R2 -> R
\ MEM R1 OP,
\ R1 MEM OP,


\ Copyright 2016 Lars Brinkhoff

\ Assembler for Motorola 68000 and ColdFire.

\ Adds to FORTH vocabulary: ASSEMBLER CODE ;CODE.
\ Creates ASSEMBLER vocabulary with: END-CODE and 68000 opcodes.

\ This will become a cross assembler if loaded with a cross-compiling
\ vocabulary at the top of the search order.

\ Conventional prefix syntax: "<source> <destination> <opcode>,".
\ Addressing modes:
\ - immediate: "n #"
\ - absolute: n
\ - register: <reg>
\ - indirect: "<reg> )"
\ - posticrement: "<reg> )+"
\ - predecrement: "<reg> -)"
\ - indirect with displacement: "n <reg> )#"
\ - indexed: not supported yet
\ - pc relative: not supported yet

require search.fth
also forth definitions
require lib/common.fth

vocabulary assembler

base @  hex

\ This constant signals that an operand is not a direct address.
deadbeef constant -addr

\ Assembler state.
variable opcode
variable d
variable s
variable r2
variable dir?
variable disp   defer ?disp,
variable imm    defer ?imm,
defer imm,
defer immediate-opcode
defer reg

\ Set opcode.  And destination: register or memory.
: opcode!   3@ is immediate-opcode >r opcode ! ;
: !mem   dir? @ if 100 d ! then dir? off ;
: !reg   dir? off ;

: reg@   opcode 0E00 @bits ;
: reg!   opcode 0E00 !bits ;
: ea@   opcode 003F @bits ;
: ea!   r2 @ if ea@ 9 lshift reg! then  opcode 003F !bits ;

0 value 'op

\ Access instruction fields.
: opmode   d @ s @ + ;
: opcode@   opcode @ ;
: imm@   imm @ ;
: disp@   disp @ ;

\ Possibly use a cross-compiling vocabulary to access a target image.
previous

\ Write instruction fields to memory.
: h,   dup 8 rshift c, c, ;
: w,   dup 10 rshift h, h, ;
: opcode,   here 1+ to 'op  opcode@ opmode + h, ;
: imm8,   imm@ c, ;
: imm16,   imm@ h, ;
: imm32,   imm@ w, ;
: disp16,   disp@ h, ;
: disp32,   disp@ w, ;
: -pc   here negate ;

\ Set operand size.
: .b   'op c@ 003F and  0000 + 'op c! ;
: .w   'op c@ 003F and  0040 + 'op c! ;
: .l   'op c@ 003F and  0080 + 'op c! ;

also forth

\ Set immediate operand.
: -imm   ['] noop is ?imm, ;
: !imm8   ['] imm8, is ?imm, ;
: !imm16   ['] imm16, is ?imm, ;
: !imm32   ['] imm32, is ?imm, ;
: imm!   imm !  ['] imm, is ?imm, ;

\ Set displacement.
: disp!   is ?disp, disp ! ;
: !disp16   ['] disp16, disp! ;
: !disp32   ['] disp32, disp! ;
: near-addr   -pc 5 - disp +! ;

\ Implements addressing modes: register, indirect, postincrement,
\ predecrement, and absolute.
: reg3   9 lshift reg! ;
: reg2   ea! ;
: !reg2   ['] reg2 is reg ;
: !reg3   ['] reg3 is reg ;
: reg1   ea! !reg !reg2 ;
: ind   0018 xor ea! !mem !reg3 ;
: ind+   0008 xor ind ;
: ind-   0030 + ind ;
: ind#   swap !disp16  0038 xor ind ;
: addr   !disp32  0039 ea! ;

\ Reset assembler state.
: 0opmode   d off  s off ;
: 0reg   ['] reg1 is reg ;
: 0disp   ['] noop is ?disp, ;
: 0imm   imm off  -imm  0 is imm, ;
: 0asm   0imm 0disp 0reg 0opmode  dir? on ;

\ Implements addressing mode: immediate.
: alu#   opcode @ reg! 80 opcode ! ;
: mov#   B0 s @ 3 lshift + ea@ + opcode ! 0opmode ;
: test#   F6 opcode ! ;
: shift#   -12 opcode +!  imm @ 1 = if 10 opcode +! -imm else !imm8 then ;
: imm-op   imm! immediate-opcode ;

\ Process one operand.  All operands except a direct address
\ have the stack picture ( n*x xt -addr ).
: addr?   dup -addr <> ;
: op   addr? if addr else drop execute then ;

\ Process the count operand to a shift instruction .
: reg?   dup ['] reg = ;
: reg-or-immediate   reg? if 2drop else execute then ;
: shift-op   addr? abort" Syntax error" drop  reg-or-immediate ;

\ Define instruction formats.
: instruction,   opcode! opcode, ?disp, ?imm, 0asm ;
: mnemonic ( u a "name" -- ) create ['] noop 3,  does> instruction, ;
: format:   create ] !csp  does> mnemonic ;
: immediate:   ' latestxt >body ! ;

\ Instruction formats.
format: 0op ;
format: 1op   r2 off op d off ;
format: 2op   r2 on op op ;
format: 2op-d   op op d off ;
format: near   op near-addr ;
format: imm   2drop opcode +! ;

\ Define registers.
: reg:   create dup 000F and , 1+  does> @ ['] reg -addr ;

\ Instruction mnemonics.
previous also assembler definitions

\ 0000 move,
\ 0000 ori,
\ 0040 movea,
\ 0200 andi,
\ 0400 subi,
\ 0600 addi,
\ 0A00 eori,
\ 0C00 eori,
0100 2op btst,
0140 2op bchg,
0180 2op bclr,
01C0 2op bset,
\ 2000 movea,
4000 1op negx,
41C0 2op-d lea,
4200 1op clr,
4400 1op neg,
4600 1op not,
\ 4800 ext,
4840 1op pea,
4840 1op swap,
4848 imm bkpt,
4A00 1op tst,
4A7C 0op illegal,
4AC0 1op tas,
\ 4E58 unlk,
4E70 0op reset,
4E71 0op nop,
\ 4E72 stop,
4E73 0op rte,
4E75 0op rts,
4E76 0op trapv,
4E77 0op rtr,
4E80 1op jsr,
4E40 imm trap,
4EC0 1op jmp,
8000 2op or,
9000 2op sub,
B000 2op cmp,
B000 2op eor,
C000 2op and,
C0C0 2op mulu,
C1C0 2op muls,
D000 2op add,
\ D0C0 2op adda, .w
\ D1C0 2op adda, .l

\ Addressing mode syntax: immediate, indirect, and displaced indirect.
: #   ['] imm-op -addr ;
: )   2drop ['] ind -addr  0reg ;
: )+   2drop ['] ind+ -addr  0reg ;
: -)   2drop ['] ind- -addr  0reg ;
: )#   2drop ['] ind# -addr  0reg ;

\ Register names.
0
reg: d0  reg: d1  reg: d2  reg: d3  reg: d4  reg: d5  reg: d6  reg: d7
reg: a0  reg: a1  reg: a2  reg: a3  reg: a4  reg: a5  reg: a6  reg: a7
drop

\ Resolve jumps.
: >mark1   here 1- ['] c! here ;
: >mark4   here 4 - ['] ! here ;
: >resolve   here - negate -rot execute ;

(*
\ Unconditional jumps.
: label   here >r get-current ['] assembler set-current r> constant set-current ;
: begin,   here ;
: again,   bra, ;
: ahead,   0 bra, >mark4 ;
: then,   >resolve ;

\ Conditional jumps.
: 0=,   ['] bne, ;
: 0<,   ['] bge, ;
: 0<>,   ['] beq, ;
: if,   0 swap execute >mark1 ;
: until,   execute ;

\ else,   ahead, 3swap then, ;
: while,   >r if, r> ;
: repeat,   again, then, ;
*)

\ Runtime for ;CODE.  CODE! is defined elsewhere.
: (;code)   r> code! ;

\ Enter and exit assembler mode.
: start-code   also assembler 0asm ;
: end-code     align previous ;

base !

previous definitions also assembler

\ Standard assembler entry points.
: code    parse-name header, ?code, reveal start-code  ;
: ;code   postpone (;code) reveal postpone [ ?csp start-code ; immediate

0asm
previous
