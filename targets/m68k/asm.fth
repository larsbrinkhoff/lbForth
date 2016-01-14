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
\ 0010 = MOVE.W, MOVEA.L
\ 0011 = MOVE.L, MOVEA.W
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
variable dir?
variable disp   defer ?disp,
variable imm    defer ?imm,
defer imm,
defer immediate-opcode
defer reg
defer ?opsize

\ Set opcode.  And destination: register or memory.
: opcode!   3@ is immediate-opcode >r opcode ! ;
: !reg   dir? @ if 100 d ! then dir? off ;
: !mem   dir? off ;

\ Set bits in mod/reg/rm byte.
: mod!   ;
: reg@   opcode 0E00 @bits ;
: reg!   opcode 0E00 !bits ;
: ea@   opcode 7 @bits ;
: ea!   ea@ 9 lshift reg!  opcode 7 !bits ;
: reg>opcode   ea@ opcode 07 !bits ;

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
: opcode,   opcode@ opmode + h, ;
: imm8,   imm@ c, ;
: imm16,   imm@ h, ;
: imm32,   imm@ w, ;
: disp8,   disp@ c, ;
: disp32,   disp@ w, ;
: -pc   here negate ;

also forth

\ Set immediate operand.
: -imm   ['] noop is ?imm, ;
: !imm8   ['] imm8, is ?imm, ;
: !imm16   ['] imm16, is ?imm, ;
: !imm32   ['] imm32, is ?imm, ;
: imm!   imm !  ['] imm, is ?imm, ;

\ Set operand size.
: -opsize   2drop r> drop ;
: opsize!   is imm,  s !  ['] -opsize is ?opsize ;
: !op8    0 ['] imm8, ?opsize ;
: !op32   80 ['] imm32, ?opsize ;
: !op16   40 ['] imm16, ?opsize ;

\ Set displacement.
: byte?   -80 80 within ;
: disp!   is ?disp, disp ! ;
: !disp8   ['] disp8, disp! ;
: !disp32   ['] disp32, disp! ;
: !disp ( a -- u ) dup byte? if !disp8 40 else !disp32 80 then ;
: short-addr   -pc 2 - disp +!  ['] disp8, is ?disp, ;
: near-addr   -pc 5 - disp +! ;

\ Implements addressing modes: register, indirect, indexed, and direct.
: reg1   ea! !reg ;
: reg2   9 lshift reg! ;
: !reg2   ['] reg2 is reg ;
: ind   dup mod! ea! !mem !reg2 ;
: ind#   swap !disp + ind ;
: idx   04 ind drop ;
: idx#   rot !disp 04 + ind drop ;
: addr   !disp32  05 ind ;

\ Reset assembler state.
: 0opsize   ['] opsize! is ?opsize ;
: 0opmode   d off  s off ;
: 0reg   ['] reg1 is reg ;
: 0disp   ['] noop is ?disp, ;
: 0imm   imm off  -imm  0 is imm, ;
: 0asm   0imm 0disp 0reg 0opmode 0opsize  dir? on ;

\ Implements addressing mode: immediate.
: imm8?   imm @ byte? ;
: ?sign-extend   d off  imm8? if 100 d !  !imm8 then ;
: alu#   opcode @ reg! 80 opcode ! ?sign-extend ;
: mov#   B0 s @ 3 lshift + ea@ + opcode ! 0opmode ;
: push#   imm8? if !imm8 6A else !imm32 68 then dup opcode ! ea! ;
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
format: 1reg   op reg>opcode 0opmode ;
format: 1op   op ;
format: 2op   op op ;
format: 2op-d   op op d off ;
format: short   op short-addr ;
format: near   op near-addr ;
format: 1imm8   !op8 op ;

\ Define registers.
: reg:   create dup 7 and , 1+  does> @ ['] reg -addr ;

\ Instruction mnemonics.
previous also assembler definitions
4200 1op clr,
4A7C 0op illegal,
4E71 0op nop,
4E73 0op rts,
4E81 0op trap1,
C000 2op and,
D000 2op add,
\ D0C0 2op adda, .w
\ D1C0 2op adda, .l

\ Addressing mode syntax: immediate, indirect, and displaced indirect.
: #   ['] imm-op -addr ;
: )   2drop ['] ind -addr  0reg 0opsize ;
: )#   2drop ['] ind# -addr  0reg 0opsize ;
\ )+   2drop ['] postdec  0reg 0opsize ;
\ -)   2drop ['] preinc  0reg 0opsize ;

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
