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
variable r2
variable dir?
variable disp   defer ?disp,
variable imm    defer ?imm,
defer imm,
defer reg
defer !size

\ Set opcode.  And destination: register or memory.
: opcode!   3@ drop >r opcode ! ;
: !mem   dir? @ if 100 d ! then dir? off ;
: !reg   dir? off ;

: reg@   opcode 0E00 @bits ;
: reg!   opcode 0E00 !bits ;
: ea@   opcode 003F @bits ;
: ea!   r2 @ if ea@ 9 lshift reg! then  opcode 003F !bits ;
: size!   opcode 00C0 !bits ;

\ Access instruction fields.
: opmode   d @ ;
: opcode@   opcode @ ;
: imm@   imm @ ;
: disp@   disp @ ;

\ Possibly use a cross-compiling vocabulary to access a target image.
previous

\ Write instruction fields to memory.
: h,   dup 8 rshift c, c, ;
: w,   dup 10 rshift h, h, ;
: opcode,   opcode@ opmode + h, ;
: imm16,   imm@ h, ;
: imm32,   imm@ w, ;
: disp16,   disp@ h, ;
: disp32,   disp@ w, ;
: -pc   here negate ;

also forth

\ Set immediate operand.
: -imm   ['] noop is ?imm, ;
: !imm16   ['] imm16, is imm, ;
: !imm32   ['] imm32, is imm, ;
: imm!   imm !  ['] imm, is ?imm, ;

\ Set operand size.
: !b   0000 size!  !imm16 ;
: !w   0040 size!  !imm16 ;
: !l   0080 size!  !imm32 ;
: .b   ['] !b is !size ;
: .w   ['] !w is !size ;
: .l   ['] !l is !size ;
: default-size .w ;

\ Set displacement.
: disp!   is ?disp, disp ! ;
: !disp16   ['] disp16, disp! ;
: !disp32   ['] disp32, disp! ;
: relative    0 ea!  disp@ -pc + 2 - !disp16 ;

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
: 0reg   ['] reg1 is reg ;
: 0disp   ['] noop is ?disp, ;
: 0imm   imm off  -imm  0 is imm, ;
: 0size   ['] default-size is !size ;
: 0opmode   d off 0size ;
: 0asm   0imm 0disp 0reg 0opmode  dir? on ;

\ Implements addressing mode: immediate.
: imm-op   imm! ;

\ Process one operand.  All operands except a direct address
\ have the stack picture ( n*x xt -addr ).
: addr?   dup -addr <> ;
: op   addr? if addr else drop execute then ;

\ Process the count operand to a shift instruction .
: reg?   dup ['] reg = ;
: reg-or-immediate   reg? if 2drop else execute then ;
: shift-op   addr? abort" Syntax error" drop  reg-or-immediate ;

\ Define instruction formats.
: instruction, ( a -- ) opcode! !size opcode, ?imm, ?disp, 0asm ;
: mnemonic ( u a "name" -- ) create ['] noop 3,  does> instruction, ;
: format:   create ] !csp  does> mnemonic ;
: immediate:   ' latestxt >body ! ;

\ Instruction formats.
format: 0op ;
format: 1op   r2 off op d off ;
format: 2opi   r2 off op op d off ;
format: 2op   r2 on op op ;
format: 2op-d   r2 on op op d off ;
format: branch   op relative ;
format: imm   2drop opcode +! ;

\ Define registers.
: reg:   create dup 000F and , 1+  does> @ ['] reg -addr ;

\ Instruction mnemonics.
previous also assembler definitions

\ 0000 move,
\ 0040 movea,
0000 2opi ori,
0200 2opi andi,
0400 2opi subi,
0600 2opi addi,
0A00 2opi eori,
0C00 2opi cmpi,
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
\ 50C8 dbcc
6000 branch bra,
6100 branch bsr,
6200 branch bhi,
6300 branch bls,
6400 branch bcc,
6500 branch bcs,
6600 branch bne,
6700 branch beq,
6800 branch bvc,
6900 branch bvs,
6A00 branch bpl,
6B00 branch bmi,
6C00 branch bge,
6D00 branch blt,
6E00 branch bgt,
6F00 branch ble,
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
\ E000 asr
\ E008 lsr
\ E010 roxl
\ E018 rol
\ E100 asl
\ E108 lsl
\ E110 roxr
\ E118 ror

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
