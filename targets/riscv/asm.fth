\ Copyright 2017 Lars Brinkhoff

\ Assembler for RISC-V.

\ Adds to FORTH vocabulary: ASSEMBLER CODE ;CODE.
\ Creates ASSEMBLER vocabulary with: END-CODE and RISC-V opcodes.

\ This will become a cross assembler if loaded with a cross-compiling
\ vocabulary at the top of the search order.

require search.fth
also forth definitions
require lib/common.fth

vocabulary assembler

base @  hex

\ This constant signals that an operand is not a direct address.
deadbeef constant -addr

\ Assembler state.
variable opcode
defer imm-op
defer reg
defer !offset

\ Set opcode.  And instruction fields.
: opcode!   3@ drop >r opcode ! ;
: rd!   007 lshift opcode 00000F80 !bits ;
: rs1!  00F lshift opcode 000F8000 !bits ;
: rs2!  014 lshift opcode 01F00000 !bits ;
: shift!  014 lshift opcode 07F00000 !bits ;
: imm!   014 lshift opcode FFF00000 !bits ;
: >liu   dup 0800 and if 01000 + then ;
: u-type!   >liu opcode FFFFF000 !bits ;
: store-offset!   dup rd!  014 lshift opcode FE000000 !bits ;
: jump-offset!   dup 014 lshift opcode 7FE00000 !bits
                 dup 009 lshift opcode 00100000 !bits
                 dup            opcode 000FF000 !bits
                     00B lshift opcode 80000000 !bits ;
: branch-offset!   dup 007 lshift opcode 00000F00 !bits
                   dup 014 lshift opcode 7E000000 !bits
                   dup 004 rshift opcode 00000080 !bits
                       013 lshift opcode 80000000 !bits ;

\ Access instruction fields.
: opcode@   opcode @ ;

\ Possibly use a cross-compiling vocabulary to access a target image.
previous definitions

\ Write instruction fields to memory.
: h,   dup c, 8 rshift c, ;
: w,   dup h, 10 rshift h, ;
: h@   dup c@ swap 1+ c@ 8 lshift + ;
: h!   2dup c!  swap 8 rshift  swap 1+ c! ;
: w@   dup h@ swap 2 + h@ 10 lshift + ;
: w!   2dup h!  swap 10 rshift  swap 2 + h! ;
: opcode,   opcode@ w, ;
: pc-   here - ;
: branch!   swap FE000F80 and  over w@ 01FFF07F and + swap w! ;

also forth definitions

\ Set displacement.
: relative   pc- !offset ;

\ Implements addressing modes: register.
: !reg4   ['] rs1! is reg ;
: reg3   rs2! !reg4 ;
: !reg3   ['] reg3 is reg ;
: reg2   rs1! !reg3 ;
: !reg2   ['] reg2 is reg ;
: reg1   rd! !reg2 ;
: ind#   rs1! !offset ;
: addr   ;

\ Set offset field.
: !shift#   ['] shift! is imm-op ;
: !store   ['] store-offset! is !offset  !reg3 ;
: !jump   ['] jump-offset! is !offset ;
: !branch   ['] branch-offset! is !offset  !reg2 ;
: !u-type   ['] u-type! is imm-op ;

\ Reset assembler state.
: 0reg   ['] reg1 is reg ;
: 0imm   ['] imm! is imm-op ;
: 0offset   ['] imm! is !offset ;
: 0asm   0reg 0imm 0offset ;

\ Process one operand.  All operands except a direct address
\ have the stack picture ( n*x xt -addr ).
: addr?   dup -addr <> ;
: op   addr? if addr else drop execute then ;

\ Define instruction formats.
: instruction, ( a -- ) opcode! opcode, 0asm ;
: mnemonic ( u a "name" -- ) create ['] noop 3,  does> instruction, ;
: format:   create ] !csp  does> mnemonic ;

\ Instruction formats.
format: 0op ;
format: 3op   op op op ;
format: shift   !shift# op op op ;
format: load   op op ;
format: store   !store op op ;
format: jump   !jump op relative ;
format: branch   !branch op op relative ;
format: u-type   !u-type op op ;

\ Define registers, condition codes, and shifts.
: reg:   create dup 001F and , 1+  does> @ ['] reg -addr ;

\ Instruction mnemonics.
previous also assembler definitions

00000003 load lb,
00001003 load lh,
00002003 load lw,
00003003 load ld,
00004003 load lbu,
00005003 load lhu,
00006003 load lwu,
00000013 3op addi,
00000073 0op ecall,
00001013 shift slli,
00002013 3op slti,
00003013 3op sltui,
00004013 3op xori,
00005013 shift srli,
40005013 shift srai,
00006013 3op ori,
00007013 3op andi,
00000033 3op add,
40000033 3op sub,
00001033 3op sll,
00002033 3op slt,
00003033 3op sltu,
00004033 3op xor,
00005033 3op srl,
40005033 3op sra,
00006033 3op or,
00007033 3op and,
02000033 3op mul,
02001033 3op mulh,
02002033 3op mulhsu,
02003033 3op mulhu,
02004033 3op div,
02005033 3op divu,
02006033 3op rem,
02007033 3op remu,
\ 00000017 u-type auipc,
00000023 store sb,
00001023 store sh,
00002023 store sw,
00003023 store sd,
\ 0000002F	amo,
00000037 u-type lui,
00000063 branch beq,
00001063 branch bne,
00004063 branch blt,
00005063 branch bge,
00006063 branch bltu,
00007063 branch bgeu,
00000067 load jalr,
0000006F jump jal,

\ Addressing mode syntax: immediate, indirect, and indexed.
: #   ['] imm-op -addr ;
: )   2drop 0 swap ['] ind# -addr ;
: )#   2drop ['] ind# -addr ;

\ Register names.
0
reg: x0  reg: x1  reg: x2  reg: x3  reg: x4  reg: x5  reg: x6  reg: x7
reg: x8  reg: x9  reg: x10  reg: x11  reg: x12  reg: x13  reg: x14  reg: x15
reg: x16  reg: x17  reg: x18  reg: x19  reg: x20  reg: x21  reg: x22  reg: x23
reg: x24  reg: x25  reg: x26  reg: x27  reg: x28  reg: x29  reg: x30  reg: x31
drop

create tmp1  3 cells allot
create tmp2  3 cells allot

\ Aliases.
: nop,   0 # x0 x0 addi, ;
: mv,   tmp1 3! tmp2 3! 0 # tmp2 3@ tmp1 3@ addi, ;
: neg,   tmp1 3! x0 tmp1 3@ sub, ;
: not,   tmp1 3! tmp2 3! 0FFF # tmp2 3@ tmp1 3@ xori, ;
: li,   tmp1 3! x0 tmp1 3@ addi, ;
: j,   x0 jal, ;
: jr,   x0 jalr, ;
: ret,   x1 ) jr, ;

\ Resolve jumps.
: >mark   here 4 - ;
: >resolve   here over - branch-offset!  opcode@ swap branch! ;

\ Unconditional jumps.
: label   here >r get-current ['] assembler set-current r> constant set-current ;
: bra,   x0 x0 beq, ;
: begin,   here ;
: again,   bra, ;
: ahead,   0 bra, >mark ;
: then,   >resolve ;

\ Conditional jumps.
: =,   ['] bne, ;
: <,   ['] bge, ;
: <>,   ['] beq, ;
: if,   2>r 2>r 2>r >r 0 r> 2r> 2r> 2r> execute >mark ;
: until,   execute ;

\ else,   ahead, 3swap then, ;
: while,   >r if, r> ;
: repeat,   again, then, ;

\ Runtime for ;CODE.  CODE! is defined elsewhere.
: (;code)   r> code! ;

\ Enter and exit assembler mode.
: start-code   also assembler 0asm ;
: end-code     align previous ;

also forth base ! previous

previous definitions also assembler

\ Standard assembler entry points.
: code    parse-name header, ?code, reveal start-code  ;
: ;code   postpone (;code) reveal postpone [ ?csp start-code ; immediate

0asm
previous
