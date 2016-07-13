\ Copyright 2016 Lars Brinkhoff

\ Assembler for ARM.

\ Adds to FORTH vocabulary: ASSEMBLER CODE ;CODE.
\ Creates ASSEMBLER vocabulary with: END-CODE and ARM opcodes.

\ This will become a cross assembler if loaded with a cross-compiling
\ vocabulary at the top of the search order.

\ Conventional prefix syntax: "<source> <destination> <opcode>,".
\ Addressing modes:
\ - immediate: "n #"
\ - absolute: n
\ - register: <reg>
\ - indirect: "<reg> )"
\ - pre-indexed with offset: "n <reg> )#"
\ - pre-indexed with offset and update: "n <reg> )#!"
\ - post-indexed with update: "n <reg> #)"

require search.fth
also forth definitions
require lib/common.fth

vocabulary assembler

base @  hex

\ This constant signals that an operand is not a direct address.
deadbeef constant -addr

\ Assembler state.
variable opcode
variable condition
variable disp
defer imm-op
defer reg

\ Set opcode.
: opcode!   3@ drop >r opcode ! ;
: rs!   8 lshift opcode 00000F00 !bits  00000010 opcode +! ;
: rd!   0c lshift opcode 0000F000 !bits ;
: rn!   10 lshift opcode 000F0000 !bits ;
: rm!   opcode 0000000F !bits ;
: !w   00200000 opcode +! ;
: !up   00800000 opcode +! ;
: !pre   01000000 opcode +! ;
: !-pre   -01000000 opcode +! ;
: !i   02000000 opcode +! ;
: !shift-type   opcode +! ;
: !shift-amount   7 lshift opcode 00000F80 !bits ;
: !mem-offset   opcode 00000FFF !bits ;
: !offset   2 rshift opcode 00FFFFFF !bits ;
: imm!   opcode 000000FF !bits ;
: !rot   opcode 00000F00 !bits ;
: 0rot   0 !rot ;
: rot+!   opcode 00000F00 @bits 100 - !rot ;

\ Access instruction fields.
: opcode@   opcode @ ;
: disp@   disp @ ;
: cond@   condition @ ;

\ Possibly use a cross-compiling vocabulary to access a target image.
previous definitions

\ Write instruction fields to memory.
: h,   dup c, 8 rshift c, ;
: w,   dup h, 10 rshift h, ;
: h@   dup c@ swap 1+ c@ 8 lshift + ;
: h!   2dup c!  swap 8 rshift  swap 1+ c! ;
: w@   dup h@ swap 2 + h@ 10 lshift + ;
: w!   2dup h!  swap 10 rshift  swap 2 + h! ;
: offset!   dup w@ FF000000 and rot 00FFFFFF and + swap w! ;
: opcode,   opcode@ cond@ + w, ;
: pc-   here - ;

also forth definitions

\ Set displacement.
: relative   pc- 8 - !offset ;

\ Implements addressing modes: register, indirect, postincrement,
\ predecrement, and absolute.
: reg3   rm! ;
: !reg3   ['] reg3 is reg ;
: reg2   rn! !reg3 ;
: !reg2   ['] reg2 is reg ;
: reg1   rd! !reg2 ;
: up?   dup 00000800 and 0= ;
: ?up   up? if !up else negate then ;
: ind#   rn! ?up !mem-offset !pre ;
: ind   0 swap ind# ;
: ind#-post   ind# !-pre ;
: ind#-pre   ind# !w ;
: addr   pc- 8 - 0F ind# ;

\ Implements addressing mode: immediate.
: shift?   dup 0FF u> ;
: shifted-imm   begin shift? while rot+! 2 rshift repeat ;
: (imm)   shifted-imm imm! !i ;

\ Reset assembler state.
: 0reg   ['] reg1 is reg ;
: 0cond   E0000000 condition ! ;
: 0shift   ['] (imm) is imm-op ;
: 0asm   0reg 0cond 0reg 0shift ;

\ Process one operand.  All operands except a direct address
\ have the stack picture ( n*x xt -addr ).
: addr?   dup -addr <> ;
: op   addr? if addr else drop execute then ;

\ Shifted operands.
: shift#   !shift-amount !reg3 ;
: !shift#   ['] shift# is imm-op ;
: reg-shift   rs! !reg3 ;
: !reg-shift   ['] reg-shift is reg ;
: shifted-reg   !shift-type !shift# !reg-shift op op ;

\ Fix the adr pseudo-instruction.
: down?   opcode@ 00800000 and 0= ;
: !sub   00400000 opcode +! ;
: ?sub   down? if !sub then ;
: >imm   opcode 00000FFF @bits 0rot shifted-imm imm! ;
: fix-adr   ?sub >imm ;

\ Define instruction formats.
: instruction, ( a -- ) opcode! opcode, 0asm ;
: mnemonic ( u a "name" -- ) create ['] noop 3,  does> instruction, ;
: format:   create ] !csp  does> mnemonic ;
: immediate:   ' latestxt >body ! ;

\ Instruction formats.
format: 0op ;
format: 1reg  !reg3 op ;
format: 2op   op op ;
format: adr   op op !-pre fix-adr ;
format: 3op   op op op ;
format: rd0   !reg2 op op ;
format: rn0   op !reg3 op ;
format: imm   2drop opcode +! ;
format: branch   relative ;

\ Define registers, condition codes, and shifts.
: reg:   create dup 000F and , 1+  does> @ ['] reg -addr ;
: cond:   create dup ,  10000000 +  does> @ condition ! ;
: shift:   create dup ,  20 +  does> @ ['] shifted-reg -addr ;

\ Instruction mnemonics.
previous also assembler definitions

00000000 3op and,
\ 00000090 mul,
\ 00200090 mla,
\ 004000B0 strh,
\ 005000B0 ldrh,
\ 005000D0 ldrsb,
\ 005000F0 ldrsh,
00200000 3op eor,
00400000 3op sub,
00500000 3op subs,
00600000 3op rsb,
00800000 3op add,
00A00000 3op adc,
00C00000 3op sbc,
00E00000 3op rsc,
\ 01000090 swp,
\ 010F0000 mrs,
\ 0128F000 msr,
\ 01400090 swpb,
01100000 rd0 tst,
01200070 imm bkpt,
012FFF10 1reg bx,
012FFF30 1reg blx,
01300000 rd0 teq,
01500000 rd0 cmp,
016F0F10 rn0 clz,
01700000 rd0 cmn,
01800000 3op orr,
01A00000 rn0 mov,
01C00000 3op bic,
01E00000 rn0 mvn,
02000000 adr adr, \ Pseudo, turns into add or sub.
04000000 2op str,
04100000 2op ldr,
04400000 2op strb,
04500000 2op ldrb,
\ 08000000 stmda,
\ 08100000 ldmda,
\ 08800000 stmia,
\ 08900000 ldmia,
\ 09000000 stmdb,
\ 09100000 ldmdb,
\ 09800000 stmib,
\ 09900000 ldmib,
0A000000 branch b,
0B000000 branch bl,
\ 0C000000 stc,
\ 0C100000 ldc,
\ 0E000000 cdp,
\ 0E000010 mcr,
\ 0E100010 mrc,
0F000000 imm svc,

\ Addressing mode syntax: immediate, indirect, and indexed.
: #   ['] imm-op -addr ;
: )   2drop ['] ind -addr ;
: )#   2drop ['] ind# -addr ;
: #)   2drop ['] ind#-post -addr ;
: )#!  2drop ['] ind#-pre -addr ;

\ Register names.
0
reg: r0  reg: r1  reg: r2  reg: r3  reg: r4  reg: r5  reg: r6  reg: r7
reg: r8  reg: r9  reg: r10  reg: r11  reg: r12  reg: sp  reg: lr  reg: pc
drop

\ Condition codes.
0
cond: eq  cond: ne  cond: cs  cond: cc  cond: mi  cond: pl  cond: vs  cond: vc
cond: hi  cond: ls  cond: ge  cond: lt  cond: gt  cond: le  cond: al
drop

\ Shifts
0
shift: lsl  shift: lsr  shift: asr  shift: ror
drop

\ Aliases.
: nop,   r0 r0 mov, ;
: beq,   eq b, ;
: bne,   ne b, ;
: bge,   ge b, ;

\ Resolve jumps.
: >mark   here 4 - ;
: >resolve   dup pc- 2 rshift swap offset! ;

\ Unconditional jumps.
: label   here >r get-current ['] assembler set-current r> constant set-current ;
: begin,   here ;
: again,   b, ;
: ahead,   0 b, >mark ;
: then,   >resolve ;

\ Conditional jumps.
: 0=,   ['] bne, ;
: 0<,   ['] bge, ;
: 0<>,   ['] beq, ;
: if,   0 swap execute >mark ;
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
