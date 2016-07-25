\ Copyright 2016 Lars Brinkhoff

\ Assembler for PDP-11.

\ Adds to FORTH vocabulary: ASSEMBLER CODE ;CODE.
\ Creates ASSEMBLER vocabulary with: END-CODE and PDP-11 opcodes.

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
include lib/common.fth

vocabulary assembler

base @

\ This constant signals that an operand is not a direct address.
hex deadbeef constant -addr

\ Assembler state.
variable opcode
variable op#
variable imm
defer ?imm,

octal

\ Set opcode.
: opcode!   3@ drop >r opcode ! ;
: !op   op# @ lshift  opcode 00077 op# @ lshift  !bits   6 op# ! ;
: !offset   1 rshift opcode 00377 !bits ;

\ Access instruction fields.
: opcode@   opcode @ ;
: imm@   imm @ ;

\ Possibly use a cross-compiling vocabulary to access a target image.
previous definitions

decimal

\ Write instruction fields to memory.
: w,   dup c, 8 rshift c, ;
: w!   over 8 rshift over 1+ c!  c! ;
: w@   dup c@ swap 1+ c@ 8 lshift + ;
octal
: offset!   dup w@ 177400 and rot 1 rshift 00377 and + swap w! ;
decimal
: opcode,   opcode@ w, ;
: imm,   imm@ w, ;
: pc-   here - 2 - ;

also forth definitions

\ Set immediate operand.
: -imm   ['] noop is ?imm, ;
: !imm   imm !  ['] imm, is ?imm, ;

octal

\ Implements addressing modes.
: ind   0060 + !op !imm ;
: ind@   0070 + !op !imm ;
: imm-op   0027 !op !imm ;
: addr   0037 !op !imm ;
: pc-rel   0067 !op  pc- !imm ;
: relative   pc- !offset ;

\ Reset assembler state.
: 0op   0 op# ! ;
: 0imm   -imm ;
: 0asm   0imm 0op ;

\ Process one operand.  All operands except a direct address
\ have the stack picture ( n*x xt -addr ).
: addr?   dup -addr <> ;
: op   addr? if addr else drop execute then ;

\ Define instruction formats.
: instruction, ( a -- ) opcode! opcode, ?imm, 0asm ;
: mnemonic ( u a "name" -- ) create ['] noop 3,  does> instruction, ;
: format:   create ] !csp  does> mnemonic ;
: immediate:   ' latestxt >body ! ;

\ Instruction formats.
format: 0op ;
format: 1op   op ;
format: 1reg   2drop opcode +! ;
format: 2op   op op ;
format: 1reg1op   2drop 6 lshift opcode +!  op ;
format: bra   relative ;
format: imm   2drop opcode +! ;

\ Define registers and mode.
: reg:   create dup , 1+  does> @ ['] !op -addr ;

\ Instruction mnemonics.
previous also assembler definitions

000000 0op halt,
000001 0op wait,
000002 0op rti,
000003 0op bpt,
000004 0op iot,
000005 0op reset,
000100 1op jmp,
000200 1reg rts,
000241 0op clc,
000242 0op clv,
000244 0op clz,
000250 0op cln,
000261 0op sec,
000262 0op sev,
000264 0op sez,
000270 0op sen,
000300 1op swab,
000400 bra br,
001000 bra bne,
001400 bra beq,
002000 bra bge,
002400 bra blt,
003000 bra bgt,
003400 bra ble,
004000 1reg1op jsr,
005000 1op clr,
005100 1op com,
005200 1op inc,
005300 1op dec,
005400 1op neg,
005500 1op adc,
005600 1op sbc,
005700 1op tst,
006000 1op ror,
006100 1op rol,
006200 1op asr,
006300 1op asl,
006400 1op mark,
006500 1op mfpi,
006600 1op mfpi,
006700 1op sxt,
010000 2op mov,
020000 2op cmp,
030000 2op bit,
040000 2op bic,
050000 2op bis,
060000 2op add,
\ 070000 1reg1op mul,
\ 070100 1reg1op div,
\ 070200 1reg1op ash,
\ 070300 1reg1op ashc,
\ 070400 1reg1op xor,
\ 070700 1reg1op sob,
100000 bra bpl,
100400 bra bmi,
101000 bra bhi,
101400 bra blos,
102000 bra bvc,
102400 bra bvs,
103000 bra bcc,
103400 bra bcs,
104000 imm emt,
104400 imm trap,
105000 1op clrb,
105100 1op comb,
105200 1op incb,
105300 1op decb,
105400 1op negb,
105500 1op adcb,
105600 1op sbcb,
105700 1op tstb,
106000 1op rorb,
106100 1op rolb,
106200 1op asrb,
106300 1op aslb,
106400 1op mtps,
106500 1op mfpd,
106600 1op mtpd,
106700 1op mfps,
110000 2op movb,
120000 2op cmpb,
130000 2op bitb,
140000 2op bicb,
150000 2op bisb,
160000 2op sub,
\ 170000 float

\ Addressing modes.
\ 000rrr r
\ 001rrr @r					)
\ 010rrr (r)+		010111 #		)+
\ 011rrr @(r)+		011111 absolute		)+@
\ 100rrr -(r)					-)
\ 101rrr @-(r)					-)@
\ 110rrr x(r)		110111 relative		)#
\ 111rrr @x(r)					)#@

\ Addressing mode syntax: immediate, indirect, and displaced indirect.
: #   ['] imm-op -addr ;
: +mode   >r rot r> + -rot ;
: )   0010 +mode ;
: )+   0020 +mode ;
: )+@   0030 +mode ;
: -)   0040 +mode ;
: -)@   0050 +mode ;
: )#   2drop ['] ind -addr ;
: )#@   2drop ['] ind@ -addr ;
: pc)   ['] pc-rel -addr ;

\ Register names.
0
reg: r0  reg: r1  reg: r2  reg: r3  reg: r4  reg: r5  reg: sp  reg: pc
drop

\ Resolve jumps.
: >mark   here 2 - here ;
: >resolve   here - negate swap offset! ;

\ Unconditional jumps.
: label   here >r get-current ['] assembler set-current r> constant set-current ;
: begin,   here ;
: again,   br, ;
: ahead,   0 br, >mark ;
: then,   >resolve ;

\ Conditional jumps.
: 0=,   ['] bne, ;
: 0<,   ['] bge, ;
: 0<>,   ['] beq, ;
: carry,   ['] bcc, ;
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
