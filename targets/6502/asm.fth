\ Copyright 2016 Lars Brinkhoff

\ Assembler for 6502.

\ Adds to FORTH vocabulary: ASSEMBLER CODE ;CODE.
\ Creates ASSEMBLER vocabulary with: END-CODE and 6502 opcodes.

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
variable mode
variable data   defer ?data,
defer absolute,y

\ Set opcode.
: opcode!   3@ drop >r opcode ! ;
: !mode   mode +! ;

\ Access instruction fields.
: opcode@   opcode @ mode @ + ;
: data@   data @ ;

\ Possibly use a cross-compiling vocabulary to access a target image.
previous

\ Write instruction fields to memory.
: w,   dup c,  8 rshift c, ;
: w!   2dup c!  swap 8 rshift  swap 1+ c! ;
: opcode,   opcode@ c, ;
: data8,   data@ c, ;
: data16,   data@ w, ;
: pc-   here - 2 - ;

also forth

\ Set operand data.
: !data8   data !  ['] data8, is ?data, ;
: !data16   data !  ['] data16, is ?data, ;

\ Implements addressing modes.
: zp?   dup 100 < ;
: special#?   opcode@ 03 and 01 = ;
: imm-op   !data8  special#? if 08 else 00 then !mode ;
: absolute   zp? if !data8 04 else !data16 0C then !mode ;
: absolute,x   absolute  10 !mode ;
: (absolute,y)   !data16  18 !mode ;
: indirect   !data16  20 !mode ;
: zeropage,x   !data8  00 !mode ;	
: zeropage,y   !data8  10 !mode ;
: accumulator   08 !mode ;
: relative   pc- !data8 ;

\ Reset assembler state.
: 0data   ['] noop is ?data, ;
: 0modes   0 mode !  ['] (absolute,y) is absolute,y ;
: 0asm   0data 0modes ;

\ Process one operand.  All operands except a direct address
\ have the stack picture ( n*x xt -addr ).
: addr?   dup -addr <> ;
: op   addr? if absolute else drop execute then ;

\ Define instruction formats.
: instruction,   opcode! opcode, ?data, 0asm ;
: mnemonic ( u a "name" -- ) create ['] noop 3,  does> instruction, ;
: format:   create ] !csp  does> mnemonic ;
: immediate:   ' latestxt >body ! ;

\ Instruction formats.
format: 0op ;
format: 1op   op ;
format: special,y   ['] absolute,x is absolute,y  op ;
format: branch   relative ;
format: jump   !data16 ;

\ Instruction mnemonics.
previous also assembler definitions
01 1op ora,
02 1op asl,
10 branch bpl,
20 jump jsr,
20 1op bit,
21 1op and,
22 1op rol,
30 branch bmi,
\ 40 rti,
41 1op eor,
42 1op lsr,
4C jump jmp,
50 branch bvc,
\ 60 rts,
61 1op adc,
62 1op ror,
70 branch bvs,
80 1op sty,
81 1op sta,
82 special,y stx,
90 branch bcc,
A0 1op ldy,
A1 1op lda,
A2 special,y ldx,
B0 branch bcs,
C0 1op cpy,
C1 1op cmp,
C2 1op dec,
D0 branch bne,
E0 1op cpx,
E1 1op sbc,
E2 1op inc,
F0 branch beq,

00 0op brk,
08 0op php,
18 0op clc,
28 0op plp,
38 0op sec,
40 0op rti,
48 0op pha,
58 0op cli,
60 0op rts,
68 0op pla,
78 0op sei,
88 0op dey,
8A 0op txa,
98 0op tya,
9A 0op txs,
A8 0op tay,
AA 0op tax,
B8 0op clv,
BA 0op tsx,
C8 0op iny,
CA 0op dex,
D8 0op cld,
E8 0op inx,
EA 0op nop,
F8 0op sed,

\ 65C02 extensions.
\ 12/32/52/72/92/B2/D2/F2 zp ) ora,/and,/eor,/adc,/sta,/lda,/cmp,/sbc,
\ 7C abs ,x jmp,
\ 34/3C addr ,x bit,
\ 89 # bit,
\ 04/0C tsb,
\ 14/1C trb,
\ 64/74/9C/9C stz,
\ 80 bra,
\ 1A a inc,
\ 3A a dec,
\ 5A phy,
\ 7A ply,
\ DA phx,
\ FA plx,

\ Addressing mode syntax.
: #   ['] imm-op -addr ;
: a   ['] accumulator -addr ;
: ,x   ['] absolute,x -addr ;
: ,y   ['] absolute,y -addr ;
: )   20 !mode ;
: ,x)   ['] zeropage,x -addr ;
: ),y   ['] zeropage,y -addr ;

\ Resolve jumps.
: rel!   - negate swap c! ;
: abs!   nip swap w! ;
: >mark1   here 1- here ['] rel! ;
: >mark2   here 2 - here  ['] abs! ;
: >resolve   here swap execute ;

\ Unconditional jumps.
: label   here >r get-current ['] assembler set-current r> constant set-current ;
: begin,   here ;
: again,   jmp, ;
: ahead,   0 jmp, >mark2 ;
: then,   >resolve ;

\ Conditional jumps.
: 0=,   ['] bne, ;
: 0<,   ['] bcs, ;
: 0<>,   ['] beq, ;
: if,   0 swap execute >mark1 ;
: until,   execute ;

\ else,   ahead, 3swap then, ;
: while,   >r if, r> ;
: repeat,   again, then, ;

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
