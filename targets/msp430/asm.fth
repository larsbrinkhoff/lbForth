\ Copyright 2016 Lars Brinkhoff.

\ Assembler for Texas Instruments MSP430.

\ Adds to FORTH vocabulary: ASSEMBLER CODE ;CODE.
\ Creates ASSEMBLER vocabulary with: END-CODE and MSP430 opcodes.

\ This will become a cross assembler if loaded with a cross-compiling
\ vocabulary at the top of the search order.

\ Conventional prefix syntax: "<source> <destination> <opcode>,".
\ Addressing modes:		Traditional assembler:
\ - immediate: "n #"		#n
\ - relative: n			n
\ - absolute: n &		&n
\ - register: <reg>		Rx
\ - indexed: "n <reg> )#"	n(Rx)
\ - indirect: "<reg> )"		@Rx
\ - postincrement: "<reg> )+"	@Rx+

require search.fth
also forth definitions
require lib/common.fth

vocabulary assembler

base @ hex

\ Assembler state.
variable opcode
variable bw
create ext  2 cells allot
variable #ext

\ Instruction fields.
: opcode!   3@ drop >r opcode ! ;
: opcode@   opcode @ ;
: .w   0000 bw ! ;
: .b   0040 bw ! ;

\ Reset assembler state.
: 0asm   0 #ext !  .w ;

\ Write instruction fields to memory.
previous
: h,   dup c, 8 rshift c, ;
: h@   dup c@ swap 1+ c@ 8 lshift + ;
: h!   2dup c!  1+ swap 8 rshift swap c! ;
: j+!   tuck h@ tuck + 03FF and swap FC00 and + swap h! ;
: opcode,   opcode@ h, ;
: pc-   here 2 + - 0FFFF and ;
also forth

\ Operand addressing modes.
-200000 constant register
 000000 constant indexed
 200000 constant indirect
 400000 constant post-increment
: >mode   register - + ;

\ Operand address.
010000 constant offset
: >ext   + offset invert and ;

\ Operand register.
: >reg   011 lshift register + offset + ;

\ Extension words.
: ext,   #ext @ begin ?dup while 1- dup cells ext + @ h, repeat ;
: !ext   ext #ext @ cells + !  1 #ext +! ;
: ext?   offset and 0= ;
: ?ext   dup ext? if !ext else drop then ;

\ Addressing modes.
: )#   indexed >mode  >ext ;	\ (Rn)
: &   2 >reg )# ;		\ &n
: )   indirect >mode ;		\ @Rn
: )+   post-increment >mode ;	\ @Rn+
: #   0 >reg )+  >ext ;		\ #n

\ Special constants.
: -1#   3 >reg )+ ;
: 0#   3 >reg ;
: 1#   3 >reg indexed >mode ;
: 2#   3 >reg ) ;
: 4#   2 >reg ) ;
: 8#   2 >reg )+ ;

: relative?   0FFFF invert and 0= ;
: ?relative ( u1 -- u1|u2 ) dup relative? if pc- then ;

\ Define registers
: reg:   dup >reg constant 1+ ;

\ Convert operand to instruction fields.
: s-reg   01E0000 and 9 rshift opcode +! ;
: d-reg   01E0000 and 11 rshift opcode +! ;
: s-mode   200000 + 11 rshift 0030 and opcode +! ;
: d-mode   200000 + 0E rshift 0080 and opcode +! ;

\ Instruction formats.
: instruction, ( a -- ) opcode! bw @ opcode +! opcode, ext, 0asm ;
: mnemonic ( u a "name" -- ) create ['] noop 3,  does> instruction, ;
: format:   create ] !csp  does> mnemonic ;

format: 0op ;
format: 1op   ?relative dup d-reg dup s-mode ?ext ;
format: 2op   ?relative dup d-reg dup d-mode ?ext
              ?relative dup s-reg dup s-mode ?ext ;
format: jump   pc- 1 rshift 03FF and opcode +! ;

\ Instruction mnemonics.
previous also assembler definitions

1000 1op rrc,
1080 1op swpb,
1100 1op rra,
1180 1op sxt,
1200 1op push,
1280 1op call,
1300 0op reti,
2000 jump jne,
2400 jump jeq,
2800 jump jnc,
2C00 jump jc,
3000 jump jn,
3400 jump jge,
3800 jump jl,
3C00 jump jmp,
4000 2op mov,
5000 2op add,
6000 2op addc,
7000 2op subc,
8000 2op sub,
9000 2op cmp,
A000 2op dadd,
B000 2op bit,
C000 2op bic,
D000 2op bis,
E000 2op xor,
F000 2op and,

\ Registers
0
reg: pc  reg: sp  reg: sr  reg: r3  reg: r4  reg: r5  reg: r6  reg: r7
reg: r8  reg: r9  reg: r10  reg: r11  reg: r12  reg: r13  reg: r14  reg: r15
drop

\ Emulated instructions.
: adc,   0# swap add, ;
: br,   pc mov, ;
: clr,   0# swap mov, ;
: clrc,   1# sr bic, ;
: clrn,   4# sr bic, ;
: clrz,   2# sr bic, ;
: dadc,   0# swap dadd, ;
: dec,   1# swap sub, ;
: decd,   2# swap sub, ;
: dint,   8# sr bic, ;
: eint,   8# sr bis, ;
: inc,   1# swap add, ;
: incd,   2# swap add, ;
: inv,   -1# swap xor, ;
: nop,   0# r3 mov, ;
: pop,   sp )+ swap mov, ;
: ret,   pc pop, ;
: rla,   dup add, ;
: rlc,   dup adc, ;
: sbc,   0# swap subc, ;
: setc,   1# sr bis, ;
: setn,   4# sr bis, ;
: setz,   2# sr bis, ;
: tst,   0# swap cmp, ;

\ Resolve jumps.
: >mark   here 2 - ['] j+! here ;
: >resolve   pc- negate 1 rshift -rot execute ;

\ Unconditional jumps.
: label   here >r get-current ['] assembler set-current r> constant set-current ;
: begin,   here ;
: again,   jmp, ;
: ahead,   here jmp, >mark ;
: then,   >resolve ;

\ Conditional jumps.
: 0=,   ['] jne, ;
: 0<,   ['] jge, ;
: 0<>,   ['] jeq, ;
: if,   here swap execute >mark ;
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
