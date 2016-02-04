\ Copyright 2016 Lars Brinkhoff.

\ Assembler for Texas Instruments MSP430.

hex


\ Instruction fields.

: .b   0040 opcode +! ;


\ Addressing modes.

100000 constant register
110000 constant indexed
120000 constant indirect
130000 constant post-increment

: )#   indexed xor ;		\ (Rn)
: &   1 ) ;
: )   indirect xor ;		\ @Rn
: )+   post-increment xor ;	\ @Rn+
: #   0 )+ ;

\ -1 #  ->  3 )+
\ 0 #  ->  3
\ 1 #  ->  3 )#
\ 2 #  ->  3 )
\ 4 #  ->  2 )
\ 8 #  ->  2 )+


\ Registers

: reg:   dup register + constant 1+ ;

0
reg: pc  reg: sp  reg: sr  reg: r3  reg: r4  reg: r5  reg: r6  reg: r7
reg: r8  reg: r9  reg: r10  reg: r11  reg: r12  reg: r13  reg: r14  reg: r15
drop


\ Instruction formats.

\ format: 1op
\ ooooooooo b ss dddd
\ ooooooooo = opcode
\ b = byte/word
\ ss = addressing mode
\ dddd = register

\ format: 2op
\ oooo ssss d b ss dddd
\ oooo = opcode
\ ssss = source register
\ d = destination addressing mode
\ b = byte/word
\ ss = source addressing mode
\ dddd = destination register

\ format: jump
\ ooo ccc dddddddddd
\ ooo = opcode
\ ccc = condition
\ dddddddddd = offset


\ Mnemonics.

\ 1000 1op rrc,
\ 1080 1op swpb,
\ 1100 1op rra,
\ 1180 1op sxt,
\ 1200 1op push,
\ 1280 1op call,
\ 1300 1op reti,
\ 2000 jump jne,
\ 2400 jump jeq,
\ 2800 jump jnc,
\ 2C00 jump jc,
\ 3000 jump jn,
\ 3400 jump jge,
\ 3800 jump jl,
\ 3C00 jump jmp,
\ 4000 2op mov,
\ 5000 2op add,
\ 6000 2op addc,
\ 7000 2op subc,
\ 8000 2op sub,
\ 9000 2op cmp,
\ A000 2op dadd,
\ B000 2op bit,
\ C000 2op bic,
\ D000 2op bis,
\ E000 2op xor,
\ F000 2op and,


\ Emulated instructions.

\ asm-op-swap
\ asm-op-dup
: adc,   0 # asm-op-swap add, ;
: br,   pc mov, ;
: clr,   0 # asm-op-swap mov, ;
: clrc,   1 # sr bic, ;
: clrn,   4 # sr bic, ;
: clrz,   2 # sr bic, ;
: dadc,   0 # asm-op-swap daddc, ;
: dec,   1 # asm-op-swap sub, ;
: decd,   2 # asm-op-swap sub, ;
: dint,   8 # sr bic, ;
: eint,   8 # sr bis, ;
: inc,   1 # asm-op-swap add, ;
: incd,   2 # asm-op-swap add, ;
: inv,   0FFFF # asm-op-swap xor, ;
: nop,   0 # r3 mov, ;
: pop,   sp @+ asm-op-swap mov, ;
: ret,   pc pop, ;
: rla,   asm-op-dup dst add, ;
: rlc,   asm-op-dup adc, ;
: sbc,   0 # asm-op-swap subc, ;
: setc,   1 # sr bis, ;
: setn,   4 # sr bis, ;
: setz,   2 # sr bis, ;
: tst,   0 # asm-op-swap cmp, ;
