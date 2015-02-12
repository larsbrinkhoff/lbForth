\ Disassembler for PDP-11.

s" search.fth" included

vocabulary assembler

also assembler definitions

variable there

: c^   there @ c@ 1 there +! ;
: h^   c^ c^ 8 lshift + ;
: ^    there @ @ /cell there +! ;

base @  hex

: 4chars,      here 4 cmove  4 allot ;
: >regname     [ 4 cell+ ] literal * + dup 4 + @ ;
: reg-table:   create  8 0 do bl word count swap 4chars, , loop
               does> -rot rshift 7 and >regname type ;

reg-table: reg-name   r0 r1 r2 r3 r4 r5 sp pc

: .,           ." , " ;

: reg          0 reg-name ;
: imm6         003f and u. ;
: imm8         00ff and u. ;
: none         drop ;
: unknown      u. ;

: op           dup 0038 and 0000 = if 0 reg-name else
               dup 0030 and 0080 = if ." (" 0 reg-name ." )" else
               dup 0030 and 0100 = if ." (" 0 reg-name ." )+" else
               dup 0030 and 0180 = if ." @(" 0 reg-name ." )+" else
               dup 0030 and 0200 = if ." -(" 0 reg-name ." )" else
               dup 0030 and 0280 = if ." @-(" 0 reg-name ." )" else
               dup 0030 and 0300 = if ." xxx(" 0 reg-name ." )" else
               dup 0030 and 0380 = if ." @xxx(" 0 reg-name ." )" else
               then then then then then then then then ;

: => ( op m "str w1 w2" -- )   , , bl parse ' , ' ,        s, ;
: -> ( op m "str w" -- )       , , bl parse ' , ['] noop , s, ;
: ---> ( op m "w" -- )         , ,          ' , ['] noop , 0 , ;
: ===> ( op m "w1 w2" -- )     , ,          ' , ' ,        0 , ;

create table-start
\  op   mask    name   xt
   0000 ffff -> halt   none
   0001 ffff -> wait   none
   0002 ffff -> rti    none
   0003 ffff -> bpt    none
   0040 ffc0 -> jmp    op
   0080 fff8 -> rts    reg
   00a0 ffff -> nop    none
   00a1 ffff -> clc    none
   00a2 ffff -> clv    none
   00a4 ffff -> clz    none
   00a8 ffff -> cln    none
   00af ffff -> ccc    none
   00b1 ffff -> sec    none
   00b2 ffff -> sev    none
   00b4 ffff -> sez    none
   00b8 ffff -> sen    none
   00bf ffff -> scc    none
   00c0 ffc0 -> swab   op
   0100 ff00 -> br     displ
   0200 ff00 -> bne    displ
   0300 ff00 -> beq    displ
   0400 ff00 -> bge    displ
   0500 ff00 -> blt    displ
   0600 ff00 -> bgt    displ
   0700 ff00 -> ble    displ
   0800 fe00 -> jsr    reg_op
   0a00 ffc0 -> clr    op
   0a40 ffc0 -> com    op
   0a80 ffc0 -> inc    op
   0ac0 ffc0 -> dec    op
   0b00 ffc0 -> neg    op
   0b40 ffc0 -> adc    op
   0b80 ffc0 -> sbc    op
   0bc0 ffc0 -> tst    op
   0c00 ffc0 -> ror    op
   0c40 ffc0 -> rol    op
   0c80 ffc0 -> asr    op
   0cc0 ffc0 -> asl    op
   0d00 ffc0 -> mark   imm6
   1000 f000 -> mov    op_op
   2000 f000 -> cmp    op_op
   3000 f000 -> bit    op_op
   4000 f000 -> bic    op_op
   5000 f000 -> bis    op_op
   6000 f000 -> add    op_op
   7000 fe00 -> mul    reg_op_rev
   7200 fe00 -> div    reg_op_rev
   7400 fe00 -> ash    reg_op_rev
   7800 fe00 -> xor    reg_op
   7a00 fff8 -> fadd   reg
   7a08 fff8 -> fsub   reg
   7a10 fff8 -> fmul   reg
   7a18 fff8 -> fdiv   reg
   8000 ff00 -> bpl    displ
   8100 ff00 -> bmi    displ
   8200 ff00 -> bhi    displ
   8300 ff00 -> blos   displ
   8400 ff00 -> bvc    displ
   8500 ff00 -> bvs    displ
   8600 ff00 -> bcc    displ
   8700 ff00 -> bcs    displ
   8800 ff00 -> emt    imm8
   8900 ff00 -> sys    imm8
   8a00 ffc0 -> clrb   op
   8a40 ffc0 -> comb   op
   8a80 ffc0 -> incb   op
   8ac0 ffc0 -> decb   op
   8b00 ffc0 -> negb   op
   8b40 ffc0 -> adcb   op
   8b80 ffc0 -> sbcb   op
   8bc0 ffc0 -> tstb   op
   8c00 ffc0 -> rorb   op
   8c40 ffc0 -> rolb   op
   8c80 ffc0 -> asrb   op
   8cc0 ffc0 -> aslb   op
   9000 f000 -> movb   op_op
   a000 f000 -> cmpb   op_op
   b000 f000 -> bitb   op_op
   c000 f000 -> bicb   op_op
   d000 f000 -> bisb   op_op
   e000 f000 -> sub    op_op
   f000 f000 -> movb   op_op
   0000 0000 -> ???    unknown
here constant table-end

: tab>mask    @ ;
: tab>op      cell+ @ ;
: tab>xt      2 cells + @ ;
: tab>xt2     3 cells + @ ;
: tab>name    4 cells + ;
: tab>cells   tab>name @ 5 cells + aligned ;

: decode
   h^ table-end table-start do
      dup i tab>mask and i tab>op = if
         i tab>name dup cell+ swap @ ?dup if type space else drop then
         i tab>xt2 execute  i tab>xt execute leave then
   i tab>cells +loop ;

base !  previous definitions  also assembler

: disassemble ( start end -- )
   swap there !  begin there @ over u< while
      ."   " there @ u. space  decode cr
   repeat drop ;

previous
