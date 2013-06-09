\ Disassembler for PDP-1.

\ s" search.fth" included
\ vocabulary assembler
s" experiments/asm-pdp1.fth" included

also assembler definitions

variable there

: c^   there @ c@ 1 there +! ;
: h^   c^ c^ 8 lshift + ;
: ^    there @ @ cell there +! ;

base @  8 base !

: 4chars,      here 4 cmove  4 allot ;
: >regname     [ 4 cell+ ] literal * + dup 4 + @ ;
: reg-table:   create  8 0 do bl word count swap 4chars, , loop
               does> -rot rshift 7 and >regname type ;

reg-table: reg-name   r0 r1 r2 r3 r4 r5 sp pc

: nop ;
: .,           ." , " ;

: reg          0 reg-name ;
: none         drop ;
: unknown      u. ;

: => ( op m "str w1 w2" -- )   , , bl parse ' , ' ,       dup , string, ;
: -> ( op m "str w" -- )       , , bl parse ' , ['] nop , dup , string, ;
: ---> ( op m "w" -- )         , ,          ' , ['] nop , 0 , ;
: ===> ( op m "w1 w2" -- )     , ,          ' , ' ,       0 , ;

create table-start
\  op     mask      name xt
   020000 760000 -> and  mem	\ AC = AC & M[MA]
   040000 760000 -> ior  mem	\ AC = AC | M[MA]
   060000 760000 -> xor  mem	\ AC = AC ^ M[MA]
   100000 760000 -> xct  mem	\ execute M[MA]
   160000 770000 -> cal  none	\ M[100] = AC, AC = PC, PC = 101
   170000 770000 -> jda  mem	\ M[MA] = AC, AC = PC, PC = MA + 1
   200000 760000 -> lac  mem	\ AC = M[MA]
   220000 760000 -> lio  mem	\ IO = M[MA]
   240000 760000 -> dac  mem	\ M[MA] = AC
   260000 760000 -> dap  mem
   300000 760000 -> dip  mem
   320000 760000 -> dio  mem	\ M[MA] = IO
   340000 760000 -> dzm  mem	\ M[MA] = 0
   400000 760000 -> add  mem	\ AC = AC + M[MA]
   420000 760000 -> sub  mem	\ AC = AC - M[MA]
   440000 760000 -> idx  mem	\
   460000 760000 -> isp  mem
   500000 760000 -> sad  mem	\ if (AC != M[MA]) PC = PC + 1
   520000 760000 -> sas  mem	\ if (AC == M[MA]) PC = PC + 1
   540000 760000 -> mul  mem
   560000 760000 -> div  mem
   600000 760000 -> jmp  mem	\ PC = MA
   620000 760000 -> jsp  mem	\ AC = PC, PC = MA
   640000 760000 -> skip skip
   \ 000x szf
   \ 00x0 szs
   \ 0100 sza
   \ 0200 spa
   \ 0400 sma
   \ 1000 szo
   \ 2000 spi
   661000 777000 -> ral  shift
   662000 777000 -> ril  shift
   663000 777000 -> rcl  shift
   665000 777000 -> sal  shift
   666000 777000 -> sil  shift
   667000 777000 -> scl  shift
   671000 777000 -> rar  shift
   672000 777000 -> rir  shift
   673000 777000 -> rcr  shift
   675000 777000 -> sar  shift
   676000 777000 -> sir  shift
   677000 777000 -> scr  shift
   700000 760000 -> law  immediate
   720000 760000 -> iot  i/o
   760000 760000 -> opr  operate
   \ 0000 nop
   \ 000x clf
   \ 001x stf
   \ 0200 cla
   \ 0400 halt
   \ 1000 cma
   \ 2000 lat
   \ 4000 cli
   000000 000000 -> ???  unknown
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
