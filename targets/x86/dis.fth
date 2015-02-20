\ Copyright Lars Brinkhoff 2013-2015.

require search.fth
[undefined] assembler [if] vocabulary assembler [then]
also assembler definitions

variable there

: c^   there @ c@ 1 there +! ;
: h^   c^ c^ 8 lshift + ;
: ^    there @ @ cell there +! ;

base @  hex

: 4chars,      here 4 cmove  4 allot ;
: >regname     [ 4 cell+ ] literal * + dup 4 + @ ;
also forth
: reg-table:   create  8 0 do bl word count swap 4chars, , loop
               does> -rot rshift 7 and >regname type ;
previous

reg-table: reg-name8     al  cl  dl  bl  ah  ch  dh  bh
reg-table: reg-name16    ax  cx  dx  bx  sp  bp  si  di
reg-table: reg-name32   eax ecx edx ebx esp ebp esi edi

defer operand-reg
defer address-reg
defer segment-reg
defer ?segment-reg
defer i^
defer a^

: operand!   is i^ is operand-reg ;
: !op8       ['] reg-name8  ['] c^ operand! ;
: !op16      ['] reg-name16 ['] h^ operand! ;
: !op32      ['] reg-name32 ['] ^  operand! ;

: address!   is a^ is address-reg ;
: !ad16      ['] reg-name16 ['] h^ address! ;
: !ad32      ['] reg-name32 ['] ^  address! ;

: segment! is ?segment-reg is segment-reg ;
: .ds ." ds:" ;   : !ds ['] .ds ['] noop segment! ;
: .es ." es:" ;   : !es ['] .es dup segment! ;
: .cs ." cs:" ;   : !cs ['] .cs dup segment! ;
: .ss ." ss:" ;   : !ss ['] .ss dup segment! ;
: .fs ." fs:" ;   : !fs ['] .fs dup segment! ;
: .gs ." gs:" ;   : !gs ['] .gs dup segment! ;

: displacement ( s/i/b mod/reg/rm -- s/i/b )
   c0 and 
   dup 00 = if else
   dup 40 = if c^ (.) ." +" else
   dup 80 = if a^ (.) ." +" else
   then then then drop ;

: base-reg ( s/i/b -- flag )
   dup 07 and 05 = if drop 0 else 0 address-reg 1 then ;

: index-reg ( s/i/b flag -- )
   swap dup 38 and 20 = if 2drop else
   swap if ." +" then
   dup 3 address-reg ." *" 6 rshift 1 swap lshift (.) then ;

: s/i/b ( mod/reg/rm -- )
   ." [" c^ swap displacement dup base-reg index-reg ." ]" ;

: mod/rm ( mod/reg/rm )
   dup c0 and c0 = if 0 operand-reg else ?segment-reg
   dup c7 and 05 = if drop ." [" a^ (.) ." ]" else
   dup 07 and 04 = if s/i/b else
   dup c0 and 00 = if ." [" 0 address-reg ." ]" else
   dup c0 and 40 = if ." [" 0 address-reg ." +" c^ (.) ." ]" else
   dup c0 and 80 = if ." [" 0 address-reg ." +" a^ (.) ." ]" else drop
   then then then then then then ;

: ?op8         01 and 0= if !op8 then ;
: direction?   02 and ;
: .,           ." , " ;

: mod/reg/rm ( op-code )
   dup ?op8 direction?  c^ dup rot
   if 3 operand-reg ., mod/rm else mod/rm ., 3 operand-reg then ;

defer decode

: opcode-c6 ( op-code )
   ?op8  c^ mod/rm ., i^ u. ;

: opcode-70 ( op-code )
   0f and
   dup 00 = if ." jo " else
   dup 01 = if ." jno " else
   dup 02 = if ." jc " else
   dup 03 = if ." jnb " else
   dup 04 = if ." je " else
   dup 05 = if ." jne " else
   dup 06 = if ." jbe " else
   dup 07 = if ." ja " else
   dup 08 = if ." js " else
   dup 09 = if ." jns " else
   dup 0a = if ." jp " else
   dup 0b = if ." jnp " else
   dup 0c = if ." jl " else
   dup 0d = if ." jge " else
   dup 0e = if ." jle " else
   dup 0f = if ." jg "
   then then then then then then then then
   then then then then then then then then
   drop c^ u. ;

: opcode-80 ( op-code )
   ?op8 c^ dup 38 and
   dup 00 = if ." add " else
   dup 08 = if ." or "  else
   dup 10 = if ." adc " else
   dup 18 = if ." sbb " else
   dup 20 = if ." and " else
   dup 28 = if ." sub " else
   dup 30 = if ." xor " else
   dup 38 = if ." cmp "
   then then then then then then then then
   drop mod/rm ., c^ u. ;

: opcode-f6 ( op-code )
   ?op8  c^ dup 38 and
   dup 00 = if ." test " ^ (.) ., else
   dup 08 = if ." test " ^ (.) ., else
   dup 10 = if ." not "  else
   dup 18 = if ." neg "  else
   dup 20 = if ." mul "  else
   dup 28 = if ." imul " else
   dup 30 = if ." div "  else
   dup 38 = if ." idiv "
   then then then then then then then then
   drop mod/rm ;

: prefix       drop decode ;
: no-d/s       fc and 3 or ;
: no-direction fd and ;
: eax          0 0 operand-reg ;
: reg          0 operand-reg ;
: eax/reg      eax ., reg ;
: eax/imm      ?op8 eax ., i^ u. ;
: moff         segment-reg (.) ;
: moff         if moff ., eax else eax ., moff then ;
: eax/moff     dup ?op8 direction?  i^ swap moff ;
: imm8         drop c^ u. ;
: imm16        drop h^ u. ;
: imm32        drop ^ u. ;
: imm32/48     drop h^ u. ." :" a^ u. ;
: cs           drop .cs ;
: ds           drop .ds ;
: es           drop .es ;
: ss           drop .ss ;
: none         drop ;
: aam/aad      c^ dup 0a =
               if drop 01 and if ." aad" else ." aam" then
               else ." unknown " swap . . then ;
: unknown      u. ;

(*
vocabulary opcodes
: name   also opcodes definitions create previous definitions ;
: 8<<    8 lshift swap ;
: ?shift   dup 100 < if 8<< 8<< then ;
: data   ?shift , , , , ;
: =>   name ' ' data ;
: ->   name ['] noop ' data ;
*)

also forth \ Override bl.
: => ( op m "str w1 w2" -- )   , , bl parse ' , ' ,        s, ;
: -> ( op m "str w" -- )       , , bl parse ' , ['] noop , s, ;
: ---> ( op m "w" -- )         , ,          ' , ['] noop , 0 , ;
: ===> ( op m "w1 w2" -- )     , ,          ' , ' ,        0 , ;
previous

create table-start
\  op mask  name     xt
   00 fc -> add      mod/reg/rm
   04 fe -> add      eax/imm
   06 ff -> push     es
   07 ff -> pop      es
   08 fc -> or       mod/reg/rm
   0e ff -> push     cs
\  0f ff --->        op...
\  0f31 ffff -> rdtsc
\  0f34 ffff -> sysenter
\  0f35 ffff -> sysexit
\  0f40 fff0 -> cmov
\  0f90 ffff -> seto
\  0f91 ffff -> setno
\  0f92 ffff -> setb
\  0f93 ffff -> setnb
\  0f94 ffff -> setz
\  0f95 ffff -> setnz
\  0f96 ffff -> setbe
\  0f97 ffff -> setnbe
\  0fa0 ffff -> push fs
\  0fa1 ffff -> pop fs
\  0fa2 ffff -> cpuid
\  0fb0 fffe -> cmpxchg
   10 fc -> adc      mod/reg/rm
   16 ff -> push     ss
   17 ff -> pop      ss
   18 fc -> sbb      mod/reg/rm
   1c fe -> sbb      eax/imm
   1e ff -> push     ds
   1f ff -> pop      ds
   20 fc -> and      mod/reg/rm
   24 fe -> and      eax/imm
   26 ff => es       prefix !es
   27 ff -> daa      none
   28 fc -> sub      mod/reg/rm
   2c fe -> sub      eax/imm
   2e ff => cs       prefix !cs
   2f ff -> das      none
   30 fc -> xor      mod/reg/rm
   34 fe -> xor      eax/imm
   36 ff => ss       prefix !ss
   37 ff -> aaa      none
   38 fc -> cmp      mod/reg/rm
   3c fe -> cmp      eax/imm
   3e ff => ds       prefix !ds
   3f ff -> aas      none
   40 f8 -> inc      reg        \ rex
   48 f8 -> dec      reg        \ rex
   50 f8 -> push     reg
   58 f8 -> pop      reg
   60 ff -> pusha    none
   61 ff -> popa     none
   64 ff => fs       prefix !fs
   65 ff => gs       prefix !gs
   66 ff ===>        prefix !op16
   67 ff ===>        prefix !ad16
   68 ff -> push     imm32
\  69 fd -> imul     ?
   6a ff -> push     imm8
\  6c fe -> ins      ?
\  6e fe -> outs     ?
   70 f0 --->        opcode-70  \ jcc
   80 fc --->        opcode-80  \ immediate add/or/adc/sbb/and/sub/xor/cmp
\  8000 fc38 -> add  mod/rm/imm8
\  8008 fc38 -> or   mod/rm/imm8
\  8010 fc38 -> adc  mod/rm/imm8
\  8018 fc38 -> sbb  mod/rm/imm8
\  8020 fc38 -> and  mod/rm/imm8
\  8028 fc38 -> sub  mod/rm/imm8
\  8030 fc38 -> xor  mod/rm/imm8
\  8038 fc38 -> cmp  mod/rm/imm8
   84 fe => test     mod/reg/rm no-direction
   86 fe => xchg     mod/reg/rm no-direction
   88 fc -> mov      mod/reg/rm
\  8c ff -> mov      reg16/sreg
   8d ff => lea      mod/reg/rm no-d/s
\  8e ff -> mov      sreg/rm16
\  8f ff -> pop      mem	\ op=0
   90 ff -> nop      none
   90 f8 -> xchg     eax/reg
   98 ff -> cwde     none
   99 ff -> cdq      none
   9a ff -> callf    imm32/48
   9c ff -> pushf    none
   9d ff -> popf     none
   9e ff -> sahf     none
   9f ff -> lahf     none
   a0 fc -> mov      eax/moff
\  a4 fe --->        movs
\  a6 fe --->        cmps
   a8 fe -> test     eax/imm
\  aa fe --->        stos
\  ac fe --->        lods
\  ad fe --->        scas
\  b0 f0 -> mov      reg/imm
\  c0 fe --->        mod/op/rm?
\  c000 fe38 -> rol
\  c008 fe38 -> ror
\  c010 fe38 -> rcl
\  c018 fe38 -> rcr
\  c020 fe38 -> shl/sal
\  c028 fe38 -> shr
\  c030 fe38 -> sar/shl
\  c038 fe38 -> sar
   c2 ff -> ret      imm16
   c3 ff -> ret      none
\  c4 ff => les      mod/reg/rm no-d/s
\  c5 ff => lds      mod/reg/rm no-d/s
   c6 fe -> mov      opcode-c6
\  c8 ff -> enter    ?
   c9 ff -> leave    none
   ca ff -> retf     imm16
   cb ff -> retf     none
   cc ff -> int3     none
   cd ff -> into     none
   ce ff -> int      imm8
   cf ff -> iret     none
\  d0 fe --->        mod/op/rm?
\  d000 fe38 -> rol
\  d008 fe38 -> ror
\  d010 fe38 -> rcl
\  d018 fe38 -> rcr
\  d020 fe38 -> shl/sal
\  d028 fe38 -> shr
\  d030 fe38 -> sar/shl
\  d038 fe38 -> sar
\  d2 fe --->        cl/op/rm
   d4 fe --->        aam/aad
   d7 ff -> xlat     none
\  d8 ff --->
\        0 fadd, 1 fmul, 2 fcom, 3 fcomp, 4 fsub, 5 fsubr, 6 fdiv, 7 fdivr
\  d9-df ff --->     float...
\  e0 fc --->        loop
\  e4 fe -> in       imm8
\  e6 fe -> out      imm8
   e8 ff -> call     imm32
   e9 ff -> jmp      imm32
\  ea ff -> jmp      imm48
   eb ff -> jmp      imm8
\  ec fe -> in       ?
\  ee fe -> out      ?
   f0 ff -> lock     none
   f2 fe -> rep      prefix
   f4 ff -> hlt      none
   f5 ff -> cmc      none
   f6 fe --->        opcode-f6 \ test/not/neg/mul/imul/div/idiv
   f8 ff -> clc      none
   f9 ff -> stc      none
   fa ff -> cli      none
   fb ff -> sti      none
   fc ff -> cld      none
   fd ff -> std      none
\  fe ff --->        inc/dec
\  ff ff --->        inc/dec/call/jmp/push
\        0 inc, 1 dec, 2 call, 3 callf, 4 jmp, 5 jmpf, 6 push, 7 ?
   00 00 -> ???      unknown
here constant table-end

: tab>mask    @ ;
: tab>op      cell+ @ ;
: tab>xt      2 cells + @ ;
: tab>xt2     3 cells + @ ;
: tab>name    4 cells + ;
: tab>cells   tab>name @ 5 cells + aligned ;

:noname
   c^ table-end table-start do
      dup i tab>mask and i tab>op = if
         i tab>name dup cell+ swap @ ?dup if type space else drop then
         i tab>xt2 execute  i tab>xt execute leave then
   i tab>cells +loop ;
is decode

: reset-prefixes    !op32 !ad32 !ds ;
: disassemble-one   reset-prefixes decode ;

base !  previous definitions  also assembler

: disassemble ( start end -- )
   swap there !  begin there @ over u< while
      ."   " there @ u. space  disassemble-one cr
   repeat drop ;

previous
