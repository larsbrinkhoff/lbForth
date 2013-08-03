\ PDP-1 nucleus.

require targets/pdp1/asm.fth

also assembler
base @  8 base !

\ "Registers"; these are all memory locaions.
4 cells constant ip
5 cells constant sp
6 cells constant rp
7 cells constant w
10 cells constant t

\ : >page ( a1 -- a2 )    cell / 7777 + 170000 and cells ;
\ : const ( x -- addr )   'const >page 'const do
\                            i @ over = if drop i unloop exit then
\                          loop -1 cells +to 'const  c 'const !  'const ;

: swap, \ Swap contents between AC and IO.
   9s rcl,
   9s rcl, ;

: addi, ( addr n -- ) \ Add n to contents at addr.
            law,
   dup      add,
            dac, ;

: subi, ( addr n -- ) \ Subtract n from contents at addr.
   negate   addi, ;

: move, ( addr1 addr2 -- ) \ Copy contents of addr2 to addr1.
            lac,
            dac, ;

: push, ( addr -- ) \ Push contents at addr onto data stack.
   sp 1      subi,
   sp i swap move, ;

: pushio, ( -- ) \ Push IO onto data stack.
   sp 1     subi,
   sp i     dio, ;

: pop, ( addr -- ) \ Pop data stack into addr.
   sp i     move,
   sp       idx, ;

: popio, ( addr -- ) \ Pop data stack into IO.
   sp i     lio,
   sp       idx, ;

: rpush, ( addr -- ) \ Push contents at addr onto return stack.
   rp 1      subi,			\		rp i swap move,
   rp i swap move, ;			\		rp idx,

: rpop, ( addr -- ) \ Pop return stack into addr.
   rp i     move,			\		rp 1 subi,
   rp       idx, ;			\		rp i move,

\ Indirect threaded code.		\ Direct threaded code.

: next,
   w ip i   move,  \ 5		w ip i  move, 5
   ip       idx,   \ 2		ip      idx,  2
   t w i    move,  \ 5		w i     jmp,  2=9
   t i      jmp, ; \ 2=14

: create     header,  ( ['] dovar ) 0 jsp,  reveal ;
: variable   create cell allot ;

\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \
\ Target image starts here.

here to offset

\ Sequence break.
4 cells allot

0 variable ip
0 variable sp
0 variable rp
0 variable w
0 variable t

100 allot    ( here value sp0 )
create rp0   20 allot

\ code cold
\    ' quit  jmp,
\ end-code
				\ Code field for enter.
\				enter jmp,    1		enter jsp, 1
code enter
   ip       rpush, \ 10		ip rpush,     10	ip lio,    2
   w        idx,   \ 2		w idx,        2		rp i dio,  3
   ip       dac,   \ 2=14	ip dac,       2=15	ip dac,    2
            next,  \					rp idx,    2=10
end-code

code exit
   ip       rpop,  \ 7		ip rpop,      7		ip rpop,   10
            next,
end-code	   \ ==49		      ==40		   ==38

code dovar
   w        idx,   2					swap,   2
   w        push, 10=12					pushio, 8=10
            next,
end-code

code docon
   w        idx,   2					w dac,     2
   w i      push, 11=13					w i push, 11=13
            next,
end-code

code dodoes
            swap,   2		<same>			<x>
   ip       rpush, 10					ip lio,    2
   ip       dio,    2					rp i dio,  3
   w        idx,    2					ip dac,    2
   w        push,  10					rp idx,    2
            next,  +1=27	+2=28			<x> push, 10
end-code						          +3=22

0 value latestxt
: does!   latestxt >code >h jsp, h> ;
: does,   [ ' dodoes >body ] literal jda, ;
\ (does>) r> does! ;
\ does>   postpone (does>) does, ; immediate

\ ITC				DTC
\ "myconst" header		"myconst" header	...
\ "enter" address		enter jsp,
\ "create" address		"create" address
\ "," address			"," address
\ "(does>)" address		"(does>)" address
\ dodoes jsp, <action>		dodoes jsp, <action>	dodoes jda,
\ "@" address			"@" address
\ "exit" address		"exit" address
\
\ "fortytwo" header		"fortytwo" header	"fortytwo" header
\ <action>			<action> jmp,		<action> jsp,
\ 42				42			42

code 0branch
   sp i     lac,	\ Zero?
            sza,
   4 +.     jmp,
   ip ip i  move,	\ Fetch branch target from thread.
   2 +.     jmp,
   ip       idx,	\ Otherwise, increment IP.
   sp       idx,	\ Pop stack.
            next,
end-code

code branch
   ip ip i  move,
            next,
end-code

code execute
   ip       rpush,
   ip       pop,
            next,
end-code

code bye
            hlt,
            next,	\ In case we proceed.
end-code

code @				\ TOS in t.
   w sp i   move, \ 5		t t i move, 5
   sp i w i move, \ 6=11
            next,
end-code

code !
   w        pop, \ 7		t i pop, 8
   w i      pop, \ 8=15		t pop,   7=15
            next,
end-code

code (literal)
   ip i     push, \ 11		t push,     10
   ip       idx,  \  2=13	t ip i move, 5
            next, \		ip idx,      2=17
end-code

code r>
   sp 1     subi, \ 5		t push, 10
   sp i     rpop, \ 8=13	t rpop,  7=17
            next,
end-code

code >r
   rp 1     subi, \ 5		t rpush, 10		rp i pop,
   rp i     pop,  \ 8		t pop, 7		rp idx,
            next,
end-code

code r@
   rp i     push, \ 11		t push,     10
            next, \		t rp i move, 5=15
end-code

code +
   w        pop, \ 7		t    lac, 2
   sp i     lac, \ 3		sp i add, 3
   w        add, \ 2		t    dac, 2
   sp i     dac, \ 3=16		sp   idx, 2=12
            next,
end-code

code +!
   w        pop, \ 7		t i lac,  3
   sp i     lac, \ 3		sp i add, 3
   w i      add, \ 3		t i dac,  3
   w i      dac, \ 3		sp idx,   2
   sp       idx, \ 2=18		t pop,    7=18
            next,
end-code

code nand
   w        pop,
   sp i     lac,
   w        and,
            cma,
   sp i     dac,
            next,
end-code

code emit
   sp i     popio \ 5		t lio, 2
            tyo,  \ 1=6		tyo,   1
            next, \		t pop, 7=10
end-code

code key
   10001    szf,
   -1 +.    jmp,
            tyi,
   1        clf,
            pushio,
            next,
end-code

code invert
   sp i     lac, \ 3		t lac, 2
            cma, \ 1		cma,   1
   sp i     dac, \ 3=7		t dac, 2=5
            next,
end-code

\ : negate    invert ;

code 0=
   true     lio, \ 2		true lio, 2
   sp i     lac, \ 3		t lac,    2
   i        sza, \ 1		i sza,    1
            cli, \ 1?		cli,      1?
   sp i     dio, \ 3=9,5	t dio,    2=7,5
            next,
end-code

code =
   true     lio, \ 2		true lio, 2
   w        pop, \ 7		t lac,    2
   sp i     lac, \ 3		sp i sas, 3
   w        sas, \ 2		cli,      1?
            cli, \ 1?		sp idx,   2
   sp i     dio, \ 3=17,5	t dio,    2=11,5
            next,
end-code

code c@
   ???
end-code

code c!
   ???
end-code

code drop
   sp       idx, \ 2		t pop, 7
            next,
end-code

code dup
   sp i     lio,    \ 3		t push, 10
            pushio, \ 8=11
            next,
end-code

code ?dup
   sp i     lac,    \ 3		t lac,    2
   i        sza,    \ 1		i sza,    1
   6 +.     jmp,    \ 1		6 +. jmp, 1
   sp i     lio,    \ 3		t push,   10=14
            pushio, \ 8=16
            next,
end-code

code swap
   w sp     move, \ 4		sp i lio,    3
   w        idx,  \ 2		sp i t move, 5
   w i      lio,  \ 3		t dio,       2=10
   w i sp i move, \ 6
   sp i     dio,  \ 3=18
            next,
end-code

code nip
            popio, \ 5		sp idx, 2
   sp i     dio,   \ 3
            next,
end-code

code over
   w sp     move, \ 4		sp i lio, 3
   w        idx,  \ 2		t push,   10
   w i      push, \ 11		t dio,    2
            next, \ =18			  = 15
end-code

code tuck
   w sp     move,   \ 4		sp i lio, 3
   w        idx,    \ 2		t lac,    2
   sp i     lio,    \ 3		sp i dac, 3
   sp i w i move,   \ 6		pushio,   8=16
   w i      dio,    \ 3
            pushio, \ 8=27
            next,
end-code

code rot
   w sp     move,   \ 4		w sp move,   4
   w        idx,    \ 2		w idx,       2
   w i      lio,    \ 3		sp i lio,    3
   w i sp i move,   \ 6		sp i t move, 5
   w        idx,    \ 2		w i lac,     3
   w i      lac,    \ 3		w i dio,     3
   w i      dio,    \ 3		t dac,       2=22
   sp i     dac,    \ 3=26
            next,
end-code
