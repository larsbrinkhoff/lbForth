\ PDP-8 nucleus.

8 base !

\ Registers, in zero page.
0214 constant w
0215 constant ip
0216 constant sp
0217 constant rp

code next
   cla,
   ip i tad,
   w dca,
   cia,
   ip tad,
   ip dca,
   w i jmp,
end-code

: next,   next jmp, ;

\ Code field for : is "enter jms,"
code enter
   label x
   0 ,
   cla+cma,
   rp tad,
   rp dca,
   ip tad,
   rp i dca,
   x tad,
   ip dca,
   next,
end-code

code exit
   cla,
   rp i tad,
   ip dca,
   cma,
   rp tad,
   rp dca,
   next,
end-code

code dodoes
   label x
   0 ,
   cla+cma,
   rp tad,
   rp dca,
   ip tad,
   rp i dca,
   x tad,
   ip dca,
   
   -2 -> AC
   x i tad,
end-code

\ HEADER  "constant"
\ CODE    enter jms,
\ XT      create
\ XT      ,
\ XT      (does>)
\ DATA    0           <- x
\ CODE    dodoes jms,
\ XT      @
\ XT      exit

\ HEADER  "fourtytwo"
\ CODE    x jms,
\ DATA    42

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
