s" search.fth" included

vocabulary assembler

also assembler definitions

: code!        lastxt @ >code ! ;
: (;code)      r> code! ;
: start-code   here  also assembler ;
: end-code     here rwx! abort" Error calling mprotect"
               align previous ;

base @  hex

: nop     90 c, ;
: repz    f3 c, ;
: ret     c3 c, ;
: next    ret ;

base !  previous definitions  also assembler

: code    create  lastxt @ >body code!  start-code  ;
: ;code   postpone (;code) reveal postpone [ ?csp start-code ; immediate

previous

code foo
   next
end-code

: bar   create 42 ,  ;code next end-code


\ add %ecx,(%edx)	01 0a
\ add $c,addr		83 05 addr c
\ add $c,%eax		83 c0 c
\ add $c,%edx		83 c2 c
\ add $c,%esp		83 c4 c
\ sub $c,%ecx		83 ea c
\ sub $c,%esp		83 ec c

\ mov addr,%edx		8b 15 addr
\ mov addr,%eax		a1 addr
\ mov %edx,addr		89 15 addr
\ mov %eax,addr		a3 addr
\ mov $c,(%edx)		c7 02 c
\ mov (%edx),%ebx	8b 1a
\ mov (%edx),%ecx	8b 0a
\ mov (%eax),%edx	8b 10
\ mov %eax,%ebx		89 c3
\ mov %ebx,%eax		89 d8
\ mov %ecx,(%edx)	89 0a
\ mov %edx,(%esp)	89 14 24
\ mov %eax,c(%esp)	89 44 24 c
\ movl $u,(%esp)	c7 04 24 u
\ movzbl (%ecx),%ecx	0f b6 09
\ mov %cl,(%ebx)	88 0b

\ and %esi,%edx		21 f2
\ not %edx		f7 d2

\ push %ebx		53
\ pop %ebx		5b

\ call addr		e8 addr
\ call *%edx		ff d2
