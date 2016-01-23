get-current
also assembler definitions

[undefined] code-offset [if]
include targets/m68k/params.fth
[then]

\ Register allocation.
: S   a7 ;
: I   a6 ;
: R   a5 ;
: W   a4 ;
: T   d0 ;

\ Macros.
: op>r     postpone >r postpone 2>r ; compile-only
: opr>     postpone 2r> postpone r> ; compile-only
: fetch,   op>r I )+ opr> .l move, ;
: pop,     op>r S )+ opr> .l move, ;
: push,    S -) .l move, ;
: rpop,    op>r R )+ opr> .l move, ;
: rpush,   R -) .l move, ;
\ : execute, code-offset ?dup if W )# else W ) then jmp, ;
: execute,   W ) a3 .l move,  a3 ) jmp, ;

\ Next.
: next,   W fetch,  execute, ;

previous
set-current
