get-current
also assembler definitions

[undefined] code-offset [if]
include targets/pdp11/params.fth
[then]

\ Register allocation.
: S   sp ;
: I   r5 ;
: R   r4 ;
: W   r3 ;
: T   r2 ;

\ Macros.
: op>r     postpone >r postpone 2>r ; compile-only
: opr>     postpone 2r> postpone r> ; compile-only
: fetch,   op>r I )+ opr> mov, ;
: pop,     op>r S )+ opr> mov, ;
: push,    S -) mov, ;
: rpop,    op>r R )+ opr> mov, ;
: rpush,   R -) mov, ;
: execute,   W ) T mov,  T ) jmp, ;

\ Next.
: next,   W fetch,  execute, ;

previous
set-current
