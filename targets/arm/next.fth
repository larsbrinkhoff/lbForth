get-current
also assembler definitions

[undefined] code-offset [if]
include targets/arm/params.fth
[then]

\ Register allocation.  r0-r2 and r7 are used in Linux syscalls.
: S   r9 ;
: R   r8 ;
: I   r5 ;
: W   r4 ;
: T   r3 ;

\ Macros.
: op>r     postpone >r postpone 2>r ; compile-only
: opr>     postpone 2r> postpone r> ; compile-only
: fetch,   op>r 4 I #) opr> ldr, ;
: pop,     op>r 4 S #) opr> ldr, ;
: push,    op>r -4 S )#! opr> str, ;
: rpop,    op>r 4 R #) opr> ldr, ;
: rpush,   op>r -4 R )#! opr> str, ;
: execute,   W ) pc ldr, ;

\ Next.
: next,   W fetch,  execute, ;

previous
set-current
