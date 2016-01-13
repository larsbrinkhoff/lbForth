get-current
also assembler definitions

[undefined] code-offset [if]
include targets/x86/params.fth
[then]

\ Register allocation.
: I   eax ;
: S   esp ;
: R   esi ;
: W   edx ;

\ Macros.
: op>r     postpone >r postpone 2>r ; compile-only
: opr>     postpone 2r> postpone r> ; compile-only
: fetch,   op>r I ) opr> mov,  4 # I add, ;
: rpop,    op>r R ) opr> mov,  4 # R add, ;
: rpush,   4 # R sub,  R ) mov, ;
: execute, code-offset ?dup if W )# else W ) then indirect-jmp, ;

\ Next.
: next,   W fetch,  execute, ;

previous
set-current
