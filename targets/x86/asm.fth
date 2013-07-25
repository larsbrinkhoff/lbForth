\ Assembler for x86.

\ Adds to FORTH vocabulary: ASSEMBLER CODE ;CODE.
\ Creates ASSEMBLER vocabulary with: END-CODE and x86 opcodes.

s" search.fth" included

vocabulary assembler

also assembler definitions

: (;code)      r> code! ;
: start-code   also assembler ;
: end-code     align previous ;

base @  hex

: nop,     90 c, ;
: repz,    f3 c, ;
: ret,     c3 c, ;

: next,    ret, ;
: call,    e8 c, , ;
: jump,    e9 c, , ;

base !  previous definitions  also assembler

: code    create  latestxt >body code!  start-code  ;
: ;code   postpone (;code) reveal postpone [ ?csp start-code ; immediate

previous

\ code foo
\    next,
\ end-code
\ 
\ : bar   create 42 , ;code  base @ hex
\    8b c, 52 c, 1c c, \ mov 28(%edx),%edx
\    next,
\ end-code  base !
