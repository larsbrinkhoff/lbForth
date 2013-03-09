s" search.fth" included

vocabulary assembler

also assembler definitions

0 value code-start

: code!        lastxt @ >code ! ;
: (;code)      r> code! ;
: start-code   here to code-start  also assembler ;
: end-code     code-start here rwx! abort" Error calling mprotect"
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

\ code foo
\    next
\ end-code
\ 
\ : bar   create 42 , ;code  base @ hex
\    8b c, 52 c, 1c c, \ mov 28(%edx),%edx
\    next
\ end-code  base !
