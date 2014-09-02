\ Assembler for x86.

\ Adds to FORTH vocabulary: ASSEMBLER CODE ;CODE.
\ Creates ASSEMBLER vocabulary with: END-CODE and x86 opcodes.

s" search.fth" included

vocabulary assembler

also assembler definitions

' here  defer here  is here
' c,    defer c,    is c,

: h,   dup c,  8 rshift c, ;
: ,   dup h,  16 rshift h, ;

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

variable imm
variable op-size
variable mod-r/m
: # ( x -- x )   1 imm ! ;
: !op8   0 op-size ! ;
: !op32   1 op-size ! ;
: !op16   66 c,  !op32 ;
: al ( -- x )   !op8 0 ;
: ax ( -- x )   !op16 0 ;
: eax ( -- x )   !op32 0 ;
: add ( src dst -- )  imm @ if 80 op-size @ + mod-r/m + c,
   else 00 op-size @ + c, then ;
