: docol,   'docol code, ;
: dovar,   'dovar code, ;
: docon,   'docon code, ;
: dodef,   'dodef code, ;

31 constant #name
: >xt   6 + aligned ;
: offset,   1 allot align  -1 allot >xt c, ;
: name, ( a u -- )     #name min  dup c,  tuck move,  offset, ;
: header, ( a u -- )   align name, here cell+ link, ;

: >nfa   dup 5 - c@ - ;
: >name    >nfa count cabs ;

: noheader,   here 0 latest! ;
