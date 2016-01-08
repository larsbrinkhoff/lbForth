: docol,   'docol code, ;
: dovar,   'dovar code, ;
: docon,   'docon code, ;
: dodef,   'dodef code, ;

: #name ( -- u )       NAME_LENGTH 1 - ;
: name, ( a u -- )     #name min c,  #name ", ;
: header, ( a u -- )   align here >r name, r> link, ;

: >nfa ;
: >name    >nfa count cabs ;
