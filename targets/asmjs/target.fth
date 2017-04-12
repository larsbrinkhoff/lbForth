: io-init ;
: r/o   s" r" drop ;

\ If you change the definition of docol, you also need to update the
\ offset to the runtime code in the metacompiler(s).
: docol,   0 , 'docol ,  ;
: dovar,   0 , 'dovar ,  ;
: docon,   0 , 'docon ,  ;
: dodef,   0 , 'dodef , ;

: NAME_LENGTH 16 ;
: #name ( -- u )       NAME_LENGTH 1 - ;
: name, ( a u -- )     #name min c,  #name ", ;
: header, ( a u -- )   align here >r name, r> link, 0 , ;

: >nfa ;
: >xt drop 0 ;
: >name    >nfa count cabs ;

: header, ( a u -- )   align here >r name, r> link, ;
: noheader,   s" " header, ;
