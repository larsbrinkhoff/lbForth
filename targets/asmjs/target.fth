: io-init ;
: r/o   s" r" drop ;

\ If you change the definition of docol, you also need to update the
\ offset to the runtime code in the metacompiler(s).
: docol,   'docol , ;
: dovar,   'dovar , ;
: docon,   'docon , ;
: dodef,   'dodef , ;

: NAME_LENGTH 16 ;
: #name ( -- u )       NAME_LENGTH 1 - ;
: name, ( a u -- )     #name min c,  #name ", ;
: header, ( a u -- )   align here >r name, r> link, 0 , ;

: >nfa ;
: >xt drop 0 ;
: >name    >nfa count cabs ;

: noheader,   s" " header, ;
