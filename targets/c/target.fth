: io-init ;
: r/o   s" r" drop ;

\ If you change the definition of dodol, you also need to update the
\ offset to the runtime code in the metacompiler(s).
: docol,   'dodoes ,  does> >r ;

: dovar,   'dodoes ,  does> ;
: docon,   'dodoes ,  does> @ ;
: dodef,   'dodoes ,  does> perform ;

: #name ( -- u )       NAME_LENGTH 1 - ;
: name, ( a u -- )     #name min c,  #name ", ;
: header, ( a u -- )   align here >r name, r> link, 0 , ;

: >nfa ;
: >name    >nfa count cabs ;

: noheader,   s" " header, ;
