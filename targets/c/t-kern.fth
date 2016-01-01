: io-init ;
: r/o   s" r" drop ;

\ If you change the definition of dodol, you also need to update the
\ offset to the runtime code in the metacompiler(s).
: docol,   'dodoes ,  does> >r ;

: dovar,   'dodoes ,  does> ;
: docon,   'dodoes ,  does> @ ;
: dodef,   'dodoes ,  does> perform ;
