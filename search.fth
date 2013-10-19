( Search-Order words. )

create voc-link  ' included-files ,

: forth-wordlist   ['] forth ;
: get-current      current @ ;
: ord ( n addr )   @+ ?dup if >r under 1+ recurse r> swap
                   else drop then ;
: get-order        0 context ord ;
: set-current      current ! ;
: definitions      context @ set-current ;
: (wordlist)       0 , voc-link @ , latestxt voc-link ! ;
: wordlist         :noname (wordlist) postpone ; ;

( Search-Order extension words. )

: forth        forth-wordlist context ! ;
: set-order    dup 0 = if ( todo ) abort then
               dup -1 = if ( todo ) abort else
               dup 8 > abort" Max 8 contexts allowed"
               n>r 0 nr> 1+
               context swap cells bounds
               do i ! cell +loop then ;
: only         forth-wordlist forth-wordlist 2 set-order ;
:noname        get-order 1+ over swap set-order ; is also
re: previous   get-order 1- nip set-order ;
: order        ." Order: " context begin dup @ ?dup while id. cell+ repeat
   drop cr ." Current: " current @ id. cr ;

( Traditional vocabulary words. )

: body>xt      [ 0 >body ] literal - ;
: vocabulary   create (wordlist)  does> body>xt context ! ;
: vocs         voc-link @ begin ?dup while dup id. >body cell+ @ repeat ;
