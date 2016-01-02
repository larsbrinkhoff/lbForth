( Search-Order words. )

: forth-wordlist   ['] forth ;
: get-current      current @ ;
: ord ( n addr )   @+ ?dup if >r under 1+ recurse r> swap
                   else drop then ;
: get-order        0 context ord ;
: set-current      current ! ;
: definitions      context @ set-current ;
: (wordlist)       0 ,  latestxt voc-link chain, ;
: wordlist         :noname (wordlist) reveal postpone [ ?csp ;

( Search-Order extension words. )

create only (wordlist)
: forth   forth-wordlist context ! ;
' only dup set-current
: forth forth ;
: forth-wordlist forth-wordlist ;
forth-wordlist set-current

: n! ( n*x n a -- )   swap cells bounds ?do i ! cell +loop ;
: order! ( n*wid n -- )   n>r 0 nr> 1+ context n! ;

: only   ['] only dup 2 order! ;
: ?voc-limit   dup 8 > abort" Max 8 contexts allowed" ;
: set-order   dup -1 = if drop only else ?voc-limit order! then ;
:noname   get-order 1+ over swap set-order ; is also
:noname   get-order 1- ?dup if nip order! else only then ; is previous
: order   ." Order: " context begin dup @ ?dup while id. cell+ repeat
   drop cr ." Current: " current @ id. cr ;

( only ) set-current
: only only ;
: set-order set-order ;
only forth definitions

( Traditional vocabulary words. )

: body>xt      [ 0 >body ] literal - ;
: vocabulary   create (wordlist)  does> body>xt context ! ;
: vocs         voc-link begin @ ?dup while dup id. >body cell+ repeat ;
