( Search-Order words. )

: forth-wordlist   ['] forth ;

: get-current   current @ ;

: get-order   0 context begin dup @ ?dup while
                drop cell+  under 1+
              repeat swap >r
              begin /cell - dup @ swap dup context = until drop r> ;

: set-current   current ! ;

: definitions   context @ set-current ;

\ TODO: wordlist

( Search-Order extension words. )

: forth   forth-wordlist context ! ;

: set-order   dup 0 = if ( todo ) abort then
              dup -1 = if ( todo ) abort else
              dup 4 > abort" Max 4 context allowed"
              n>r 0 nr> 1+
              context swap cells bounds
	      do i ! /cell +loop then ;

: only   forth-wordlist forth-wordlist 2 set-order ;

:redefine also   get-order 1+ over swap set-order ;

:redefine previous   get-order 1- nip set-order ;

: order   ." Order:"
          context begin dup @ ?dup while
             bl emit >name type cell+
          repeat drop cr
          ." Current: " current @ >name type ;

( Traditional vocabulary words. )

: body>xt   [ 0 >body ] literal - ;

: vocabulary   create 0 ,  does> body>xt context ! ;
