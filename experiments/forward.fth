: >resolve-xt@ ( xt a -- )
   swap >r @ begin ?dup while
      dup @ r@ rot !
   repeat r> drop ;

: forward ( "name" -- )    create immediate 0 , does> >mark! ;
: resolve: ( "name" -- )   >in @ ' dup >body >resolve-xt@ >in ! : ;

\ Usage:
forward foo
: bar          foo 1+ . ;
resolve: foo   42 ;
bar
