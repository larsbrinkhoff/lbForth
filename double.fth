( Double-Number words. )

: 2constant ( x y "word" -- )
    create , ,
  does> ( -- x y )
    2@ ;

\ 2literal

: 2variable ( "word" -- )
    create 2 cells allot ;

\ d+
\ d-
\ d.
\ d.r
\ d0<
\ d0=
\ d2*
\ d2/
\ d<
\ d=
\ d>s
\ dabs
\ dmax
\ dmin
\ dnegate
\ m*/
\ m+

( Double-Number extension words. )

\ 2rot
\ du<
