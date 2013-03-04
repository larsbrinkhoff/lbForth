\ -*- forth -*- Copyright 2004, 2013 Lars Brinkhoff

\ TODO:   .r

: .( ( "<string><paren>" -- )
    [char] ) parse type ; immediate

: 0<>   if -1 else 0 then ;

: 0> ( n -- flag )   0 > ;

: 2r@ ( -- x1 x2 ) ( R: x1 x2 -- x1 x2 )   2r> 2dup 2>r ;

: :noname   align here  0 c, 15 allot  link,
            [ ' enter >code @ ] literal , 0 , ] lastxt @ !csp ;

: (?do)   r> 2r> 2dup > rot rot 2>r swap >r ;
: ?do     leaves @  0 leaves !
          postpone 2>r postpone begin postpone (?do) postpone if ; immediate

: string+ ( caddr -- addr )
    count + aligned ;

: (c") ( -- caddr ) ( R: ret1 -- ret2 )
    r> dup string+ >r ;

: c" ( "<string><quote>" -- caddr )
    postpone (c")  [char] " parse  dup c,  string, ; immediate

: case ( -- ) ( C: -- case-sys )
    0 ;

\ TODO: convert

: endcase ( x -- ) ( C: case-sys -- )
    0 do  postpone then  loop
    postpone drop ;

: endof ( -- ) ( C: case-sys1 of-sys -- case-sys2 )
    postpone else  swap 1+ ;

\ TODO: erase
\ TODO: expect

: false ( -- 0 )
    0 ;

: hex ( -- )
    16 base ! ;

\ TODO:   marker

: of ( x x -- | x y -- x ) ( C: -- of-sys )
    postpone over  postpone =  postpone if  postpone drop ;

: pick   1+ cells sp@ + @ ;

: query ( -- )
    tib ''source !  #tib ''#source !  0 'source-id !
    refill drop ;

\ TODO: roll ( xn xn-1 ... x0 n -- xn-1 ... x0 xn )
\     n>r r> begin ?dup while r> -rot 1- repeat ;

\ TODO:   span

: to   ' >body postpone literal postpone ! ; immediate

: true ( -- -1 )
    -1 ;

: tuck ( x y -- y x y )
    swap over ;

\ TODO: u.r

: u>   swap u< ;

\ TODO: unused

: value   create ,  does> @ ;

: within   over - under - u< ;

\ TODO: [compile]

\ ----------------------------------------------------------------------

( Forth2012 core extension words. )

\ TODO: buffer:

: defer       create ['] abort ,  does> @ execute ;
: defer!      >body ! ;
: defer@      >body @ ;
: is          ' defer! ;
: is          postpone ['] postpone defer! ; immediate compile-only
: action-of   ' defer@ ; immediate
: action-of   postpone ['] postpone defer@ ; immediate compile-only

\ TODO: holds

\ TODO: parse-name ( "name" -- caddr u )

\ TODO: s\"
