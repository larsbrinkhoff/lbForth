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

: string+ ( c-addr -- a-addr )   count + aligned ;

: (c")   r> dup string+ >r ;
: c"     postpone (c")  [char] " parse  dup c,  string, ; immediate

\ TODO: convert

: case      0 ;
: of        postpone over  postpone =  postpone if  postpone drop ;
: endof     postpone else  swap 1+ ;
: endcase   0 do  postpone then  loop postpone drop ;

\ TODO: erase
\ TODO: expect

: true    -1 ;
: false   0 ;

: hex   16 base ! ;

\ TODO:   marker

: pick   ?dup if swap >r 1- recurse r> swap exit then dup ;
: roll   ?dup if swap >r 1- recurse r> swap then ;

: query ( -- )
    tib ''source !  #tib ''#source !  0 'source-id !
    refill drop ;

\ TODO:   span

: value   create ,  does> @ ;
: to      ' >body ! ;
: to      ' >body postpone literal postpone ! ; immediate compile-only
: +to     ' >body +! ;
: +to     ' >body postpone literal postpone +! ; immediate compile-only

: tuck    swap over ;

\ TODO: u.r

: u>   swap u< ;

\ TODO: unused

: within   over - under - u< ;

: [compile]   ' compile, ; immediate

\ ----------------------------------------------------------------------

( Forth2012 core extension words. )

: buffer:   create allot ;

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
