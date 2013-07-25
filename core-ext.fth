\ -*- forth -*- Copyright 2004, 2013 Lars Brinkhoff

: .(   [char] ) parse type ; immediate

: 0<>   if -1 else 0 then ;
: 0>    0 > ;

: 2r@   r> 2r> 2dup 2>r rot >r ;

: :noname   s" " header, [ ' enter >code @ ] literal ,  ] latestxt !csp ;

: (?do)   r> 2r> 2dup > rot rot 2>r swap >r ;
: ?do     leaves @  0 leaves !
   postpone 2>r postpone begin postpone (?do) postpone if ; immediate

: string+   count + aligned ;
: (c")      r> dup string+ >r ;
: c"        postpone (c")  [char] " parse  dup c,  string, ; immediate

: convert   char+ 65535 >number drop ;

: case      0 ; immediate
: of        postpone over  postpone =  postpone if  postpone drop ; immediate
: endof     postpone else  swap 1+ ; immediate
: endcase   postpone drop  0 ?do postpone then loop ; immediate

: erase   0 fill ;

variable span
: expect   accept span ! ;

: true    -1 ;
: false   0 ;

: hex   16 base ! ;

\ TODO: marker

: pick   ?dup if swap >r 1- recurse r> swap exit then dup ;
: roll   ?dup if swap >r 1- recurse r> swap then ;

: query   terminal-input  refill drop ;

: value   create ,  does> @ ;
: to      ' >body ! ;
: to      ' >body postpone literal postpone ! ; immediate compile-only
: +to     ' >body +! ;
: +to     ' >body postpone literal postpone +! ; immediate compile-only

: tuck    swap over ;

: (.r) ( n f u -- )   0 <# #s rot sign #> rot over - spaces type ;
: u.r   0 rot (.r) ;
: .r   swap s>d swap abs (.r) ;

: u>   swap u< ;

: unused   end_of_dictionary @ here - ;

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

: holds   bounds swap begin 2dup < while 1- dup c@ hold repeat 2drop ;

\ TODO: s\"
