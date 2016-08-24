\ -*- forth -*- Copyright 2004, 2013-2016 Lars Brinkhoff

: .(   [char] ) parse type ; immediate

: 0<>   if -1 else 0 then ;
: 0>    0 > ;

: 2r@   r> 2r> 2dup 2>r rot >r ;

: :noname   noheader, docol, ] latestxt !csp ;

: (?do)   r> 2r> 2dup > rot rot 2>r swap >r ;
: ?do   leaves @  0 leaves !
   postpone 2>r postpone begin postpone (?do) postpone if ; compile-only

: string+   count + aligned ;
: (c")   r> dup string+ >r ;
: string,   dup c, ", ;
: ,c"   parse" string, ;
: c"   postpone (c") ,c" ; compile-only

: convert   char+ 65535 >number drop ;

: case   0 ; compile-only
: of   postpone over  postpone =  postpone if  postpone drop ; compile-only
: endof   postpone else  swap 1+ ; compile-only
: endcase   postpone drop  0 ?do postpone then loop ; compile-only

: erase   0 fill ;

variable span
: expect   accept span ! ;

: true    -1 ;
: false   0 ;

: hex   16 base ! ;

: pick   ?dup if swap >r 1- recurse r> swap exit then dup ;
: roll   ?dup if swap >r 1- recurse r> swap then ;

: query   terminal-input  refill drop ;

: value   header docon, , ;
: to      ' >body ! ;
: to      ' >body postpone literal postpone ! ; compile-only
: +to     ' >body +! ;
: +to     ' >body postpone literal postpone +! ; compile-only

: @+ ( addr -- a' x )   dup cell+ swap @ ;

create voc-link  ' env-words ,

: current,   current @ , ;
: context,   context 9 cells move, ;
: latestxt,   latestxt , ;
: voc-link,   voc-link @ , ;
: vocs,   voc-link begin @ ?dup while >body dup @ , cell+ repeat ;
: marker,   current, context, latestxt, voc-link, vocs, ;

: here!   dup dp ! ;
: current!   @+ current ! ;
: context!   dup context 9 cells cmove  9 cells + ;
: latestxt!   @+ to latestxt ;
: voc-link!   @+ voc-link ! ;
: voc! ( a1 a2 -- a1' a2' ) >body >r @+ r@ ! r> ;
: vocs!   voc-link begin @ ?dup while voc! cell+ repeat ;
: marker!    here! current! context! latestxt! voc-link! vocs! drop ;

: marker   here marker, create ,  does> @ marker! ;

?: tuck    swap over ;

: (.r) ( n f u -- )   0 <# #s rot sign #> rot over - spaces type ;
: u.r   0 rot (.r) ;
: .r   swap s>d swap abs (.r) ;

: u>   swap u< ;

: unused   limit @ here - ;

: within   over - under - u< ;

: [compile]   ' compile, ; compile-only

\ ----------------------------------------------------------------------

( Forth2012 core extension words. )

: buffer:   create allot ;

: alias ( xt "name" -- ) header dodef, , ;

: defer   ['] abort alias ;
: defer!   >body ! ;
: defer@   >body @ ;
: is   ' defer! ;
: is   postpone ['] postpone defer! ; compile-only
: action-of   ' defer@ ;
: action-of   postpone ['] postpone defer@ ; compile-only
: +is ( xt "name" -- ) >r :noname r> compile, ' dup defer@ compile, >r postpone ; r> defer! ;

: holds   bounds swap begin 2dup < while 1- dup c@ hold repeat 2drop ;

\ TODO: s\"
