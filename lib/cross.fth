\ Cross compilation to a separate image.


\ This library defines separate images.  When an image is active,
\ all addresses and memory accesses will refer to that image.
\ 
\ An image is defined by five words:
\ cell - cell size,
\ c@ - read a character,
\ c! - write a character,
\ dp - dictionary pointer,
\ org - set dictionary pointer.


\ Redirect the five basic words to the currently active image.

variable 'image
: image:   create ' , ' , ' , ' , ' ,   does> 'image ! ;
image: host-image  cell c@ c! dp drop
host-image

: redirect:   create dup , cell+  does> @ 'image @ + perform ;
0
redirect: cell
redirect: c@
redirect: c!
redirect: dp
redirect: org
drop


\ Reimplement other memory access and dictionary words in terms of the
\ basic words.

: cell+   cell + ;
: cells   cell * ;
: aligned   cell + 1-  cell negate and ;

: here   dp @ ;
: allot   dp +! ;
: align   here aligned dp ! ;

: mask   0 8 cells 0 do 1 lshift 1 + loop and ;
: rrotate ( u1 u2 -- u3 ) 2dup rshift -rot 8 cells - negate lshift + ;
t-little-endian [if]
  : @   0 swap cell bounds do i c@ + 8 rrotate loop mask ;
  : !   cell bounds do dup i c! 8 rshift loop drop ;
[else]
  : @   0 swap cell bounds do 8 lshift i c@ + loop ;
  : !   cell bounds do 24 rrotate dup i c! loop drop ;
[then]

: c+!   dup >r c@ + r> c! ;
: c!+   tuck c! 1+ ;
: c@+   dup 1+ swap c@ ;
: c,   here c!  1 allot ;

: +!   dup >r @ + r> ! ;
: !+   tuck ! cell+ ;
: @+   dup cell+ swap @ ;
: ,   here !  cell allot ;

: 2@      dup cell+ @ swap @ ;
: 2!      swap over ! cell+ ! ;

: fill   -rot bounds ?do dup i c! loop drop ;
: erase   0 fill ;

: cmove   bounds do count i c! loop drop ;
: move,   here swap dup allot cmove ;
: ,"   postpone s" postpone move, ; compile-only
: ",   move, align ;
