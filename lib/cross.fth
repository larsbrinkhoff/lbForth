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

\ Save the host versions to stack, for the benefit of host-image below.
1 here ! here c@  ' drop ' dp ' c! ' c@ cell

variable 'image
: redirect-constant:   create dup , cell+  does> @ 'image @ + @ ;
: redirect:   create dup , cell+  does> @ 'image @ + perform ;
0
redirect-constant: cell
redirect: c@
redirect: c!
redirect: dp
redirect: org
redirect: ?brev
redirect: ?lrev
drop

: split ( u -- u1 u2 ) dup 8 rshift swap 255 and ;
: lrev ( u1 -- u2 ) 0 cell 0 do swap split rot 8 lshift + loop nip ;
: brev   swap lrev swap ;

: endian,   if ['] noop , ['] lrev , else ['] brev , ['] noop , then ;
: image:   create , , , , , endian,  does> 'image ! ;

image: host-image
host-image


\ Reimplement other memory access and dictionary words in terms of the
\ basic words.

: cell+   cell + ;
: cells   cell * ;
: aligned   cell + 1-  cell negate and ;

: here   dp @ ;
: allot   dp +! ;
: align   here aligned dp ! ;

0 [if] \ Work in progress.
: 1+-   1- ;
: >end ( a1 u -- a2 u ) tuck + 1- swap ;
: 1+-   1+ ;
: >end ( a1 u -- a2 u ) ;
: 1c!+- ( x1 a1 u1 -- x2 a2 u2 ) >r >r dup r@ c! 8 rshift r> 1+- r> 1- ;
: nc! ( x a u -- ) >end begin ?dup while 1c!+- repeat 2drop ;
: 1c@+- ( x1 a1 u1 -- x2 a2 u2 ) >r >r 8 lshift r@ c@ + r> 1+- r> 1- ;
: nc@ ( a u -- x ) 0 -rot >end begin ?dup while 1c@+- repeat drop ;
\ : !   cell nc! ;
\ : @   cell nc@ ;
[then]
: !   ?brev cell bounds do dup i c! 8 rshift loop drop ;
: @   0 swap cell bounds do 8 lshift i c@ + loop ?lrev ;

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
