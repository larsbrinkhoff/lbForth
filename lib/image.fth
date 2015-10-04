\ Compilation to a target image.


\ Define a target image, and words to access its contents.

100 constant t-size
4 constant t-cell

create t-image  t-size allot  t-image t-size erase
variable t-delta  t-image t-delta !
: >host   t-delta @ + ;

: t-c@   >host c@ ;
: t-c!   >host c! ;

variable t-dp   0 t-dp !
: t-org   t-dp @ over - t-delta +! t-dp ! ;


\ Redirect five basic words to the target image.

variable context
: image:   create ' , ' , ' , ' , ' ,   does> context ! ;
image: host-image  cell c@ c! dp drop
image: target-image  t-cell t-c@ t-c! t-dp t-org
host-image

: redirect:   create dup , cell+  does> @ context @ + perform ;
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

: rrotate ( u1 u2 -- u3 ) 2dup rshift -rot cell 8 * - negate lshift + ;
: @   0 swap cell bounds do i c@ + 8 rrotate loop ;
: !   cell bounds do dup i c! 8 rshift loop drop ;

: c+!   dup >r c@ + r> c! ;
: c!+   tuck c! 1+ ;
: c@+   dup 1+ swap c@ ;
: c,   here c!  1 allot ;

: +!   dup >r @ + r> ! ;
: !+   tuck ! cell+ ;
: @+   dup cell+ swap @ ;
: ,   here !  cell allot ;
