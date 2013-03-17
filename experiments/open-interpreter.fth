\ http://forth.sourceforge.net/wordset/open-interpreter/

: >rr      >r ;
: rr>      r> ;
: rr@      r@ ;
: rrdrop   r> drop ;
: >rr<     r> swap >r ;
: rush     execute ;

: r-restore-sys ;
: r-save-sys    ;
: copy>rr       dup >r ;
: raddr@        @ ;
: raddr!        ! ;
: raddr+        cell+ ;
: raddr-        1 cells - ;
\ rp@
\ rp!

: /allot   ;
: /here    ;
: >tcode   >body ;
: ref!     ! ;
: ref+     cell+ ;
: ref-     1 cells - ;
: ref@     @ ;
: refs     cells ;

: token!   ! ;
: token,   compile, ;
: token@   @ ;
: token+   cell+ ;
: token>   dup cell+ swap @ ;
: tokens   cells ;

: /!         ! ;
: /+         cell+ ;
: /,         , ;
: /@         @ ;
: /align     align ;
: /aligned   aligned ;
: /c!        c! ;
: /c@        c@ ;
: /cell+     cell+ ;
: /c,        c, ;
: /get       rot rot move ;
: /put       ;

: //swap   swap ;
: /xswap   swap ;
: x/swap   swap ;
