\ Compilation to a target image.

100 constant t-size
4 constant t-cell

create t-image  t-size allot
variable t-delta  t-image t-delta !
: >host   t-delta @ + ;

: tc@   >host c@ ;
: tc!   >host c! ;
: t@   >host @ ;
: t!   >host ! ;

: t-cells   t-cell * ;
: t-aligned   t-cell + 1 - t-cell negate nand invert ;

variable t-dp   0 t-dp !
: t-here   t-dp @ ;
: t-allot   t-dp +! ;
: t-align   t-here t-aligned t-dp ! ;
: t-org   t-here - dup negate t-delta +! t-allot ;

: t,   t-here t!  t-cell t-allot ;
: tc,   t-here tc!  1 allot ;
