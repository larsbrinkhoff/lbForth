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

require lib/cross.fth
image: target-image  t-cell t-c@ t-c! t-dp t-org
