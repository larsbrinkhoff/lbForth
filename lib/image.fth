\ Compilation to a target image.

\ Define a target image, and words to access its contents.

\ The image can be divided into sections, each with their own address
\ range.  ORG sets the dictionary pointer, and also creates a new section.


4 constant t-cell
4000 t-cell * constant t-size

create t-image  t-size allot  t-image t-size erase

\ The currently active section.

variable prev  0 prev !
variable start  0 start !
variable t-dp  0 t-dp !
variable t-delta  t-image t-delta !

\ Store current section and start a new one.

: prev,   here prev @ , prev ! ;
: start,   start @ , ;
: end,   t-dp @ , ;
: delta,   t-delta @ , ;
: section,   prev, start, end, delta, ;

\ Access fields in a stored section.

: start@   cell+ @ ;
: end@   2 cells + @ ;
: delta@   3 cells + @ ;
: range@   dup start@ swap end@ ;

\ Translate a target address to a host address.  This requires finding
\ the section which contains the target address.

variable stop  0 stop !
: section?   2dup range@ within ;
: ?section   section? if delta@ + stop then ;
: search   prev begin @ ?dup while ?section repeat ;
: t-end   t-image t-size + t-delta @ - ;
: current?   start @ t-end within ;
: >host   dup current? if t-delta @ + else search then ;

\ Read and write a target character.

: t-c@   >host c@ ;
: t-c!   >host c! ;

\ Move the target dicionary pointer, and start a new section.

: delta!   t-dp @ over - t-delta +! ;
: start!   dup t-dp ! start ! ;
: t-org   section, delta! start! ;

\ Define a word to cross compile to the target image.

require lib/cross.fth
image: target-image  t-cell t-c@ t-c! t-dp t-org

\ Returns the host address and size of the target image.

: target-region ( -- a u ) t-image t-dp @ >host over - ;
