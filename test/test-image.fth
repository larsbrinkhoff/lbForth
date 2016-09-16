\ Test cross compilation to a target image.

require lib/image.fth
true cell section: target-image
10 t-allot

: fail   ." FAIL: " source type cr abort ;
: check   <> if fail then ;

target-image

\ Target -2.  This definition works with any host cell size.
1 8 cells 1- lshift 1- 1 lshift constant t-2

\ Write a byte to target address 0.
0 org            here 0 check
0 >host          t-image check
42 c,            0 c@ 42 check

\ Advance the dictionary pointer, and write a cell to target address 10.
10 org           here 10 check
10 >host         t-image 1+ check
-2 ,             10 @ t-2 check
                 here 10 cell + check

\ Check that the first section is still accessible.
0 >host          t-image check

\ The resulting image should be one cell and one byte large.
target-region    cell 1+ check t-image check
