\ Test cross compilation to a target image.

require lib/image.fth

: fail   ." FAIL: " source type cr abort ;
: check   <> if fail then ;

target-image

\ Target -2.  This definition works with both 32-bit and 64-bit host cells.
1 8 cells 1- lshift 1- 1 lshift constant t-2

\ Write a byte to target address 0.
0 org            here 0 check
0 >host          t-image check
42 c,            0 c@ 42 check

\ Advance the dictionary pointer, and write a cell to target address 10.
10 org           here 10 check
10 >host         t-image 1+ check
-2 ,             10 @ t-2 check
                 here 14 check

\ Check that the first section is still accessible.
0 >host          t-image check

\ The resulting image should be 5 bytes.
target-region    5 check t-image check
