code cold
   \ here entry-point
    then,

   4 a7 )# d0 .l move,
   ' limit >body d1 .l move,
   ' image0 >body d1 .l sub,
   d1 a7 -) .l move,
   d0 a7 -) .l move,
   a7 -) .w clr,
   74 # a7 -) .w move,
   1 # trap, \ Mshrink

   ' sp0 >body S .l move,
   ' rp0 >body R .l move,

   ' turnkey # W .l move,
   execute,
end-code

latest ' latest0 >body !

\ Start of image.
load-address ' image0 >body !

\ Start of free dictionary.
here ' dp0 >body !

\ Allocate space for dictionary and stacks.
here
   18000 cells allot  here ' limit >body !
   2000 cells allot  here ' sp0 >body !
   256 cells allot  here ' rp0 >body !
here - dup allot negate extra-bytes
