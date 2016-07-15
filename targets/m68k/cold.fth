code cold
   here entry-point

   exe-init

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
