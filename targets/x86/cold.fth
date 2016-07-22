code cold
   here entry-point

   ' sp0 >body S mov,
   ' rp0 >body R mov,

   ' turnkey # W mov,
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
   \ Large stack needed by Wine.
   2000 cells allot  here ' sp0 >body !
   128 cells allot  here ' rp0 >body !
here - dup allot negate extra-bytes
