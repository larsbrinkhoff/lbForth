code cold
   here entry-point

   hex
   ' sp0 >body # x5 lui,
   ' sp0 >body # x5 x5 addi,
   x5 ) S lw,

   ' rp0 >body # x5 lui,
   ' rp0 >body # x5 x5 addi,
   x5 ) R lw,

   ' turnkey # W lui,
   ' turnkey # W W addi,
   execute,
   decimal
end-code

latest ' latest0 >body !

\ Start of image.
load-address ' image0 >body !

\ Start of free dictionary.
here ' dp0 >body !

\ Allocate space for dictionary and stacks.
here
   decimal
   18000 cells allot  here ' limit >body !
   64 cells allot  here ' sp0 >body !
   128 cells allot  here ' rp0 >body !
here - dup allot negate extra-bytes
