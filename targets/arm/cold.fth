also assembler
label 'sp0  ' sp0 >body ,
label 'rp0  ' rp0 >body ,
label 'turnkey  ' turnkey ,
previous

code cold
   here entry-point

   'sp0 r0 ldr,
   r0 ) S ldr,
   'rp0 r0 ldr,
   r0 ) R ldr,

   'turnkey W ldr,
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
