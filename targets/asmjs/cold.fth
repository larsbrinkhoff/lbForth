host

also meta ' latest0 previous  constant 'latest0
also meta ' turnkey previous  constant 'turnkey
also meta ' limit previous  constant 'limit
also meta ' sp0 previous  constant 'sp0
also meta ' rp0 previous  constant 'rp0
also meta ' dp0 previous  constant 'dp0

: final
   ." HEAPU32[" 'latest0 . ." +28>>2] = " 'turnkey . ." ;" cr
   ." HEAPU32[" 'limit . ." +28>>2] = params.sp0;" cr
   ." HEAPU32[" 'sp0 . ." +28>>2] = params.sp0;" cr
   ." HEAPU32[" 'rp0 . ." +28>>2] = params.rp0;" cr
   ." HEAPU32[" 'dp0 . ." +28>>2] = params.dictoff;" cr
   ." run(" 'turnkey . ." );" cr ;

: mask   0 8 4 * 0 do 1 lshift 1 + loop and ;
: rrotate ( u1 u2 -- u3 ) 2dup rshift -rot 32 - negate lshift + ;
: 4@   0 swap 4 bounds do i c@ + 8 rrotate loop mask ;

: [meta]   also meta ; immediate
: [host]   previous ; immediate
: t-dp   [meta] t-dp [host] ;
: >host   [meta] >host [host] ;

: .cell   ." HEAPU32[" swap . ." >> 2] = " u. ." ;" cr ;
: ?.cell   dup if .cell else 2drop then ;
also meta definitions previous
: save-target   t-dp @ 1024 do i dup >host 4@ ?.cell 4 +loop final ;
