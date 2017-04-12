\ Copyright 2017 Pip Cet

\ Assembler for asm.js

vocabulary assembler

variable scount  0 scount !

: ?refill   refill 0= abort" Refill?" ;
: more?   source s" end-code" compare ;
: start-code   begin ?refill more? while source type cr repeat ;

: end-code ;

: header   parse-name header, 'dodoes , scount @ , reveal ;
: .case   ." case " scount ? ." :" cr 1 scount +! ;
: .break   ."     break;" cr cr ;
: code   header .case start-code .break ;
