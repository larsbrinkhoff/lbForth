\ Test assembler and nucleus by loading into running Forth.
\ The existing CODE words will be patched to point to the
\ new nucleus.

require targets/x86/asm.fth

\ Redefine CODE and END-CODE to overwrite the code field of the
\ previously defined code word with the same name.
: code   source type  >in @ ' >code swap >in !  code  here ;
also assembler definitions
: end-code   here over - hex cr cdump  end-code  latestxt >code @ swap ! ;

only forth definitions
vocabulary newcleus
also newcleus definitions

include targets/x86/nucleus.fth
only forth definitions
