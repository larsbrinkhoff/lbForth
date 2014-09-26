require targets/x86/asm.fth

\ Redefine CODE and END-CODE to overwrite the code field of the
\ previously defined code word with the same name.
: code   source type  >in @ ' >code swap >in !  code  here ;
also assembler definitions
: end-code   here over - hex cr cdump  end-code  latestxt >code @ swap ! ;
only forth definitions

\ include targets/x86/nucleus.fth

: %sp sp ;
: next, [ also assembler ] ret, [ previous ] ;

code exit
   RP edx mov,
   edx ) eax mov,
   4 # edx add,
   edx RP mov,
   next,
end-code

code +
   8 # esp sub,
   %SP edx mov,
   ebx esp ) mov,
   esi 4 esp )# mov,
   4 edx )# ebx mov,
   4 edx )# esi lea,
   edx ) ecx mov,
   esi %SP mov,
   4 esp )# esi mov,
   ebx ecx add,
   esp ) ebx mov,
   ecx 4 edx )# mov,
   8 # esp add,
   next,
end-code
