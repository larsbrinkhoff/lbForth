variable dp
: here      dp @ ;
: allot     dp +! ;
: aligned   cell + 1 - cell negate nand invert ;
: align     dp @ aligned dp ! ;

: ,          here !  cell allot ;
: c,         here c!  1 allot ;
: compile,   , ;
: string,    here over allot align  swap cmove ;

variable current

: chain, ( nt wid -- )  >body dup @ , ! ;
: link, ( nt -- )       to latestxt  current @ >body @ , ;
: reveal                latestxt  current @ >body ! ;
: #name                 NAME_LENGTH 1 - ;
: name, ( a u -- )      #name min c,  #name string, ;
: header, ( code -- )   align here  parse-name name,  link, ( code ) , 0 , ;

