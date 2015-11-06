require search.fth

hex
08048000 constant load-address
load-address 54 + constant entry-point

vocabulary t-words
defer t,
: t-word ( a u xt -- ) -rot "create , does> @ t, ;
: t' ( a u -- ) also t-words find-name previous drop >body @ ;

vocabulary meta
only forth also meta definitions
include lib/image.fth

' , is t,

: link, ( nt -- ) drop 0 , ;
: reveal ;
: name, ( a u -- ) #name min c,  #name ", ;
: header, ( a u -- ) align here 3dup t-word >r name, r> link, 0 , ;
: ?code, ( -- ) here cell+ , ;

: host   only forth definitions host-image ;

include targets/x86/asm.fth
include lib/elf.fth

only forth definitions also meta
: target   only forth also meta also t-words definitions previous target-image ;

target
0 org

load-address x86 elf32,

entry-point org

include targets/x86/nucleus.fth

;elf

target-region type bye
