require search.fth

hex
08048000 constant load-address
load-address 54 + constant entry-point

vocabulary meta
only forth also meta definitions
include lib/image.fth

: link, ( nt -- ) drop 0 , ;
: reveal ;
: name, ( a u -- ) #name min c,  #name ", ;
: header, ( a u -- ) align here >r name, r> link, 0 , ;
: ?code, ( -- ) here cell+ , ;

: host   only forth host-image ;

include targets/x86/asm.fth
include lib/elf.fth

only forth definitions also meta

: target   only forth also meta target-image ;

target
0 org

load-address x86 elf32,

entry-point org

include targets/x86/nucleus.fth

;elf

target-region type bye
