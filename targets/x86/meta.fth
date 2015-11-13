require search.fth

hex
08048000 constant load-address
load-address 54 + constant entry-point

vocabulary compiler

vocabulary t-words
defer t,
: t-word ( a u xt -- ) -rot "create , does> @ t, ;
: t' ( a u -- xt ) also t-words find-name previous drop >body @ ;
: t-compile ( "name" -- ) parse-name t' postpone literal postpone t, ; immediate

vocabulary meta
only forth also meta definitions
include lib/image.fth

0 value latest

' , is t,

: link, ( nt -- ) latest ,  to latest ;
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

host

only forth also meta definitions

: >mark   here 0 , ;
: >resolve   here swap ! ;
: t-literal   t-compile (literal) , ;

action-of number constant h-number
: ?number,   if 2drop undef else drop t-literal 2drop then ;
: t-number ( a u -- ) 0 0 2over >number nip ?number, ;

: h: : ;

h: ]   only t-words also compiler  ['] t-number is number ;
h: :   parse-name header, 0 , ] ;
h: create   parse-name header, 0 , ;
h: variable   create cell allot ;
h: forward:   parse-name 0 t-word ; \ TODO: dummy

0 constant jmp_buf

only forth also meta also compiler definitions

h: h; postpone ; ; immediate

h: [   target  h-number is number h;
h: ;   t-compile exit [ h;
h: if   t-compile 0branch >mark h;
h: then   >resolve h;
h: else   t-compile branch >mark swap >resolve h;
h: literal   t-literal h;

target

:noname 2dup type space (parsed) ; is parsed
include kernel.fth

;elf

target-region type bye

also t-words definitions words

target-region hex dump bye
