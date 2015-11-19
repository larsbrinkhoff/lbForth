require search.fth

hex
08048000 constant load-address
load-address 54 + constant entry-point
decimal

vocabulary compiler

vocabulary t-words
defer t,
: t-word ( a u xt -- ) -rot "create , does> @ t, ;
: t' ( a u -- xt ) also t-words find-name previous drop >body @ ;
: t-compile ( "name" -- ) parse-name t' postpone literal postpone t, ; immediate
: t-[compile] ( "name" -- ) also compiler ' previous compile, ; immediate

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
include lib/xforward.fth

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

s" docol" t' 28 + constant 'docol
s" dovar" t' 28 + constant 'dovar

: h: : ;

h: '   parse-name t' ;
h: ]   only t-words also compiler also forward-refs  ['] t-number is number ;
h: :   parse-name header, 'docol , ] ;
h: create   parse-name header, 'dovar , ;
h: variable   create cell allot ;

0 constant jmp_buf

only forth also meta also compiler definitions previous

h: [   target  h-number is number ;
h: ;   t-compile exit t-[compile] [ ;
h: if   t-compile 0branch >mark ;
h: else   t-compile branch >mark swap >resolve ;
h: then   >resolve ;
h: literal   t-literal ;

target

\ :noname 2dup type space (parsed) ; is parsed
\ include kernel.fth
include test/test-meta.fth
also t-words resolve-all-forward-refs previous

;elf

target-region type bye

cr .( Target words: ) also t-words definitions words
cr .( Forward refs: ) also forward-refs words
cr

target-region hex dump bye
