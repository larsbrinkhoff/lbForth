\ Metacompiler for x86.  Copyright Lars Brinkhoff 2015.

require search.fth

hex
08048000 constant load-address
load-address 54 + constant entry-point
decimal

vocabulary compiler

vocabulary t-words
defer t,
: t-word ( a u xt -- ) -rot "create , does> @ t, ;
: t' ( "name" -- xt ) also t-words parse-name find-name previous drop >body @ ;
: t-compile ( "name" -- ) t' postpone literal postpone t, ; immediate
: t-[compile] ( "name" -- ) also compiler ' previous compile, ; immediate

vocabulary meta
only forth also meta definitions
include lib/image.fth

0 value latest

' , is t,

include targets/x86/params.fth
: >link   next-offset + ;
: >does   does-offset + ;
: >code   code-offset + ;
: >body   body-offset + ;

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
: <mark   here ;
: >resolve   here swap ! ;
: <resolve   , ;
: t-literal   t-compile (literal) , ;

action-of number constant h-number
: ?number,   if 2drop undef else drop t-literal 2drop then ;
: t-number ( a u -- ) 0 0 2over >number nip ?number, ;

t' docol >body constant 'docol
t' dovar >body constant 'dovar

: h: : ;

h: '   t' ;
h: ]   only t-words also compiler also forward-refs  ['] t-number is number ;
h: :   parse-name header, 'docol , ] ;
h: create   parse-name header, 'dovar , ;
h: variable   create cell allot ;

0 constant jmp_buf

only forth also meta also compiler definitions previous

h: \   postpone \ ;
h: (   postpone ( ;
h: [if]   postpone [if] ;
h: [else]   postpone [else] ;
h: [then]   postpone [then] ;

h: [   target  h-number is number ;
h: ;   t-compile exit t-[compile] [ ;

h: cell   cell t-literal ;
h: TO_NEXT   next-offset t-literal ;
h: TO_DOES   does-offset t-literal ;
h: TO_CODE   code-offset t-literal ;
h: TO_BODY   body-offset t-literal ;
h: NAME_LENGTH   name-size t-literal ;

h: s"   t-compile (sliteral) parse" dup , ", ;

h: if   t-compile 0branch >mark ;
h: ahead   t-compile branch >mark ;
h: then   >resolve ;
h: literal   t-literal ;

h: begin   <mark ;
h: again   t-compile branch <resolve ;
h: until   t-compile 0branch <resolve ;

h: else   t-[compile] ahead swap t-[compile] then ;
h: while    t-[compile] if swap ;
h: repeat   t-[compile] again t-[compile]  then ;

target

\ :noname 2dup type space (parsed) ; is parsed
\ include kernel.fth
include test/test-meta.fth
also t-words resolve-all-forward-refs previous

;elf

target-region type bye

host also meta

cr .( Target size: ) t-size .
cr .( Target used: ) target here host also meta >host t-image host - .
cr .( Host unused: ) unused .
cr .( Target words: ) also t-words words only forth
cr .( Forward refs: ) also meta also forward-refs words
cr

target-region hex dump bye
