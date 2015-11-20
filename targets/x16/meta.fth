require search.fth

hex
0 constant load-address
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

: h@   dup 1+ c@ 8 lshift swap c@ + ;
: h!   2dup c!  swap 8 rshift swap 1+ c! ;
: h,   here h!  2 allot ;

' h, is t,

include targets/x16/params.fth
: >link   next-offset + ;
: >does   does-offset + ;
: >code   code-offset + ;
: >body   body-offset + ;

: link, ( nt -- ) latest h,  to latest ;
: reveal ;
: name, ( a u -- ) #name min c,  #name ", ;
: header, ( a u -- ) align here 3dup t-word >r name, r> link, 0 h, ;
: ?code, ( -- ) here 2 + h, ;

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

include targets/x16/nucleus.fth

host

only forth also meta definitions

variable leaves

0 constant jmp_buf

: >mark   here 0 h, ;
: <mark   here ;
: >resolve   here swap h! ;
: <resolve   h, ;
: t-literal   t-compile (literal) h, ;

action-of number constant h-number
: ?number,   if 2drop undef else drop t-literal 2drop then ;
: t-number ( a u -- ) 0 0 2over >number nip ?number, ;

s" docol" t' >body constant 'docol
s" dovar" t' >body constant 'dovar

: h: : ;

h: '   parse-name t' ;
h: ]   only t-words also compiler also forward-refs  ['] t-number is number ;
h: :   parse-name header, 'docol h, ] ;
h: create   parse-name header, 'dovar h, ;
h: variable   create 2 allot ;
h: defer   parse-name header, 0 h, 0 h, ;
h: constant   parse-name header, 0 h, 0 h, ;
h: value   constant ;
h: immediate   latest dup c@ negate swap c! ;

only forth also meta also compiler definitions previous

h: \   postpone \ ;
h: (   postpone ( ;
h: [if]   postpone [if] ;
h: [else]   postpone [else] ;
h: [then]   postpone [then] ;

h: [   target  h-number is number ;
h: ;   t-compile exit t-[compile] [ ;

h: [']   ' t-literal ;
h: [char]   char t-literal ;
h: literal   t-literal ;
h: compile   ' t-literal 0 h, ;
h: [compile]   ' h, ;
h: postcode   ' 0 + t-literal 0 h, ;
h: does>   0 h, ;

h: cell   2 t-literal ;
h: TO_NEXT   0 t-literal ;
h: TO_DOES   0 t-literal ;
h: TO_CODE   0 t-literal ;
h: TO_BODY   0 t-literal ;
h: NAME_LENGTH   0 t-literal ;

h: s"   t-compile (sliteral) parse" dup h, ", ;
h: ."   t-[compile] s" 0 h, ;

h: if   t-compile 0branch >mark ;
h: ahead   t-compile branch >mark ;
h: then   >resolve ;

h: begin   <mark ;
h: again   t-compile branch <resolve ;
h: until   t-compile 0branch <resolve ;

h: else   t-[compile] ahead swap t-[compile] then ;
h: while    t-[compile] if swap ;
h: repeat   t-[compile] again t-[compile]  then ;

h: is   ' 0 + t-literal t-compile ! ;
h: to   ' 0 + t-literal t-compile ! ;

h: do   0 leaves !  t-compile 2>r  t-[compile] begin ;
h: loop   t-compile (loop) t-[compile] until  here leaves @ chains! ;
h: leave   t-compile branch  leaves chain, ;

h: abort"   t-[compile] if t-[compile] s" 0 h, 0 h, 0 h, 0 h, t-[compile] then ;

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
