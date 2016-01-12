\ Metacompiler for x86.  Copyright Lars Brinkhoff 2015-2016.

require search.fth

vocabulary compiler

vocabulary t-words
defer t,
: t-word ( a u xt -- ) -rot "create , does> @ t, ;
: fatal   cr source type cr bye ;
: ?undef   0= if ." Undefined!" fatal then ;
: t-search   ['] t-words search-wordlist ;
: defined?   t-search if drop -1 else 0 then ;
: "' ( u a -- xt ) t-search ?undef >body @ ;
: t'   parse-name "' ;
: t-compile   parse-name postpone sliteral postpone "' postpone t, ; immediate
: t-[compile]   also compiler ' previous compile, ; immediate
: t-literal   t-compile (literal) t, ;
: t-constant   create , does> @ t-literal ;

: already-defined?   >in @ >r parse-name defined? r> >in ! ;
: trailing-semicolon?   source 1- + c@ [char] ; = ;
: ignore-definition   begin trailing-semicolon? postpone \ until ;

variable leaves
: 0leaves   0 leaves ! ;
: leaves@   leaves @ ;

vocabulary meta
only forth also meta definitions
include lib/image.fth

0 value latest

' , is t,

include params.fth
: >link   next-offset + ;
: >code   code-offset + ;
: >body   body-offset + ;

0 value 'docol
0 value 'dovar
0 value 'docon
0 value 'dodef
0 value 'dodoes

: code,   , ;

: link, ( nt -- ) latest ,  to latest ;
: reveal ;

include t-kern.fth

: header, ( a u -- ) 2dup align here over >xt + t-word header, ;
: ?code, ( -- ) here cell+ , ;

: host   only forth definitions host-image ;

include targets/x86/asm.fth
include lib/xforward.fth

only forth definitions also meta
: target   only forth also meta also t-words definitions previous target-image ;

target
load-address org
exe-header

include targets/x86/nucleus.fth

host also meta definitions

: >mark   here 0 , ;
: <mark   here ;
: >resolve   here swap ! ;
: <resolve   , ;

: h-number   [ action-of number ] literal is number ;
: ?number,   if 2drop undef fatal else drop t-literal 2drop then ;
: number, ( a u -- ) 0 0 2over >number nip ?number, ;
: t-number   ['] number, is number ;

t' docol >body to 'docol
t' dovar >body to 'dovar
t' docon >body to 'docon
t' dodef >body to 'dodef
t' dodoes >body to 'dodoes

: h: : ;

h: '   t' ;
h: ]   only forward-refs also t-words also compiler  t-number ;
h: :   parse-name header, docol, ] ;
h: create   parse-name header, dovar, ;
h: variable   create cell allot ;
h: defer   parse-name header, dodef, t-compile abort ;
h: constant   parse-name header, docon, , ;
h: value   constant ;
h: immediate   latest >nfa dup c@ negate swap c! ;
h: to   ' >body ! ;
h: is   ' >body ! ;
h: [defined]   parse-name defined? ;
h: [undefined]   [defined] 0= ;

h: ?:   already-defined? if ignore-definition else : then ;

only forth also meta also compiler definitions previous

h: \   postpone \ ;
h: (   postpone ( ;
h: [if]   postpone [if] ;
h: [else]   postpone [else] ;
h: [then]   postpone [then] ;

h: [   target h-number ;
h: ;   t-compile exit t-[compile] [ ;

h: [']   ' t-literal ;
h: [char]   char t-literal ;
h: literal   t-literal ;
h: compile   ' t-literal t-compile , ;
h: [compile]   ' , ;
h: does>   t-compile (does>) ;

cell-size t-constant cell
next-offset t-constant TO_NEXT   
code-offset t-constant TO_CODE   
body-offset t-constant TO_BODY   

'docol t-constant 'docol   
'dovar t-constant 'dovar   
'docon t-constant 'docon   
'dodef t-constant 'dodef   
'dodoes t-constant 'dodoes   

h: s"   t-compile (sliteral) parse" dup , ", ;
h: ."   t-[compile] s" t-compile type ;

h: if   t-compile 0branch >mark ;
h: ahead   t-compile branch >mark ;
h: then   >resolve ;

h: begin   <mark ;
h: again   t-compile branch <resolve ;
h: until   t-compile 0branch <resolve ;

h: else   t-[compile] ahead swap t-[compile] then ;
h: while    t-[compile] if swap ;
h: repeat   t-[compile] again t-[compile]  then ;

h: to   ' >body t-literal t-compile ! ;
h: is   t-[compile] to ;

h: do   0leaves  t-compile 2>r  t-[compile] begin ;
h: loop   t-compile (loop) t-[compile] until  here leaves@ chains!  t-compile 2rdrop ;
h: leave   t-compile branch  leaves chain, ;

h: abort"   t-[compile] if t-[compile] s" t-compile cr t-compile type
   t-compile cr t-compile abort t-[compile] then ;

\ only forth :noname 2dup type space (parsed) ; is parsed
target

include kernel.fth
include targets/x86/cold.fth

only forth also meta also t-words resolve-all-forward-refs

only forth also meta
exe-end

target-region type bye

host also meta

cr .( Target size: ) t-size .
cr .( Target used: ) target here host also meta >host t-image host - .
cr .( Host unused: ) unused .
cr .( Target words: ) also t-words words only forth
cr .( Forward refs: ) also meta also forward-refs words
cr

target-region hex dump bye
