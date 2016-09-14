\ Metacompiler.  Copyright Lars Brinkhoff 2016-2017.

(* Use of vocabularies.

 T-WORDS holds the host definition of target words.  When executed,
 they compile an xt into the target image.  There are no immediate
 words in this vocabulary.

 Immediate compiling words must be defined in the COMPILER vocabulary.

 Words in FORWARD-REFS compile unresolved forward references to
 undefined words.  When the build is wrapped up at the end, all
 forward referenced are resolved in one go.

 The META vocabulary consists mostly of words which access the target
 image.  Those are memory accesses and defining words.  Much of the
 metacompiler implementation is put in this vocabulary, but that's for
 convenience; it's not required by design.

   Search order.

 When interpreting target code, the search order is: FORTH, META, and
 T-WORDS is current.  This means that defining words in META will be
 found first, but ordinary words fall back to the regular host words
 in FORTH.  New words are entered into T-WORDS, but they will not be
 visible outside colon definitions.

 When compiling, the search order is: FORWARD-REFS, T-WORDS, COMPILER.
 So the compiling words in the host take precendence over the
 corresponding words in the target.  And defined words take
 precendence over forward references.

 The host word TARGET and the target ] set up these search orders.
 HOST resets the search order back to the host.  *)

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

s" " searched
s" src/" searched
include params.fth
include lib/image.fth
t-little-endian cell-size section: target-image

0 value latest

' , is t,

include output.fth

4000 cell-size * t-allot

: >link   next-offset + ;
: >code   code-offset + ;
: >body   body-offset + ;

\ Does target have a DOES> field?
[defined] does-offset constant has-does?

\ Start of code in a CODE word.  Can be overridden by the target.
[undefined] code@ [if] : code@ >body ; [then]

0 value 'docol
0 value 'dovar
0 value 'docon
0 value 'dodef
0 value 'dodoes

: code,   , ;

: link, ( nt -- ) latest ,  to latest ;
: reveal ;

include target.fth

: header, ( a u -- ) 2dup align here over >xt + t-word header, ;
: ?code, ( -- ) here cell+ , ;

: host   only forth definitions host-image ;

include asm.fth
include lib/xforward.fth

only forth definitions also meta
: target   only forth also meta also t-words definitions previous target-image ;

target
load-address org
exe-header

include nucleus.fth

host also meta definitions

: >mark   here 0 , ;
: <mark   here ;
: >resolve   here swap ! ;
: <resolve   , ;

: h-number   [ action-of number ] literal is number ;
: ?number,   if 2drop undef fatal else drop t-literal 2drop then ;
: number, ( a u -- ) 0 0 2over >number nip ?number, ;
: t-number   ['] number, is number ;

target-image
t' docol code@ to 'docol
t' dovar code@ to 'dovar
t' docon code@ to 'docon
t' dodef code@ to 'dodef
t' dodoes code@ to 'dodoes

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
has-does? [if] does-offset t-constant TO_DOES [then]

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
include cold.fth

only forth also meta also t-words resolve-all-forward-refs

only forth also meta
exe-end

save-target bye

host also meta

cr .( Target size: ) t-size .
cr .( Target used: ) target here host also meta >host t-image host - .
cr .( Host unused: ) unused .
cr .( Target words: ) also t-words words only forth
cr .( Forward refs: ) also meta also forward-refs words
cr

target-region hex dump bye
