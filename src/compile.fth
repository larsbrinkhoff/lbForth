\ Metacompile the kernel.  Copyright 2017 Lars Brinkhoff.

: h-[defined] postpone [defined] ;
: h-[undefined] postpone [undefined] ;

include lib/meta.fth

only forth definitions

: h: : ;

also meta definitions

s" " searched
s" src/" searched
include params.fth
: >link   next-offset + ;
: >code   code-offset + ;
: >body   body-offset + ;

h-[defined] does-offset constant has-does?
h-[undefined] code@ [if] : code@ >body ; [then]

only forth also meta definitions

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

include asm.fth

load-address org
exe-header

include nucleus.fth

host also meta definitions

: >mark   here 0 , ;
: <mark   here ;
: >resolve   here swap ! ;
: <resolve   , ;

' docol code@ to 'docol
' dovar code@ to 'dovar
' docon code@ to 'docon
' dodef code@ to 'dodef
' dodoes code@ to 'dodoes

h: :   parse-name header, docol, ] ;
h: create   parse-name header, dovar, ;
h: variable   create cell allot ;
h: defer   parse-name header, dodef, compile abort ;
h: constant   parse-name header, docon, , ;
h: value   constant ;
h: immediate   latest >nfa dup c@ negate swap c! ;
h: to   ' >body ! ;
h: is   ' >body ! ;

h: ?:   already-defined? if ignore-definition else : then ;

only forth also meta also compiler definitions previous

h: ;   compile exit [compile] [ ;

h: [']   ' t-literal ;
h: [char]   char t-literal ;
h: literal   t-literal ;
h: compile   ' t-literal compile , ;
h: [compile]   ' , ;
h: does>   compile (does>) ;

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

h: s"   compile (sliteral) parse" dup , ", ;
h: ."   [compile] s" compile type ;

h: if   compile 0branch >mark ;
h: ahead   compile branch >mark ;
h: then   >resolve ;

h: begin   <mark ;
h: again   compile branch <resolve ;
h: until   compile 0branch <resolve ;

h: else   [compile] ahead swap [compile] then ;
h: while    [compile] if swap ;
h: repeat   [compile] again [compile]  then ;

h: to   ' >body t-literal compile ! ;
h: is   [compile] to ;

h: do   0leaves  compile 2>r  [compile] begin ;
h: loop   compile (loop) [compile] until  here leaves@ chains!  compile 2rdrop ;
h: leave   compile branch  leaves chain, ;

h: abort"   [compile] if [compile] s" compile cr compile type
   compile cr compile abort [compile] then ;

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
