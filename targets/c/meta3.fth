( Metacompiler for C target. )

(* Need to support these words:
 * ( \ [IF] [ELSE] [THEN] [DEFINED] [UNDEFINED] INCLUDE
 * : ; IMMEDIATE DOES> DEFER CODE END-CODE
 * VARIABLE VALUE CREATE ALLOT , ' CELLS >CODE @ INVERT RSHIFT = CHAR -
 * [CHAR] ['] [ ] LITERAL POSTPONE IS TO  ." S"
 * IF ELSE THEN DO LEAVE LOOP +LOOP BEGIN AGAIN WHILE REPEAT UNTIL
 * CELL JMP_BUF NAME_LENGTH TO_NEXT TO_CODE TO_DOES TO_BODY
 *)

require search.fth
only forth definitions  decimal

vocabulary target           \ Holds compiled target words.
vocabulary host-compiler    \ Overrides metacompiler definitions.
vocabulary host-interpreter \ Overrides metacompiler definitions.
vocabulary meta-compiler    \ Words executed in interpretation state.
vocabulary meta-interpreter \ Immediate words used in compilation state.

: interpreter-context   only also meta-interpreter ;
: compiler-context   only previous target also meta-compiler ;
: target-context   only previous target ;
: meta-context   only previous meta-compiler also meta-interpreter ;

: [h   r> get-order n>r  only forth >r ;
: h]   r> nr> set-order >r ;
: [H]  [h ' h] compile, ; immediate
: [m   r> get-order n>r  meta-context >r ;
: m]   r> nr> set-order >r ;
: [M]  [m ' m] compile, ; immediate
: [t   r> get-order n>r get-current >r  target-context definitions >r ;
: t]   r> r> set-current nr> set-order >r ;
: [T]  postpone [t ' compile, postpone t] ; immediate

: input>r   r> save-input n>r >r ;
: r>input   r> nr> restore-input abort" Restore-input?" >r ;

: copy   >in @ >r : r> >in !  ' compile, postpone ; ;
: immediate:   copy immediate ;

create code-line  128 allot
create t-dictionary  17000 allot

: ?resolve ( a u )   [T] find-name if ." RESOLVE-FORWARD:" dup id.
      0 over c!  drop \ TODO: resolve previous references.
   else 2drop then ;



( Host words to override defining words in metacompiler. )

only also host-interpreter definitions

: ]   ] interpreter-context also host-interpreter ;
: :   : interpreter-context also host-compiler ;

copy ,       copy '         copy allot     copy defer  copy immediate
copy create  copy variable  copy constant  copy value  
\ here align [defined] [undefined]

cr .( HOST INTERPRETER WORDS: ) cr  words

( Host words to override compiling words in metacompiler. )

only also host-compiler definitions

: [   postpone [ interpreter-context also host-compiler ; immediate
: ;   postpone ; interpreter-context also host-interpreter ; immediate

immediate: [defined]  immediate: [undefined]
immediate: literal    immediate: [']          immediate: postpone
immediate: is         immediate: to
immediate: abort"     immediate: [char]

immediate: (       immediate: if       immediate: else   immediate: \
immediate: [if]    immediate: [else]   immediate: [then]
immediate: then    immediate: begin    immediate: until  immediate: while
immediate: repeat  immediate: again    immediate: do     immediate: leave
immediate: loop    immediate: ."       immediate: s"

cr .( HOST COMPILING WORDS: ) cr  words



( Metacompiler interpreter words. Primarily defining words. )

(* [DEFINED] [UNDEFINED] INCLUDE
 * : IMMEDIATE DOES> DEFER CODE END-CODE
 * VARIABLE VALUE CREATE ALLOT , ' CELLS >CODE
 * CELL JMP_BUF NAME_LENGTH
 *
 * From host:
 * ( \ [IF] [ELSE] [THEN]  INVERT RSHIFT CHAR @ = -
 *)

interpreter-context definitions also host-interpreter

include params.fth
cell-size constant cell
jmp_buf-size constant jmp_buf
name-size constant name_length
next-offset constant to_next
code-offset constant to_code
does-offset constant to_does
body-offset constant to_body

0 value latestxt
( *** Target compiler included here. *** )
include c.fth
variable forth
0 forth !
' forth current !
t-dictionary dp !
             
: >mark   here 0 , ;
: >resolve   here swap ! ;

: cells   cell * ;

t-dictionary value there
: (.def)   space here there - .  here to there
   cr type space  >in @ parse-name type >in ! ;
: .def   parse-name postpone sliteral postpone (.def) ; immediate

\ Possibly, intercept definitions and create a corresponding
\ definition in the host.
\ : header,   >in @ [T] create >in !  header, ;

: header,   >in @ parse-name ?resolve >in !  header, ;

: create   .def CREATE  0 header, reveal ;

: code   .def CODE  0 header, reveal
   \ ." xt_t * REGPARM "
   \ latestxt >name type
   \ ." _code (xt_t *IP, struct word *word)" cr
   \ ." {" cr
   begin
      refill 0= abort" Refill?" source s" end-code" compare
   while
   \   source type cr
   repeat
   \ ." }" cr ;
;
: end-code   ;

: find-name   #name min 2dup ['] forth search-wordlist dup if 2nip then ;

only forth definitions
: forward ( a u -- xt )   ."  FORWARD:" 2dup type  input>r string-input
   [T] create latestxt  r>input ;

interpreter-context definitions also host-interpreter

: forward,    forward compile, ;
finders target-xt   compile, forward, abort
: target, ( a u -- )   find-name target-xt ;

only forth definitions also meta-interpreter also host-interpreter
finders tpp   compile, forward, abort
: (t-postpone)   find-name tpp ;
: ppt   drop postpone sliteral postpone (t-postpone) ;
: ppn   drop ppt ;
: pph   [H] compile, 2drop ;
finders pp   ppt ppn pph
: t-postpone   parse-name 2dup meta-context [H] find-name
   interpreter-context also host-compiler pp ; immediate
interpreter-context definitions also host-interpreter

: postpone,   t-postpone (literal) , t-postpone compile, ;
finders meta-postpone   postpone, forward, compile,

: ]   1 state !  compiler-context ;
: constant   .def CONSTANT  0 header, , reveal ;
: variable   .def VARIABLE  0 header, 0 , reveal ;
: :   .def COLON  0 header, ] ;
: defer   .def DEFER  0 header, 0 , reveal ;
: value   .def VALUE  0 header, , reveal ;

: '   parse-name find-name 0=
   if cr ." Undefined, and cannot tick: " type cr abort then ;
: immediate   ."  IMMEDIATE " ;

: [undefined]   parse-name find-name if drop 0 else 2drop -1 then ; immediate
: [defined]     postpone [undefined] 0= ; immediate

: ?end ( xt nt -- nt 0 | xt 1 )   2dup >nextxt = if nip 0 else drop 1 then ;
: >end ( xt -- a )   ['] forth ['] ?end traverse-wordlist ;


cr .( META-INTERPRETER WORDS: ) cr
previous words cr



( Metacompiler compilation words. )

(* ( \ [IF] [ELSE] [THEN] [DEFINED] [UNDEFINED]
 * ; DOES>
 * [CHAR] ['] [ LITERAL POSTPONE TO IS ." S" ABORT"
 * IF ELSE THEN DO LEAVE LOOP +LOOP BEGIN AGAIN WHILE REPEAT UNTIL
 * CELL
 *)

only also meta-interpreter also meta-compiler definitions also host-interpreter

: [undefined]   parse-name find-name if drop 0 else 2drop -1 then ; immediate
: [defined]     postpone [undefined] 0= ; immediate

: [   0 state !  interpreter-context ; immediate
: ;   reveal t-postpone exit t-postpone [ ; immediate
\ : ;code   postpone [ ... ; immediate
: literal   t-postpone (literal) , ; immediate
: cell   cell t-postpone literal ; immediate
: name_length   name_length t-postpone literal ; immediate
: to_next   to_next t-postpone literal ; immediate
: to_code   to_code t-postpone literal ; immediate
: to_does   to_does t-postpone literal ; immediate
: to_body   to_body t-postpone literal ; immediate
: [']   t-postpone (literal) parse-name target, ; immediate
: is   t-postpone (literal) parse-name target, t-postpone >body
   t-postpone ! ; immediate
: to   t-postpone (literal) parse-name target, t-postpone >body
   t-postpone ! ; immediate

: if   t-postpone 0branch >mark ; immediate
: then   >resolve ; immediate
: ahead   t-postpone branch >mark ; immediate
: else   t-postpone ahead swap t-postpone then ; immediate

: postpone   ." POSTPONE " parse-name find-name meta-postpone ; immediate

: s"   t-postpone (s")  [char] " parse  dup ,  string, ; immediate
: ."   [M] s"  t-postpone type ; immediate

only also meta-compiler definitions previous

: abort"   postpone if postpone s" postpone (abort") postpone then ; immediate
: [char]   char postpone literal ; immediate

immediate: (       immediate: \
immediate: [if]    immediate: [else]   immediate: [then]
immediate: begin    immediate: until  immediate: while
immediate: repeat  immediate: again    immediate: do     immediate: leave
immediate: loop

immediate: does>

cr .( META-COMPILER WORDS: ) cr
also meta-compiler words cr previous


only forth definitions

: ?compile,   state @ if [M] compile, else execute then ;

: ?literal,   state @ if [M] literal then ;

: meta-number  2>r 0 0 2r@ >number nip if 2drop 2r> [M] target,
   else 2r> 3drop ?literal, then ;

finders meta-xt   ?compile, meta-number execute

: ?.name   state @ if space 2dup type then ;

\ 1. Search host order.  If found, always execute!
\ 2. If not found, search target dictionary.  If found, always compile!

: meta-interpret   begin parse-name  ?.name  dup while
   find-name meta-xt ?stack repeat 2drop ;

: meta-loop   begin refill while meta-interpret repeat ;

: meta-compile   parse-name r/o open-file abort" Open?"  input>r
   file-input interpreter-context meta-loop
   source-id close-file abort" Close?"  r>input ;

interpreter-context definitions also host-interpreter
: include   .def INCLUDE  meta-compile ;

only forth definitions also meta-interpreter also host-interpreter
: t-id.     >name type space ;
: t-.nt     t-id. 1 ;
: t-words   ['] forth ['] t-.nt traverse-wordlist ;
: t-used    here t-dictionary - ;
: t'        parse-name ['] forth search-wordlist 0= abort" Unknown?" ;
: t-xt?     1 ['] forth ['] xt?? traverse-wordlist nip 0= ;

: t-disassemble   dup . dup t-xt? if t-id. else drop then ;
: t-see-line   cr dup .addr  @ t-disassemble ;
: body-bounds   dup >end swap >body ;
: t-see-xt   ." : " dup t-id.  body-bounds do i t-see-line cell +loop ;
: t-see   t' t-see-xt ." ;" cr ;



( Start metacompilation. )

interpreter-context
meta-compile targets/c/nucleus.fth
meta-compile kernel.fth

cr .( TARGET DICTIONARY: ) cr
only definitions
t-words cr
." Used: " t-used .
