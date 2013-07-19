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

: interpreter-context   only forth also meta-interpreter ;
: compiler-context   only previous meta-compiler ;
: meta-context   compiler-context also meta-interpreter ;

: [h   r> get-order n>r  only forth >r ;
: h]   r> nr> set-order >r ;
: [H]  [h ' h] compile, ; immediate
: [m   r> get-order n>r  meta-context >r ;
: m]   r> nr> set-order >r ;
: [M]  [m ' m] compile, ; immediate
: [t   r> get-order n>r get-current >r only previous target definitions >r ;
: t]   r> r> set-current nr> set-order >r ;
: [T]  postpone [t ' compile, postpone t] ; immediate

: input>r   r> save-input n>r >r ;
: r>input   r> nr> restore-input abort" Restore-input?" >r ;

: copy   >in @ >r : r> >in !  ' compile, postpone ; ;
: immediate:   copy immediate ;

create code-line  128 allot
create t-dictionary  17000 allot

: ?first   over c@ 1 swap case
   [char] 0 of ." zero" endof  [char] 1 of ." one" endof
   [char] 2 of ." two" endof   [char] 3 of ." three" endof
   [char] 4 of ." four" endof  [char] 5 of ." five" endof
   [char] 6 of ." six" endof   [char] 7 of ." seven" endof
   [char] 8 of ." eight" endof [char] 9 of ." nine" endof
   nip 0 swap endcase /string ;
: ?emit   2>r dup 2r> 1+ within if emit 0 then ;
: .m    [char] 0 [char] 9 ?emit  [char] A [char] Z ?emit
   [char] a [char] z ?emit  case  [char] ? of ." question" endof
   [char] > of ." to" endof  [char] < of ." from" endof
   [char] ' of ." tick" endof  [char] = of ." equal" endof
   [char] . of ." dot" endof  [char] " of ." quote" endof
   [char] [ of ." lbracket" endof  [char] ] of ." rbracket" endof
   [char] + of ." plus" endof  [char] - of ." minus" endof
   [char] * of ." star" endof  [char] # of ." number" endof
   [char] / of ." slash" endof  [char] \ of ." backslash" endof
   [char] @ of ." fetch" endof  [char] ! of ." store" endof
   0 of endof  ." _" endcase ;
: .mangled   ?first bounds ?do i c@ .m loop ;



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
: <mark      here ;
: <resolve   , ;

: cells   cell * ;

t-dictionary value there
: (.def)   space here there - .  here to there
   cr type space  >in @ parse-name type >in ! ;
: .def   parse-name postpone sliteral postpone (.def) ; immediate

: create   .def CREATE  0 header, reveal ;

: .code1   ." xt_t * REGPARM " latestxt >name .mangled
   ." _code (xt_t *IP, struct word *word)" cr ;

\ TODO: remember the C function name for later output.
: .code2   source >in @ /string type cr ;

: code   .def CODE  0 header, reveal
   parse-name s" \" compare if .code1 else .code2 then ." {" cr
   begin refill 0= abort" Refill?" source s" end-code" compare
   while source type cr repeat ." }" cr ;

: end-code   ;

: find-name   #name min 2dup ['] forth search-wordlist dup if 2nip then ;

only forth definitions
: forward-reference   [T] create immediate [M] here ,  0 [M] ,
   does> ." FREF:" dup 0 >body - id. [M] here over @ [M] ,  swap ! ;
: forward ( a u -- )   ." FNEW:" 2dup type  input>r string-input
   forward-reference  r>input ;
: ?forward   2dup ['] target search-wordlist if nip nip execute
   else forward then ;
: pph   compile, 2drop ;

only forth definitions also meta-interpreter also host-interpreter
finders tpp   compile, ?forward abort
: target,   find-name tpp ;
: ppt   drop postpone sliteral postpone target, ;
: ppn   drop ppt ;
finders pp   ppt ppn pph
: t-postpone   parse-name 2dup meta-context [H] find-name
   interpreter-context also host-compiler pp ; immediate
interpreter-context definitions also host-interpreter

: postpone,   t-postpone (literal) , t-postpone compile, ;
finders meta-postpone   postpone, forward compile,

: ]   1 state !  compiler-context ;
: constant   .def CONSTANT  0 header, , reveal ;
: variable   .def VARIABLE  0 header, 0 , reveal ;
: :   .def COLON  0 header, ] ;
: defer   .def DEFER  0 header, 0 , reveal ;
: value   .def VALUE  0 header, , reveal ;

: '   parse-name find-name 0=
   if cr ." Undefined, and cannot tick: " type cr abort then ;
: immediate   latestxt dup c@ negate swap c! ;

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

: [defined]   parse-name find-name if drop -1 else 2drop 0 then ; immediate
: [undefined]   postpone [defined] 0= ; immediate

: [   0 state !  interpreter-context ; immediate
: ;   reveal t-postpone exit t-postpone [ ; immediate
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

interpreter-context definitions also host-interpreter
: unresolved   postpone t-postpone  postpone >mark ; immediate
only also meta-interpreter also meta-compiler definitions also host-interpreter
: ahead        unresolved branch ; immediate
: if           unresolved 0branch ; immediate
: then         >resolve ; immediate

: postpone   parse-name find-name meta-postpone ; immediate

: s"   t-postpone (s")  [char] " parse  dup ,  string, ; immediate
: ."   [M] s"  t-postpone type ; immediate
: [char]   char t-postpone literal ; immediate
: abort"   t-postpone if [M] s" t-postpone cr t-postpone type t-postpone cr
   t-postpone abort t-postpone then ; immediate
\ : abort"   t-postpone if [M] s" t-postpone (abort") t-postpone then ; immediate

interpreter-context definitions also host-interpreter
: resolving   postpone t-postpone  postpone <resolve ; immediate
only also meta-interpreter also meta-compiler definitions also host-interpreter
: begin       <mark ; immediate
: again       resolving branch ; immediate
: until       resolving 0branch ; immediate

: while    t-postpone if swap ; immediate
: repeat   t-postpone again t-postpone then ; immediate
: else     t-postpone ahead swap t-postpone then ; immediate

: do      leaves @  0 leaves !  t-postpone 2>r  t-postpone begin  0 ; immediate
: leave   t-postpone branch  here leaves chain, ; immediate
: +loop   ?dup if swap t-postpone r+ t-postpone again t-postpone then
          else t-postpone (+loop) t-postpone until then
          leaves >resolve@  leaves !  t-postpone unloop ; immediate
: loop    1 t-postpone literal t-postpone +loop ; immediate

immediate: [if]    immediate: [else]   immediate: [then]
immediate: does>   immediate: (        immediate: \

cr .( META-COMPILER WORDS: ) cr
also meta-compiler words cr previous



only forth definitions

: ?found   0= if cr ." Unresolved forward reference: " type cr abort then ;
: resolve   ." FRES:" dup id. dup >name [M] find-name ?found  swap >body @
   \ TODO: merge with LEAVE resolution.
   begin dup while 2dup @ >r swap ! r> repeat 2drop 1 ;
: resolve-forward-references   ['] target ['] resolve traverse-wordlist ;

: ?compile,   state @ abort" Metacompile to host definition?!?" execute ;

: ?literal,   state @ if [M] literal then ;

: meta-number  2>r 0 0 2r@ >number nip if 2drop 2r> target,
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

: .,   ." , " ;
: .{  ." struct word " >name .mangled ." _word = { " ;
: .name   dup c@ (.) .,  [char] " emit >name type [char] " emit ., ;
: .link   >nextxt ?dup if >name ." &" .mangled ." _word, " else ." 0, " then ;
: .code   >code @ drop ." enter_code, " ;
: .does   >does @ drop ." 0, {" cr ;
: .body   body-bounds ?do i @ ."   (cell)" (.) ., cr cell +loop ;
: .}   ." } }" cr ;
: dis   dup .{  dup .name  dup .link  dup .code  dup .does  .body  .}  1 ;
: disassemble-target-dictionary   ['] forth ['] dis traverse-wordlist ;



( Start metacompilation. )

interpreter-context
meta-compile targets/c/nucleus.fth
meta-compile kernel.fth
resolve-forward-references
disassemble-target-dictionary

cr .( TARGET DICTIONARY: ) cr
only forth definitions
t-words cr
." Used: " t-used .
