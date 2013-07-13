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

: (>defs)   get-current r> 2>r set-current ;
: (defs>)   r> r> set-current >r ;
: [definitions]   ' postpone literal  postpone (>defs)  ' compile,
   postpone (defs>) ; immediate
: [m   r> get-order n>r  meta-context >r ;
: m]   r> nr> set-order >r ;
: [M]   [m ' m] compile, ; immediate
: [t   r> get-order n>r get-current >r  target-context definitions >r ;
: t]   r> r> set-current nr> set-order >r ;
: [T]   postpone [t ' compile, postpone t] ; immediate

: input>r   r> save-input n>r >r ;
: r>input   r> nr> restore-input abort" Restore-input?" >r ;

: forward ( a u -- xt )   ."  FORWARD:" 2dup type  input>r string-input
   [T] create latestxt  r>input ;

: iconst   create immediate ,  does> @ postpone literal ;
: copy   >in @ >r : r> >in !  ' compile, postpone ; ;
: immediate:   copy immediate ;

create code-line  128 allot
create t-dictionary  17000 allot



( Host words to override defining words in metacompiler. )

only also host-interpreter definitions

: ]   ] interpreter-context also host-interpreter ;
: :   : interpreter-context also host-compiler ;

copy ,       copy '         copy allot     copy defer  copy immediate
copy create  copy variable  copy constant  copy value  
\ [DEFINED] [UNDEFINED]

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

: [defined]     [T] [defined] ; immediate
: [undefined]   [T] [undefined] ; immediate

158 constant jmp_buf
16 constant name_length
16 constant to_next

0 value latestxt
( *** Target compiler included here. *** )
include c.fth
variable forth
0 forth !
' forth current !
t-dictionary dp !
             
: cells   cell * ;

t-dictionary value there
: (.def)   space here there - .  here to there
   cr type space  >in @ parse-name type >in ! ;
: .def   parse-name postpone sliteral postpone (.def) ; immediate

\ Possibly, intercept definitions and create a corresponding
\ definition in the host.
\ : header,   >in @ [T] create >in !  header, ;

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

: forward,    forward compile, ;
: postpone,   postpone literal  postpone compile, ;
finders meta-postpone   postpone, forward, compile,

: ]   1 state !  compiler-context ;
: constant   .def CONSTANT  0 header, , reveal ;
: variable   .def VARIABLE  0 header, 0 , reveal ;
: :   .def COLON  0 header, ] ;
: defer   .def DEFER  0 header, 0 , reveal ;
: value   .def VALUE  0 header, , reveal ;

: find-name   #name min 2dup ['] forth search-wordlist dup if 2nip then ;
: '   parse-name find-name 0= if forward then ;
: immediate   ."  IMMEDIATE " ;

cr .( META-INTERPRETER WORDS: ) cr
previous words cr



( Metacompiler compilation words. )

(* ( \ [IF] [ELSE] [THEN] [DEFINED] [UNDEFINED]
 * ; DOES>
 * [CHAR] ['] [ LITERAL POSTPONE TO IS ." S" ABORT"
 * IF ELSE THEN DO LEAVE LOOP +LOOP BEGIN AGAIN WHILE REPEAT UNTIL
 * CELL
 *)

only also meta-compiler definitions previous

4 iconst cell
16 iconst name_length
16 iconst to_next
20 iconst to_code
24 iconst to_does
28 iconst to_body

: [defined]     [T] [defined] ; immediate
: [undefined]   [T] [undefined] ; immediate
: [   0 state !  interpreter-context ; immediate
: ;   [M] reveal [M] [ ; immediate
\ : ;code   postpone [ ... ; immediate
: literal   s" (literal)" [M] find-name drop [M] compile, [M] , ; immediate
: [']   [M] ' [M] literal ; immediate
: is   [M] ' >body [M] literal postpone ! ; immediate
: to   [M] ' >body [M] literal postpone ! ; immediate
: postpone   parse-name [M] find-name [M] meta-postpone ; immediate
: abort"   postpone if postpone s" postpone (abort") postpone then ; immediate
: [char]   char postpone literal ; immediate

immediate: (       immediate: if       immediate: else   immediate: \
immediate: [if]    immediate: [else]   immediate: [then]
immediate: then    immediate: begin    immediate: until  immediate: while
immediate: repeat  immediate: again    immediate: do     immediate: leave
immediate: loop    immediate: ."       immediate: s"

immediate: does>

cr .( META-COMPILER WORDS: ) cr
also meta-compiler words cr previous


only forth definitions

: ?compile,   state @ if [M] compile, else execute then ;

: meta-number  2>r 0 0 2r@ >number nip if 2drop 2r> forward ?compile,
   else 2r> 3drop postpone literal then ;

finders meta-xt   ?compile, meta-number execute

: ?.name   state @ if space 2dup type then ;

: meta-interpret   begin parse-name  ?.name  dup while
   find-name meta-xt ?stack repeat 2drop ;

: meta-loop   begin refill while meta-interpret repeat ;

: meta-compile   parse-name r/o open-file abort" Open?"  input>r
   file-input interpreter-context meta-loop
   source-id close-file abort" Close?"  r>input ;

: t-see   only previous target see only ;

interpreter-context definitions also host-interpreter
: include   .def INCLUDE  meta-compile ;

only definitions also meta-interpreter also host-interpreter
: t-id.   >name type space ;
: t-.nt   t-id. 1 ;
: t-words   ['] forth ['] t-.nt traverse-wordlist ;
: t-used    here t-dictionary - ;
: t'        parse-name ['] forth search-wordlist ;



( Start metacompilation. )

interpreter-context
meta-compile targets/c/nucleus.fth
meta-compile kernel.fth

cr .( TARGET DICTIONARY: ) cr
only definitions
t-words cr
." Used: " t-used .
