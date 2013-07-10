( Metacompiler for C target. )

(* Need to support these words:
 * ( \ [IF] [ELSE] [THEN] [DEFINED] [UNDEFINED]
 * : ; IMMEDIATE DOES> DEFER CODE END-CODE
 * VARIABLE VALUE CREATE ALLOT , ' CELLS >CODE @ INVERT RSHIFT = CHAR -
 * [CHAR] ['] [ ] LITERAL POSTPONE IS TO  ." S"
 * IF ELSE THEN DO LEAVE LOOP +LOOP BEGIN AGAIN WHILE REPEAT UNTIL
 * CELL JMP_BUF NAME_LENGTH TO_NEXT TO_CODE TO_DOES TO_BODY
 *)

require search.fth
only forth definitions  decimal

vocabulary target           \ Holds compiled target words.
vocabulary host-compiler
vocabulary host-interpreter
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

: forward ( a u -- xt )   ."  FORWARD:" 2dup type  save-input n>r string-input
   [T] create latestxt  nr> restore-input abort" Restore input?" ;
: forward,   forward compile, ;
finders meta-postpone   postpone, forward, compile,

: iconst   create immediate ,  does> @ postpone literal ;
: copy   >in @ >r : r> >in !  ' compile, postpone ; ;
: immediate:   copy immediate ;

: (.def)   cr type space  >in @ parse-name type >in ! ;
: .def   parse-name postpone sliteral postpone (.def) ; immediate

create code-line  128 allot
create t-dictionary  17000 allot



( Host words. )

only also host-interpreter definitions

: :   : interpreter-context also host-compiler ;

copy defer  copy immediate  copy create  copy variable  copy constant
copy value

cr .( HOST INTERPRETER WORDS: ) cr  words

only also host-compiler definitions

4 iconst cell
16 iconst name_length
158 iconst jmp_buf
16 iconst to_next
20 iconst to_code
24 iconst to_does
28 iconst to_body

: ;   postpone ; interpreter-context also host-interpreter ; immediate

immediate: [defined]  immediate: [undefined]  immediate: [
immediate: [compile]  immediate: literal      immediate: [']
immediate: is         immediate: to           immediate: postpone
immediate: abort"     immediate: [char]

immediate: (       immediate: if       immediate: else   immediate: \
immediate: [if]    immediate: [else]   immediate: [then]
immediate: then    immediate: begin    immediate: until  immediate: while
immediate: repeat  immediate: again    immediate: do     immediate: leave
immediate: loop    immediate: ."       immediate: s"

cr .( HOST COMPILING WORDS: ) cr  words



( Metacompiler interpreter words. )

(* [DEFINED] [UNDEFINED]
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

16 iconst name_length
158 constant jmp_buf

variable dp  t-dictionary dp !
: here       dp @ ;
: allot      dp +! ;
: aligned    cell + 1 - cell negate nand invert ;
: align      dp @ aligned dp ! ;
: ,          here !   cell allot ;
: c,         here c!  1 allot ;
: compile,   , ;
: string,    here over allot align  swap cmove ;
: #name      NAME_LENGTH 1 - ;

variable forth    0 forth !
variable current  ' forth current !

0 value latestxt
: link, ( nt -- )       to latestxt  current @ >body @ , ;
: reveal                latestxt current @ >body ! ;
: name, ( a u -- )      #name min c,  #name string, ;

: header,    >in @ [T] create >in !
             align here  parse-name name,  link, 0 , 0 , ;
             
: used       here t-dictionary - ;

: cells   cell * ;

: create   .def CREATE  header, reveal ;

: code   .def CODE  header, reveal
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

: ]   1 state !  compiler-context ;
: constant   .def CONSTANT  header, , reveal ;
: variable   .def VARIABLE  header, 0 , reveal ;
: :   .def COLON  header, ] ;
: '   parse-name [T] find-name 0= if forward then ;
: defer   .def DEFER  header, 0 , reveal ;
: value   .def VALUE  header, , reveal ;
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
158 iconst jmp_buf
16 iconst to_next
20 iconst to_code
24 iconst to_does
28 iconst to_body

: [defined]     [T] [defined] ; immediate
: [undefined]   [T] [undefined] ; immediate
: [   0 state !  interpreter-context ; immediate
: ;   [M] reveal [M] [ ; immediate
\ : ;code   postpone [ ... ; immediate
: [compile]   [T] [compile] ; immediate
: literal   s" (literal)" find-name drop [M] compile,
   [M] , ; immediate
: [']   [M] ' [M] literal ; immediate
: is   [M] ' >body [M] literal postpone ! ; immediate
: to   [M] ' >body [M] literal postpone ! ; immediate
: postpone   parse-name [T] find-name meta-postpone ; immediate
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

: meta-interpret  begin parse-name ( space 2dup type ) dup while
   find-name meta-xt ?stack repeat 2drop ;

: meta-loop   begin refill while meta-interpret repeat ;

: meta-compile   parse-name r/o open-file abort" Open?"  save-input n>r
   file-input interpreter-context meta-loop
   source-id close-file abort" Close?"  postpone [
   nr> restore-input abort" Restore-input?"  only forth definitions ;

: t-see   only previous target see only ;



( Start metacompilation. )

interpreter-context
meta-compile targets/c/nucleus.fth
meta-compile kernel.fth

cr .( TARGET WORDS: ) cr
also target words only

cr .( TARGET DICTIONARY: ) cr
