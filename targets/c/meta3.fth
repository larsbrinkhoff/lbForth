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

: iconst   >r : r> postpone literal postpone ; ; immediate
: immediate:   >in @ >r : r> >in !  ' compile, postpone ; immediate ;

: (.def)   cr type space  >in @ parse-name type >in ! ;
: .def   parse-name postpone sliteral postpone (.def) ; immediate

create code-line 128 allot

create t-dictionary   2048 allot
variable tp   t-dictionary tp !



( Metacompiler interpreter words. )

(* [DEFINED] [UNDEFINED]
 * : IMMEDIATE DOES> DEFER CODE END-CODE
 * VARIABLE VALUE CREATE ALLOT , ' CELLS >CODE
 * CELL JMP_BUF NAME_LENGTH
 *
 * From host:
 * ( \ [IF] [ELSE] [THEN]  INVERT RSHIFT = CHAR - @
 *)

only also meta-interpreter definitions previous

4 constant cell
16 constant name_length
256 constant jmp_buf

: [defined]     [T] [defined] ; immediate
: [undefined]   [T] [undefined] ; immediate

: here       tp @ ;
: allot      tp +! ;
: ,          [M] here !   [M] cell [M] allot ;
: compile,   [M] , ;

: cells   [M] cell * ;

: create   >in @ cr ." CREATE " parse-name type >in ! ( .def CREATE )  [T] create ;

: code   >in @ cr ." CODE " parse-name type >in ! ( .def CODE )  [T] create
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
: constant   >in @ cr ." CONSTANT " parse-name type 2dup >in !
   [definitions] meta-interpreter constant  >in !
   [T] constant ;
: variable   >in @ cr ." VARIABLE " parse-name type >in !  [T] variable ;
: :   >in @ cr ." COLON " parse-name type >in !  [T] create [M] ] ; immediate
: '   parse-name [T] find-name 0= if forward then ;
: defer   >in @ cr ." DEFER " parse-name type >in !  [T] defer ;
: value   >in @ cr ." VALUE " parse-name type >in !  [T] value ;
: immediate   ."  IMMEDIATE " ;



( Metacompiler compilation words. )

(* ( \ [IF] [ELSE] [THEN] [DEFINED] [UNDEFINED]
 * ; DOES>
 * [CHAR] ['] [ LITERAL POSTPONE TO IS ." S" ABORT"
 * IF ELSE THEN DO LEAVE LOOP +LOOP BEGIN AGAIN WHILE REPEAT UNTIL
 * CELL
 *)

only also meta-compiler definitions previous

16 iconst to_next
20 iconst to_code
24 iconst to_does
28 iconst to_body

: [defined]     [T] [defined] ; immediate
: [undefined]   [T] [undefined] ; immediate
: [   0 state !  interpreter-context ; immediate
: ;   [M] [ ; immediate
\ : ;code   postpone [ ... ; immediate
: [compile]   [T] [compile] ; immediate
: literal   s" (literal)" find-name drop [M] compile,
   [M] , ; immediate
: cell   [M] cell [M] literal ;
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

.( METACOMPILER WORDS: )
also meta-compiler words cr previous


only forth definitions

: ?compile,   state @ if compile, else execute then ;

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
