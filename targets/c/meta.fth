( Metacompiler for C target. )

\ This is in a state of a mess at the moment.

(* Need to support these words:
 * ( \ [IF] [ELSE] [THEN] [DEFINED] [UNDEFINED] INCLUDE
 * : ; IMMEDIATE DOES> DEFER CODE END-CODE
 * VARIABLE VALUE CREATE ALLOT , ' CELLS INVERT RSHIFT = CHAR -
 * [CHAR] ['] [ ] LITERAL POSTPONE IS TO  ." S"
 * IF ELSE THEN DO LEAVE LOOP +LOOP BEGIN AGAIN WHILE REPEAT UNTIL
 * CELL NAME_LENGTH TO_NEXT TO_CODE TO_DOES TO_BODY
 *)

require search.fth
require lib/bitmap.fth
require lib/common.fth

: reset   only forth definitions  decimal ;
reset
:noname reset clear bye ; is abort

(* A note about the vocabularies.

 TARGET holds the host definition of target words.  When executed,
 they compile an xt into the target image.  There are no immediate
 words in this vocabulary.  Compiling words must be defined in the
 META-COMPILER vocabulary.

 HOST-INTERPRETER and HOST-COMPILER are mostly aliases for host words,
 which are then used in the implementation of the metacompiler.  They
 are needed to override the metacompiler definitions with the same names.

 The META-INTERPRETER vocabulary is searched outside colon definitions
 when metacompiling target code.  It consists mostly of defining words
 which writes to the target image.  META-COMPILER is searched inside
 definitions and consists mostly of target compiling words.

   Search order.

 In the metacompiler implementation, the interpreter search order is:
 FORTH META-INTERPRETER HOST-INTERPRETER.  The compiler search order
 is: FORTH META-INTERPRETER HOST-COMPILER.

 When interpreting target code, the search order is: FORTH
 META-INTERPRETER.  When compiling, the search order is: TARGET
 META-COMPILER. *)

vocabulary target
vocabulary host-compiler
vocabulary host-interpreter
vocabulary meta-compiler
vocabulary meta-interpreter
vocabulary forward-declarations

: interpreter-context   only forth also meta-interpreter ;
: compiler-context   only forward-declarations also target also meta-compiler ;
: meta-context   compiler-context also meta-interpreter ;

\ Offset, within the definition of :, to the runtime action of :.
\ Obviously, the constant should match the string.
4 cells constant colon-runtime-offset
: colon-runtime   c" &docolcomma_word.param[4]" ;

: [M]  also meta-compiler ' compile, previous ; immediate

: input>r   r> save-input n>r >r ;
: r>input   r> nr> restore-input abort" Restore-input?" >r ;

defer number,
defer t-compile,
action-of number constant host-number

: host-word ( xt a u -- )   get-current >r ['] target set-current
   "create , r> set-current  does> @ t-compile, ;

: copy   >in @ >r : r> >in !  ' compile, postpone ; ;
: immediate:   copy immediate ;

create code-line  128 allot

4000 cells constant t-space
create t-dictionary  t-space allot
t-space bitmap t-map
: >map ( a1 -- a2 x )   t-dictionary - t-map ;
: addr! ( a -- )   >map 1bit ;
: addr? ( a -- f )   >map bit@ ;


: ?first   over c@ 1 swap case  [char] - of ." minus" endof
   [char] < of ." less" endof  [char] > of ." greater" endof
   [char] 0 of ." zero" endof  [char] 1 of ." one" endof
   [char] 2 of ." two" endof   [char] 3 of ." three" endof
   [char] 4 of ." four" endof  [char] 5 of ." five" endof
   [char] 6 of ." six" endof   [char] 7 of ." seven" endof
   [char] 8 of ." eight" endof [char] 9 of ." nine" endof
   nip 0 swap endcase /string ;
: ?emit   2>r dup 2r> 1+ within if emit 0 then ;
: .mc    [char] 0 [char] 9 ?emit  [char] A [char] Z ?emit
   [char] a [char] z ?emit  case  [char] ? of ." question" endof
   [char] > of ." to" endof  [char] < of ." from" endof
   [char] ' of ." tick" endof  [char] = of ." equal" endof
   [char] . of ." dot" endof  [char] , of ." comma" endof
   [char] [ of ." lbracket" endof  [char] ] of ." rbracket" endof
   [char] + of ." plus" endof  [char] " of ." quote" endof
   [char] * of ." star" endof  [char] # of ." number" endof
   [char] / of ." slash" endof  [char] \ of ." backslash" endof
   [char] @ of ." fetch" endof  [char] ! of ." store" endof
   [char] : of ." colon" endof  [char] ; of ." semicolon" endof
   0 of endof  ." _" endcase ;
: .mangled   ?first bounds ?do i c@ .mc loop ;

: .qc ( c -- )   case  [char] \ of ." \\" endof
   [char] " of ." \" [char] " emit endof
   dup emit endcase ;
: .quoted   [char] " emit  bounds ?do i c@ .qc loop  [char] " emit ;

create forward-references 0 ,
: create-forward   also forward-declarations definitions
   create previous 0 ,  latestxt forward-references chain, ;
: .extern   ." extern struct word " .mangled ." _word;" cr ;
: .forward   >in @  parse-name .extern  >in ! ;

: pph   compile, 2drop ;

defer does+
: does: ( u "name1" "name2" -- )   create cells , parse-name s,
   does> @+ swap @+ does+ ;
vocabulary does-table  also does-table definitions previous

0 does: create noop
0 does: variable noop
1 does: constant dup
1 does: value dup
0 does: defer perform

only forth definitions

: find-does ( a1 u -- a2 )   also does-table evaluate previous ;
: target-evaluate   get-order n>r  only target evaluate  nr> set-order ;
: t-find   get-order n>r  only target find-name  nr> set-order ;
: target-xt   t-find 0= ?undef  >body @ ;
: t-defined?   t-find if drop -1 else 2drop 0 then ;

: already-defined?   >in @ >r parse-name t-defined? r> >in ! ;
: trailing-semicolon?   source 1- + c@ [char] ; = ;
: ignore-definition   begin trailing-semicolon? postpone \ until ;

: us,    here over allot  swap cmove ;
: save-function-name ( a1 u -- a2 )   here -rot  dup c, us, ;
: dodoes_code   s" dodoes_code" save-function-name ;



( Host words to override defining words in metacompiler. )

only forth also host-interpreter definitions

: ]   ] interpreter-context also host-compiler ;
: :   : ] ;

copy ,       copy '         copy allot     copy defer  copy immediate
copy create  copy variable  copy constant  copy value  
\ here align [defined] [undefined]
: forward:   parse-name 2drop ;

\ cr .( HOST INTERPRETER WORDS: ) cr  words

( Host words to override compiling words in metacompiler. )

only forth also host-compiler definitions

: [   postpone [ interpreter-context also host-interpreter ; immediate
: ;   postpone ; postpone [ ; immediate

immediate: [defined]  immediate: [undefined]  immediate: case
immediate: literal    immediate: [']          immediate: postpone
immediate: is         immediate: to           immediate: sliteral
immediate: abort"     immediate: [char]       immediate: +loop
immediate: of         immediate: endof        immediate: endcase

immediate: (       immediate: if       immediate: else   immediate: \
immediate: [if]    immediate: [else]   immediate: [then] immediate: ?do
immediate: then    immediate: begin    immediate: until  immediate: while
immediate: repeat  immediate: again    immediate: do     immediate: leave
immediate: loop    immediate: ."       immediate: s"     immediate: does>

\ cr .( HOST COMPILING WORDS: ) cr  words



( Metacompiler interpreter words. Primarily defining words. )

(* [DEFINED] [UNDEFINED] INCLUDE
 * : IMMEDIATE DOES> DEFER CODE END-CODE
 * VARIABLE VALUE CONSTANT CREATE ALLOT , ' CELLS
 * CELL NAME_LENGTH
 *
 * From host:
 * ( \ [IF] [ELSE] [THEN]  INVERT RSHIFT CHAR @ = -
 *)

interpreter-context definitions also host-interpreter

include params.fth
cell-size constant cell
name-size constant name_length
next-offset constant to_next
code-offset constant to_code
does-offset constant to_does
body-offset constant to_body
body-offset 2 cells + constant /wordlist
dodoes_code constant 'dodoes

( *** Target compiler included here. *** )
include dictionary.fth
include target.fth
create t-wordlist  /wordlist allot  t-wordlist /wordlist erase
t-wordlist current !
t-dictionary dp !
             
\ Redefine HEADER, to create a word in the host dictionary.
' compile, is t-compile,
: header,   2dup align here -rot host-word  header, ;

: >mark   here 0 , ;
: >resolve   here swap ! ;
: <mark      here ;
: <resolve   , ;

: forward, ( a -- )   here swap chain, ;
: forward: ( "name" -- )   .forward  create-forward  does> forward, ;

: <body   here cell - dup @ >body swap ! ;
: <code   here cell - dup @ >code @ swap ! ;

only forth definitions also meta-interpreter also host-interpreter

: target,   here addr!  target-evaluate ;
: ppt   drop postpone sliteral postpone target, ;
: ppn   drop ppt ;
finders pp   ppt ppn pph
: host-pp   get-order n>r  compiler-context pp  nr> set-order ;
: t-postpone   parse-name 2dup host-pp ; immediate
: code,   target, <code ;
: postcode   parse-name postpone sliteral postpone code, ; immediate

:noname   target-xt >body + ; is does+
: does!   latestxt >does ! ;
: (does>)   find-does does! ;
: (:-does>)   colon-runtime does! ;
only forth also host-compiler definitions
: does>   latestxt >name 2dup s" :" compare
   if postpone sliteral postpone (does>)
   else 2drop postpone (:-does>) then  postpone exit ; immediate
interpreter-context definitions also host-interpreter

: create    parse-name header, postcode dodoes reveal does> ;

: .code1   ." xt_t * REGPARM " latestxt >name .mangled
   ." _code (xt_t *IP, xt_t word)" cr s"     return IP;" latestxt ;

: .code2   source >in @ /string type cr s" "
   parse-name 2drop parse-name save-function-name ;

: code   parse-name header,
   parse-name s" \" compare if .code1 else .code2 then , reveal
   ." {" cr begin refill 0= abort" Refill?" source s" end-code" compare
   while source type cr repeat type cr ." }" cr ;

: end-code   ;

: >resolve@   @ begin ?dup while dup @ here rot ! repeat ;

: a,   here addr! , ;

: ]   compiler-context  ['] number, is number ;
: constant   create , does> @ ;
: variable   create cell allot ;
: :   parse-name header, postcode dodoes  ] ( !csp )  does> >r ;
: ?:   already-defined? if ignore-definition else : then ;
: defer   create s" abort" target,  does> @ execute ;
: value   create ,  does> @ ;
: is   parse-name target-xt >body ! ;

: '   parse-name target-xt ; \ find-name 0= ?undef ;
: immediate   latestxt >nfa dup c@ negate swap c! ;

: [defined]     parse-name t-defined? ; immediate
: [undefined]   postpone [undefined] 0= ; immediate

: check-colon-runtime   s" docol," target-xt >body colon-runtime-offset + @
   s" >r" target-xt <> if ." Bad offset into colon definition." cr bye then ;

: s,   dup , ", ;   \ Redefined to use target dictionary.

only forth definitions
: resolve ( xt -- )   dup >name target-xt  swap >body @
   begin dup while 2dup @ >r swap ! r> repeat 2drop ;
: resolve-all-forward-references   forward-references
  begin @ ?dup while dup resolve  >body cell+ repeat ;

\ cr .( META-INTERPRETER WORDS: ) cr  previous words cr



( Metacompiler compilation words. )

(* ( \ [IF] [ELSE] [THEN] [DEFINED] [UNDEFINED]
 * ; DOES>
 * [CHAR] ['] [ LITERAL POSTPONE TO IS ." S" ABORT"
 * IF ELSE THEN DO LEAVE LOOP +LOOP BEGIN AGAIN WHILE REPEAT UNTIL
 * CELL
 *)

only forth also meta-interpreter also meta-compiler definitions also host-interpreter

: [defined]   parse-name t-defined? ; immediate
: [undefined]   postpone [defined] 0= ; immediate

: [   host-number is number  interpreter-context ; immediate
: ;   reveal t-postpone exit t-postpone [ ; immediate
: literal   t-postpone (literal) , ; immediate
: cell   cell t-postpone literal ; immediate
: name_length   name-size t-postpone literal ; immediate
: to_next   next-offset t-postpone literal ; immediate
: to_code   code-offset t-postpone literal ; immediate
: to_does   does-offset t-postpone literal ; immediate
: to_body   body-offset t-postpone literal ; immediate
: [']   t-postpone (literal) parse-name target, ; immediate
: is   t-postpone (literal) parse-name target, <body t-postpone ! ; immediate
: to   t-postpone (literal) parse-name target, t-postpone >body
   t-postpone ! ; immediate

interpreter-context definitions also host-interpreter
: unresolved   postpone t-postpone  postpone >mark ; immediate
only forth also meta-interpreter also meta-compiler definitions also host-interpreter
: ahead        unresolved branch ; immediate
: if           unresolved 0branch ; immediate
: then         >resolve ; immediate

: 'dodoes   t-postpone (literal) dodoes_code a, ; immediate
: compile   t-postpone (literal)  parse-name target,
   t-postpone compile, ; immediate
: [compile]   parse-name target, ; immediate

: s"   t-postpone (sliteral) parse" s, ; immediate
: ."   [M] s"  t-postpone type ; immediate
: [char]   char t-postpone literal ; immediate
: abort"   t-postpone if [M] s" t-postpone cr t-postpone type t-postpone cr
   t-postpone abort t-postpone then ; immediate
: does>   t-postpone (does>) ; immediate

interpreter-context definitions also host-interpreter
: resolving   postpone t-postpone  postpone <resolve ; immediate
interpreter-context also meta-compiler definitions also host-interpreter
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
immediate: (       immediate: \

\ cr .( META-COMPILER WORDS: ) cr  also meta-compiler words cr previous



only forth definitions

: meta-number ( a u -- )   2>r 0 0 2r@ >number nip if 2r> undef
   else 2r> 3drop [M] literal then ;
' meta-number is number,

only forth definitions also meta-interpreter also host-interpreter
: t-xt?   >r latestxt begin ?dup while dup r@ <> while >nextxt
          repeat drop 1 else 0 then r> drop ;

: .ref   ?dup if ." &" >name .mangled ." _word" else ." 0" then ;
: .a ( xt a1 -- xt )   over .ref over - case
      0 of endof
      next-offset of ." .next" endof
      code-offset of ." .code" endof
      does-offset of ." .does" endof
      dup ." .param[" body-offset - cell / (.) ." ]"
   endcase ;
: .t-addr
   >r here latestxt begin dup while
   r@ dup 2over swap within if .a else drop then
   nip dup >nextxt  repeat r> 3drop ;
: t-addr? ( a -- f )   t-dictionary t-dictionary t-space + within ;
: .addr ( a -- )   ?dup 0= if ." 0" exit then
   dup t-addr? if .t-addr exit then
   count type ;

: .,   ." , " ;
: .{  ." struct word " >name .mangled ." _word = { " ;
: .name   dup c@ (.) .,  >name .quoted ., ;
: .link   >nextxt .ref ., ;
: .code   >code @ dup t-xt? if >name .mangled ." _code"
    else ." (code_t *)" count type then ." , {" ;
: .does   >does @ .addr ., ;
: .cr   cr ."   (cell)" ;
: .(literal)   ['] (literal) .ref ., .cr ;
: .branch ( a xt -- u )   .ref ., .cr @ .addr 2 cells ;
: .literal ( a xt -- u )   .ref ., .cr dup addr? if @ .addr
   else @ (.) ." U" then 2 cells ;
\ Dumping as a C string doesn't work so well at the moment.
\ : .sliteral ( a xt -- u )   drop .(literal) @+ tuck .quoted .,
\    .cr .(literal) dup (.)  aligned 2 cells + ;
: .xt ( a xt -- u )   dup >name s" branch" compare 0= if .branch else
   dup >name s" 0branch" compare 0= if .branch else
   dup >name s" (literal)" compare 0= if .literal else
   \ dup >name s" (sliteral)" compare 0= if .sliteral else
   .ref drop cell then then then ;
: .number   (.) ." U" cell ;
: .cell   .cr dup t-xt? if .xt else nip .number then ., ;
: search-prev   >r latestxt begin dup >nextxt r@ <> while >nextxt repeat r> drop ;
: >prevxt   dup latestxt = if drop here else search-prev then ;
: body-bounds   dup >prevxt swap >body ;
: .body   body-bounds ?do i @+ .cell +loop ;
: .}   cr ." } };" cr ;
: .word   dup .{  dup .name  dup .link  dup .does  dup .code  .body  .} ;
: disassemble-target-dictionary
   0 begin >prevxt dup .word dup latestxt = until drop ;



( Start metacompilation. )

interpreter-context
.( #include "forth.h" ) cr
s" docol," .extern
include targets/c/nucleus.fth
include kernel.fth
resolve-all-forward-references
check-colon-runtime
disassemble-target-dictionary
only forth definitions

\ This is output as the very last line to signal that the metacompiler
\ ran to completion.  Since it will become part of the C file, it's
\ formatted as a C comment.
.( /* Meta-OK */ ) cr
