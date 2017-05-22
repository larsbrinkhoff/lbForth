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

vocabulary t-words
defer t-compile,
defer t-literal
defer t,
: t-word ( a u xt -- ) -rot "create , does> @ t-compile, ;
: fatal   cr source type cr bye ;
: ?undef   0= if ." Undefined!" fatal then ;
: t-search   ['] t-words search-wordlist ;
: defined?   t-search if drop -1 else 0 then ;
: "' ( u a -- xt ) t-search ?undef >body @ ;

: already-defined?   >in @ >r parse-name defined? r> >in ! ;
: trailing-semicolon?   source 1- + c@ [char] ; = ;
: ignore-definition   begin trailing-semicolon? postpone \ until ;

: h-number   [ action-of number ] literal is number ;

variable leaves
: 0leaves   0 leaves ! ;
: leaves@   leaves @ ;

vocabulary meta
vocabulary compiler

: target   only forth also meta also t-words definitions previous ;

only forth also meta definitions
: host   only forth definitions ;

include lib/image.fth
target-image

0 value latest

' , is t,
' t, is t-compile,

include lib/xforward.fth

: compile   parse-name postpone sliteral postpone "' postpone , ; compile-only
: [compile]   also compiler ' previous compile, ; compile-only
: (t-literal)   compile (literal) , ;
also forth
' (t-literal) is t-literal
: t-constant   create , does> @ t-literal ;
previous

: ?number,   if 2drop undef fatal else drop t-literal 2drop then ;
: number, ( a u -- ) 0 0 2over >number nip ?number, ;
: t-number   ['] number, is number ;
: '   parse-name "' ;

: ]   only forward-refs also t-words also compiler  t-number ;

: [defined]   parse-name defined? ;
: [undefined]   [defined] 0= ;

only forth also compiler definitions
: [   target h-number ;
: \   postpone \ ;
: (   postpone ( ;
: [if]   postpone [if] ;
: [else]   postpone [else] ;
: [then]   postpone [then] ;

also meta host
