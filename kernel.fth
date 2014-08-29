\ -*- forth -*- Copyright 2004, 2013, 2014 Lars Brinkhoff

\ This kernel, together with a target-specific nucleus, provides
\ everything needed to load and compile the rest of the system from
\ source code.  And not much else.  The kernel itself is compiled by
\ the metacompiler.

\ At a minimum, these 16 primitives must be provided by the nucleus:
\
\ Definitions:		dodoes exit
\ Control flow:		0branch
\ Literals:		(literal)
\ Memory access:	! @ c! c@
\ Aritmetic/logic:	+ nand
\ Return stack:		>r r>
\ I/O:			emit open-file read-file close-file

: nop ;

create data_stack     110 cells allot
create return_stack   256 cells allot
create jmpbuf         jmp_buf allot

variable dictionary_end

variable SP
variable RP

: cell    cell ; \ Metacompiler knows what to do.
: cell+   cell + ;

: sp@   SP @ cell + ;
: sp!   SP ! ;
: rp@   RP @ cell + ;
\ rp! in core.fth

variable  temp
: drop    temp ! ;
: 2drop   drop drop ;
: 3drop   2drop drop ;

: r@   rp@ cell+ @ ;

: swap   >r temp ! r> temp @ ;
: over   >r >r r@ r> temp ! r> temp @ ;
: rot    >r swap r> swap ;

: 2>r   r> swap rot >r >r >r ;
: 2r>   r> r> r> rot >r swap ;

: dup    sp@ @ ;
: 2dup   over over ;
: 3dup   >r >r r@ over 2r> over >r rot swap r> ;
: ?dup   dup if dup then ;

: nip    swap drop ;
: 2nip   2>r 2drop 2r> ;

: invert   -1 nand ;
: negate   invert 1 + ;
: -        negate + ;

: branch    r> @ >r ;
forward: <
: (+loop)   r> swap r> + r@ over >r < invert swap >r ;
: unloop    r> 2r> 2drop >r ;

: 1+   1 + ;
: +!   swap over @ + swap ! ;
: 0=   if 0 else -1 then ;
: =    - 0= ;
: <>   = 0= ;

: min   2dup < if drop else nip then ;

: bounds   over + swap ;
: count    dup 1+ swap c@ ;

: i    r> r@ swap >r ;
: bl   32 ;
: cr   10 emit ;
: type   ?dup if bounds do i c@ emit loop else drop then ;

: unex   2r> r> 3drop ;
\ Put xt and 'unex on return stack, then jump to that.
: execute   ['] unex >r >r rp@ >r ;
\ : execute   [ here 3 cells + ] literal ! [ 0 , ] ;
: perform   @ execute ;

variable state

: 0<   [ 0 invert 1 rshift invert ] literal nand invert if -1 else 0 then ;
: xor   2dup nand 1+ dup + + + ;
: <   2dup xor 0< if drop 0< else - 0< then ;
: >   swap < ;

: cmove ( addr1 addr2 n -- )   ?dup if bounds do  dup c@  i c!  1+  loop drop
   else 2drop then ;

: cabs   dup 127 > if 256 swap - then ;

0 value latestxt

include dictionary.fth

: (sliteral)   r> dup @ swap cell+ 2dup + aligned >r swap ;

forward: abort
defer quit
: abort   data_stack 100 cells + sp!  quit ;
: undef ( a u -- )   ." Undefined: " type cr abort ;
: ?undef ( a u x -- a u )   if undef then ;

: literal   compile (literal) , ; immediate
: ?literal ( x -- )   state @ if [compile] literal then ;

defer number

\ Sorry about the long definition, but I didn't want to leave many
\ useless factors lying around.
: (number) ( a u -- )
   over c@ [char] - = dup >r if swap 1+ swap 1 - then
   0 rot rot
   begin dup while
      over c@ [char] 0 - dup -1 > while dup 10 < while
      2>r 1+ swap dup dup + dup + + dup +  r> + swap r> 1 -
   repeat then drop then
   ?dup ?undef drop r> if negate then  ?literal ;

variable >in
variable input
: input@ ( u -- a )   cells input @ + ;
: 'source   0 input@ ;
: #source   1 input@ ;
: source#   2 input@ ;
: 'refill   3 input@ ;
: 'prompt   4 input@ ;
: source>   5 input@ ;
6 cells constant /input-source

variable base

create forth  2 cells allot
create compiler-words  2 cells allot
create included-files  2 cells allot
create context  9 cells allot


: r@+   r> r> dup cell+ >r @ swap >r ;
: search-context ( a u context -- a 0 | xt ? )   >r begin r@+ ?dup while
   (find) ?dup until else drop 0 then r> drop ;
: find-name ( a u -- a u 0 | xt ? )   swap over #name min context
   search-context ?dup if rot drop else swap 0 then ;

: or   invert swap invert nand ;

: source   'source @  #source @ ;
: source? ( -- flag )   >in @ source nip < ;
: <source ( -- char|-1 )   source >in @ dup rot = if
   2drop -1 else + c@  1 >in +! then ;

: blank?   dup bl =  over 8 = or  over 9 = or  over 10 = or  swap 13 = or ;
: skip ( "<blanks>" -- )   begin source? while
   <source blank? 0= until -1 >in +! then ;
: parse-name ( "<blanks>name<blank>" -- a u )   skip  source drop >in @ +
   0 begin source? while 1+ <source blank? until 1 - then ;

: (previous)   ['] forth context ! ;

defer also
defer previous
defer catch

create interpreters  ' execute , ' number , ' execute ,
: ?exception   if cr ." Exception!" cr then ;
: interpret-xt   1+ cells  interpreters + @ catch ?exception ;

: [   0 state !  ['] execute interpreters !  previous ; immediate
: ]   1 state !  ['] compile, interpreters !
   also ['] compiler-words context ! ;

variable csp

: .latest   latestxt >name type ;
: ?bad   rot if type ."  definition: " .latest cr abort then 2drop ;
: !csp   csp @ s" Nested" ?bad  sp@ csp ! ;
: ?csp   sp@ csp @ <> s" Unbalanced" ?bad  0 csp ! ;

: does!   latestxt >does ! ;
: (does>)   r> does! ;

\ If you change the definition of :, you also need to update the
\ offset to the runtime code in the metacompiler(s).
: :   parse-name header, postcode dodoes  ] !csp  does> >r ;
: ;   reveal compile exit [compile] [ ?csp ; immediate

\ ----------------------------------------------------------------------

( Core extension words. )

: refill   0 >in !  0 #source !  'refill perform ;
: ?prompt    'prompt perform ;
: source-id   source# @ ;

256 constant /file

: file-refill   'source @ /file bounds do
      i 1 source-id read-file if 0 unloop exit then
      0= if source nip unloop exit then
      i c@ 10 = if leave then
      1 #source +!
   loop -1 ;

0 value file-source

: save-input   >in @ input @ 2 ;
: restore-input   drop input ! >in ! 0 ;

defer backtrace

: sigint   cr backtrace abort ;

\ ----------------------------------------------------------------------

( File Access words. )

: n>r   r> over >r swap begin ?dup while rot r> 2>r 1 - repeat >r ;
: nr>   r> r@ begin ?dup while 2r> >r rot rot 1 - repeat r> swap >r ;

defer parsed
: (parsed) ( a u -- )   find-name interpret-xt ;
: ?stack   data_stack 99 cells + sp@ < abort" Stack underflow" ;
: interpret   begin parse-name dup while parsed ?stack repeat 2drop ;
: interpreting   begin refill while interpret ?prompt repeat ;

: 0source   'prompt !  'refill !  source# !  'source !  0 source> ! ;
: source, ( 'source sourceid refill prompt -- )
   input @ >r  here input !  /input-source allot  0source  r> input ! ;
: file,   0 0 ['] file-refill ['] nop source,  /file allot ;
: +file   here source> !  file, ;
: file>   source> @  ?dup if input ! else +file then ;
: alloc-file   file-source input ! begin 'source @ while file> repeat ;
: file-input ( fileid -- )   alloc-file  source# !  6 input@ 'source ! ;

: include-file ( fileid -- )   save-input n>r
   file-input interpreting  source-id close-file drop  0 'source !
   nr> restore-input abort" Bad restore-input" ;

: r/o   s" r" drop ;

: included   2dup align here >r  name,  r> included-files chain, 0 , 0 ,
   r/o open-file abort" Read error." include-file ;

: dummy-catch   execute 0 ;

\ NOTE: THIS HAS TO BE THE LAST WORD IN THE FILE!
: warm
   ." lbForth" cr
   ['] nop dup is backtrace is also
   ['] dummy-catch is catch
   ['] (number) is number
   ['] (parsed) is parsed
   ['] (previous) is previous
   ['] warm dup to latestxt forth !
   ['] forth current !
   here to file-source  file,

   0 forth cell+ !
   0 compiler-words !  ['] forth compiler-words cell+ !
   0 included-files !  ['] compiler-words included-files cell+ !
   ['] forth dup context ! context cell+ ! 0 context 2 cells + !

   10 base !
   [compile] [
   s" core.fth" included
   s" core-ext.fth" included
   s" string.fth" included
   s" tools.fth" included
   s" file.fth" included
   ." ok" cr
   quit ;
