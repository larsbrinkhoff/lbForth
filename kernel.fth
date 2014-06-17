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
forward: ,
forward: compile,
: rp!   compile (literal) RP , compile ! ; immediate

variable  temp
: drop    temp ! ;
: 2drop   drop drop ;
: 3drop   2drop drop ;

: r@   rp@ cell+ @ ;
forward: swap
: i    r> r@ swap >r ;

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
forward: >
: cabs     dup 127 > if 256 swap - then ;

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

: bl   32 ;
: cr   10 emit ;
: type   ?dup if bounds do i c@ emit loop else drop then ;

: unex   2r> r> 3drop ;
\ Put xt and 'unex on return stack, then jump to that.
: execute   ['] unex >r >r rp@ >r ;
\ : execute   [ here 3 cells + ] literal ! [ 0 , ] ;
: perform   @ execute ;

forward: abort
defer quit
forward: cells

: abort   data_stack 100 cells + sp!  quit ;

forward: (sliteral)
: ?stack   data_stack 99 cells + sp@ < abort" Stack underflow" ;

variable state

: literal   compile (literal) , ; immediate
: ?literal ( x -- )   state @ if [compile] literal then ;

: undef ( a u -- )   ." Undefined: " type cr abort ;
: ?undef ( a u x -- a u )   if undef then ;

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

defer catch
: dummy-catch   execute 0 ;

: cmove ( addr1 addr2 n -- )   bounds do  dup c@  i c!  1+  loop drop ;

include dictionary.fth

: (sliteral)   r> dup @ swap cell+ 2dup + aligned >r swap ;

\ TODO: This is wrong if "-" overflows.
\ : <   - [ 0 invert 1 rshift invert ] literal nand invert if -1 else 0 then ;
: 0<   [ 0 invert 1 rshift invert ] literal nand invert if -1 else 0 then ;
: xor   2dup nand 1+ dup + + + ;
: <   2dup xor 0< if drop 0< else - 0< then ;
\ If d=x-y and sX is the sign bit, this computes "less than":
\ ((~y)&(x^d)) ^ (d&x);
\ : <   2dup - >r invert over r@ xor and swap r> and xor 0< ;
: >   swap < ;

variable >in

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

create src  2 cells allot
: source   src dup cell+ @ swap @ ;
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

create interpreters  ' execute , ' number , ' execute ,
: ?exception   if cr ." Exception!" cr then ;
: interpret-xt   1+ cells  interpreters + @ catch ?exception ;

: [   0 state !  ['] execute interpreters !  previous ; immediate
: ]   1 state !  ['] compile, interpreters !
   also ['] compiler-words context ! ;

variable csp

: .latest   latestxt >name type ;
: ?bad   rot if type ."  definition: " .latest cr abort else 2drop then ;
: !csp   csp @ s" Nested" ?bad  sp@ csp ! ;
: ?csp   sp@ csp @ <> s" Unbalanced" ?bad  0 csp ! ;

: :   parse-name header, postcode dodoes  ] !csp
   [ here cell + ] ['] nop latestxt >does ! exit then >r ;
: ;   reveal compile exit [compile] [ ?csp ; immediate

\ ----------------------------------------------------------------------

( Core extension words. )

defer refill
defer ?prompt
0 value source-id

create fib   256 allot

: file-refill ( -- flag )   0 >in !  0 src !  -1
   fib 256 bounds do
      i 1 source-id read-file if drop 0 leave then
      dup 0=  i c@ 10 =  or  if src @ or 0= if drop 0 then leave then
      drop  1 src +!
   loop ;

: restore-input   drop  is ?prompt  is refill  src !  src cell+ !
   ['] source-id >body !  >in !  0 ;
: save-input   >in @  source-id  source  ['] refill >body @
   ['] ?prompt >body @  6 ;

defer backtrace

: sigint   cr backtrace abort ;

\ ----------------------------------------------------------------------

( File Access words. )

: n>r   r> over >r swap begin ?dup while rot r> 2>r 1 - repeat >r ;
: nr>   r> r@ begin ?dup while 2r> >r rot rot 1 - repeat r> swap >r ;

defer parsed
: (parsed) ( a u -- )   find-name interpret-xt ;
: interpret   begin parse-name dup while parsed ?stack repeat 2drop ;
: interpreting   begin refill while interpret ?prompt repeat ;

: file-input ( fileid -- )    ['] source-id >body !  fib src cell+ !
   ( 0 blk ! )  ['] file-refill is refill  ['] nop is ?prompt ;

: include-file ( fileid -- )   save-input n>r
   file-input interpreting  source-id close-file drop
   nr> restore-input abort" Bad restore-input" ;

: r/o   s" r" drop ;

: included   2dup align here >r  name,  r> included-files chain, 0 , 0 ,
   r/o open-file abort" Read error." include-file ;

: warm
   ." lbForth" cr
   ['] nop dup is backtrace is also
   ['] dummy-catch is catch
   ['] (number) is number
   ['] (parsed) is parsed
   ['] (previous) is previous
   ['] latestxt dup to latestxt forth !
   ['] forth current !

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

\ NOTE: THIS HAS TO BE THE LAST WORD IN THE FILE!
0 value latestxt
