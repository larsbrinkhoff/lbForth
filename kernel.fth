\ -*- forth -*- Copyright 2004, 2013 Lars Brinkhoff

\ This kernel needs, at a minimum, these 17 primitives:
\
\ Definitions:		enter dodoes exit
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
: rp!   postpone (literal) RP , postpone ! ; immediate

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

: cells   [ cell 1 > ] [if] dup + [then]
   [ cell 2 > ] [if] dup + [then]
   [ cell 4 > ] [if] dup + [then] ;

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

: abort   data_stack 100 cells + sp!  quit ;

forward: (sliteral)
: ?stack   data_stack 99 cells + sp@ < abort" Stack underflow" ;

variable state

: literal   state @ if postpone (literal) , then ; immediate

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
   ?dup if ." Undefined: " type cr abort then
   drop r> if negate then
   postpone literal ;

defer catch
: dummy-catch   execute 0 ;

: cmove ( addr1 addr2 n -- )   bounds do  dup c@  i c!  1+  loop drop ;

include c.fth

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

variable forth
variable compiler-words
variable included-files

create context   ' forth , ' forth , 0 , 0 , 0 , 0 , 0 , 0 , 0 ,

: find-name ( a u -- a u 0 | xt ? )
   #name min context >r begin r> dup cell+ >r @ ?dup while
      >r 2dup r> search-wordlist ?dup
      if 2nip r> drop exit then
   repeat r> drop 0 ;

: key   here dup 1 0 read-file 0 = 1 = nand 0= abort" Read error"  c@ ;

: or   invert swap invert nand ;

create src  2 cells allot
: source   src dup cell+ @ swap @ ;
: source? ( -- flag )   >in @ source nip < ;
: <source ( -- char|-1 )   source >in @ dup rot = if
   2drop -1 else + c@  1 >in +! then ;

: blank?   dup bl =  over 8 = or  over 9 = or  over 10 = or  over 13 = or nip ;
: skip ( "<blanks>" -- )   begin source? while
   <source blank? 0= until -1 >in +! then ;
: parse-name ( "<blanks>name<blank>" -- a u )   skip  source drop >in @ +
   0 begin source? while 1+ <source blank? until 1 - then ;

: previous   ['] forth context ! ;

defer also

create interpreters  ' execute , ' number , ' execute ,

: interpret-xt   1+ cells  interpreters + @ catch
                 if ." Exception" cr then ;

: [   0 state !  ['] execute interpreters !  previous ; immediate
: ]   1 state !  ['] compile, interpreters !
   also ['] compiler-words context ! ;

variable csp

: .latest   latestxt >name type ;
: ?bad   rot if type ." definition: " latestxt >name type cr abort
   else 2drop then ;
: !csp   csp @ s" Nested" ?bad  sp@ csp ! ;
: ?csp   sp@ csp @ <> s" Unbalanced" ?bad  0 csp ! ;

: :   parse-name header, postcode enter ] !csp ;
: ;   reveal postpone exit postpone [ ?csp ; immediate

\ ----------------------------------------------------------------------

( Core extension words. )

defer refill

create fib   256 allot

forward: source-id
: file-refill ( -- flag )   0 >in !  0 src !  -1
   fib 256 bounds do
      i 1 source-id read-file abort" Read error."
      dup 0=  i c@ 10 =  or  if src @ or 0= if drop 0 then leave then
      drop  1 src +!
   loop ;

variable 'source-id
: source-id   'source-id @ ;
: restore-input   drop  is refill  src !  src cell+ !  'source-id !  >in !  0 ;
: save-input   >in @  source-id  source  ['] refill >body @  5 ;

defer backtrace

: sigint   cr backtrace abort ;

\ ----------------------------------------------------------------------

( File Access words. )

: n>r   r> over >r swap begin ?dup while rot r> 2>r 1 - repeat >r ;
: nr>   r> r@ begin ?dup while 2r> >r rot rot 1 - repeat r> swap >r ;

: interpret  begin parse-name dup while
   find-name interpret-xt ?stack repeat 2drop ;

: interpret-loop   >r begin ['] refill catch if ." Exception" cr -1 then while
   interpret r@ execute repeat r> drop ;

: file-input ( fileid -- )    'source-id !  fib src cell+ !
   ( 0 blk ! )  ['] file-refill is refill ;

: include-file ( fileid -- )   save-input n>r  file-input
   ['] nop interpret-loop  source-id close-file drop
   nr> restore-input abort" Bad restore-input" ;

: r/o   s" r" drop ;

: included   2dup align here >r  name,  r> included-files chain, 0 , 0 ,
   r/o open-file abort" Read error." include-file ;

: warm
   ." lbForth" cr
   ['] nop dup is backtrace is also
   ['] dummy-catch is catch
   ['] (number) is number
   ['] latestxt dup to latestxt forth !
   ['] forth current !
   0 compiler-words !
   0 included-files !
   10 base !
   postpone [
   s" core.fth" included
   s" core-ext.fth" included
   s" string.fth" included
   s" tools.fth" included
   s" file.fth" included
   ." ok" cr
   quit ;

\ NOTE: THIS HAS TO BE THE LAST WORD IN THE FILE!
0 value latestxt
