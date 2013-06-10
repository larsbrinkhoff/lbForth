\ -*- forth -*- Copyright 2004, 2013 Lars Brinkhoff

\ This kernel needs, at a minimum, these 18 primitives:
\
\ Definitions:		enter dodoes exit
\ Control flow:		0branch
\ Literals:		(literal)
\ Memory access:	! @ c! c@
\ Aritmetic/logic:	+ nand
\ Return stack:		>r r>
\ Number conversion:	>number
\ I/O:			emit open-file read-file close-file

: warm ( -- )
    ." lbForth" cr
    ['] lastxt lastxt !
    ['] lastxt forth !
    0 compiler-words !
    ['] forth current !
    10 base !
    s" core.fth" included
    s" core-ext.fth" included
    s" string.fth" included
    s" tools.fth" included
    ." ok" cr
    quit ;

create data_stack     110 cells allot
create return_stack   100 cells allot
create jmpbuf         jmp_buf allot

variable dp
variable end_of_dictionary

variable SP
variable RP

: sp@   SP @ cell + ;
: sp!   SP ! ;
: rp@   RP @ cell + ;
: rp!   postpone (literal) RP , postpone ! ; immediate

: cabs ( char -- |char| )   dup 127 > if 256 swap - then ;

: >name    count cabs ;
: >lfa     TO_NEXT + ;
: >nextxt   >lfa @ ;

: branch    r> @ >r ;
: (+loop)   r> swap r> + r@ over >r < invert swap >r ;

: ?stack   data_stack 99 cells + sp@  < if ." Stack underflow" cr abort then ;

: number ( a u -- ... )
    0 rot rot 0 rot rot ?dup if
	>number ?dup if
	    ." Undefined: " type cr abort
	else
	    drop nip  postpone literal
	then
    else
	3drop
    then ;

: dummy-catch   execute 0 ;
create catcher   ' dummy-catch ,
: catch   catcher @ execute ;

create interpreters
    ' compile, ,
    ' number ,
    ' execute ,

: interpret-xt   1+ cells  interpreters + @ catch
                 if ." Exception" cr then ;

: interpret  begin parse-name dup while
   find-name interpret-xt ?stack repeat 2drop ;

: bounds    over + swap ;
: count     dup 1+ swap c@ ;

: c,   here c!  1 allot ;
: string, ( addr n -- )    here over allot align  swap cmove ;
: #name   NAME_LENGTH 1 - ;

: link, ( nt -- )      lastxt !  current @ >body @ , ;
: name, ( "name" -- )  parse-name #name min c,  #name string, ;
: header, ( code -- )  align here  name,  link, ( code ) , 0 , ;

: reveal   lastxt @ current @ >body ! ;

\ ----------------------------------------------------------------------

( Core words. )

: +!   swap over @ + swap ! ;
: ,    here !  cell allot ;
: -    negate + ;
: 0=   if 0 else -1 then ;
: 1+   1 + ;

variable  sink
: drop    sink ! ;
: 2drop   drop drop ;
: 3drop   2drop drop ;

: swap   >r >r rp@ cell+ @ r> r> drop ;
: over   >r >r r@ 2r> ;
: rot    >r swap r> swap ;

: dup    sp@ @ ;
: 2dup   over over ;
: 3dup   >r >r r@ over 2r> over >r rot swap r> ;
: ?dup   dup if dup then ;

: nip    swap drop ;
: 2nip   2>r 2drop 2r> ;

variable csp

: !csp   csp @ if ." Nested definition: "
         lastxt @ >name type cr abort then
         sp@ csp ! ;

: ?csp   sp@ csp @ <> if ." Unbalanced definition: "
         lastxt @ >name type cr abort then
         0 csp ! ;

: :   [ ' enter >code @ ] literal header, ] !csp ;
: ;   reveal postpone exit postpone [ ?csp ; immediate

: =   - if 0 else -1 then ;
\ TODO: This is wrong if "-" overflows.  If d=x-y and sX is the
\ sign bit, this computes "less than":  (sy&sx) ^ (sd&sx) ^ (sd&sy)
: <   - [ 0 invert 1 rshift invert ] literal nand invert if -1 else 0 then ;
\ : <   2dup xor 0< if drop 0< else - 0< then ;
\ create numbers -1 1 rshift invert ,  -1 ,  0 ,  1 ,  -1 1 rshift ,
\ : n   cells numbers + @ ;
\ : foo ( n -- )   5 0 do dup i n over . ." < " dup . < ." => " . cr loop drop ;
\ : bar   5 0 do i n foo loop ;
: >   swap < ;

: >code   TO_CODE + ;
: >does   TO_DOES + ;
: >body   TO_BODY + ;

variable >in

: r@   rp@ cell+ @ ;
: i    r> r@ swap >r ;

: abort   data_stack 100 cells + sp!  quit ;

: align     dp @ aligned dp ! ;
: aligned   cell + 1 - cell negate nand invert ;
: allot     dp +! ;

variable base

: bl   32 ;
: cr   10 emit ;

: cell    cell ; \ Metacompiler knows what to do.
: cell+   cell + ;
cell 4 = [if] : cells   dup + dup + ; [then]
cell 8 = [if] : cells   dup + dup + dup + ; [then]

: unex   2r> r> 3drop ;
\ Put xt and 'unex on return stack, then jump to that.
: execute   ['] unex >r >r rp@ >r ;

create forth            ' lastxt ,
create compiler-words   0 ,

create context   ' forth , ' forth , 0 , 0 , 0 ,
variable current

: lowercase? ( c -- flag )   dup [char] a < if drop 0 exit then [char] z 1+ < ;
: upcase ( c1 -- c2 )   dup lowercase? if [ char A char a - ] literal + then ;
: c<> ( c1 c2 -- flag )   upcase swap upcase <> ;

: name= ( ca1 u1 ca2 u2 -- flag )
    2>r r@ <> 2r> rot if 3drop 0 exit then
    bounds do
      dup c@ i c@ c<> if drop unloop 0 exit then
      1+
    loop drop -1 ;
: nt= ( ca u nt -- flag )   >name name= ;

: immediate?   c@ 127 > if 1 else -1 then ;

\ TODO: nt>string nt>interpret nt>compile
\ Forth83: >name >link body> name> link> n>link l>name

: traverse-wordlist ( wid xt -- ) ( xt: nt -- continue? )
    >r >body @ begin dup while
       r@ over >r execute r> swap
       while >nextxt
    repeat then r> 2drop ;

: ?nt>xt ( ca u -1 nt -- xt i? 0 0 | ca u -1 -1 )
    nip 3dup nt= if nip nip dup immediate? 0 0
    else drop -1 -1 then ;
: search-wordlist ( ca u wl -- 0 | xt 1 | xt -1 )
    -1 swap ['] ?nt>xt traverse-wordlist
    if 2drop 0 then ;

: find-name ( a u -- a u 0 | xt ? )
    #name min context >r begin r> dup cell+ >r @ ?dup while
       >r 2dup r> search-wordlist ?dup
       if 2nip r> drop exit then
    repeat r> drop 0 ;

: here   dp @ ;

: invert   -1 nand ;
: negate   invert 1+ ;

: key   here dup 1 0 read-file 0 = 1 = nand
        if c@ else ." Read error" abort then ;

: literal   state @ if postpone (literal) , then ; immediate

: min   2dup < if drop else nip then ;

: or ( x y -- x|y )   invert swap invert nand ;

\ The literal 0 will be patched when loading core.
: quit ( R: ... -- )   0 execute ;

variable ''source
variable ''#source

: source ( -- addr n )   ''source @  ''#source @ @ ;

variable state

: type ( addr n -- )
    ?dup if
	bounds do
	    i c@ emit
	loop
    else
	drop
    then ;

: unloop   r> 2r> 2drop >r ;

: source? ( -- flag )   >in @ source nip < ;
: <source ( -- char|-1 )   source >in @ dup rot = if
   2drop -1 else + c@  1 >in +! then ;

: blank?   dup bl =  over 8 = or  over 9 = or  over 10 = or  over 13 = or nip ;

: skip ( "<blanks>" -- )   begin source? while
   <source blank? 0= if -1 >in +! exit then repeat ;

: parse-name ( "<blanks>name<blank>" -- a u )   skip  source drop >in @ +
    0 begin source? while 1+ <source blank? until 1 - then ;

: previous   ['] forth context ! ;

: also   ;

: [   0 state !  previous ; immediate
: ]   1 state !  also ['] compiler-words context ! ;

\ ----------------------------------------------------------------------

( Core extension words. )

variable #tib
variable #fib

create tib   256 allot
create fib   256 allot

: <>   = 0= ;

: 2>r   r> swap rot >r >r >r ;
: 2r>   r> r> r> rot >r swap ;

: compile,   state @ if , else execute then ;

: refill ( -- flag )
    source-id dup 0= if
	drop terminal-refill
    else -1 = if
	0 \ string-refill
    else
	file-refill
    then then ;

: terminal-refill ( -- flag )
    0 >in !
    0 #tib !
    -1
    source drop 256 bounds do
	key dup 10 = if drop leave then
	i c!  1 #tib +!
    loop ;

: file-refill ( -- flag )
    0 >in !
    0 #fib !
    -1
    source drop 256 bounds do
	i 1 source-id read-file  if ." Read error." abort then
	dup 0=  i c@ 10 =  or  if #fib @ or 0= if drop 0 then leave then
	drop  1 #fib +!
    loop ;

: restore-input   drop  'source-id !  ''#source !  ''source !  >in !  0 ;
: save-input      >in @  ''source @  ''#source @  source-id  4 ;

variable 'source-id
: source-id ( -- 0 | -1 | fileid )   'source-id @ ;

: nop ;
create 'bt   ' nop ,

: sigint   cr 'bt @ execute abort ;

\ ----------------------------------------------------------------------

( String words. )

: cmove ( addr1 addr2 n -- )   bounds do  dup c@  i c!  1+  loop drop ;

\ ----------------------------------------------------------------------

( File Access words. )

: n>r ( x1 ... xn n -- ) ( R: -- x1 ... xn n )
   r> over >r swap begin ?dup while
     rot r> 2>r 1 -
   repeat >r ;

: nr> ( -- x1 ... xn n ) ( R: x1 ... xn n -- )
   r> r@ begin ?dup while
      2r> >r rot rot 1 -
   repeat r> swap >r ;

: include-file ( fileid -- )
    save-input n>r 'source-id !
    fib ''source !  #fib ''#source !  0 #fib !  \ 0 blk !
    begin refill while interpret repeat
    source-id close-file drop
    nr> restore-input if ." Bad restore-input" cr abort then ;

: included ( ... addr n -- ... )
    r/o open-file if cr ." Read error." cr abort then include-file ;

: r/o   s" r" drop ;

\ NOTE: THIS HAS TO BE THE LAST WORD IN THE FILE!
variable lastxt
