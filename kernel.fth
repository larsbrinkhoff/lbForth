\ -*- forth -*- Copyright 2004, 2013 Lars Brinkhoff

( System implementation words. )

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

create data_stack   110 cells allot

create return_stack   100 cells allot

variable 'here

create jmpbuf   jmp_buf allot

variable SP
variable RP

: sp@   SP @ /cell + ;
: sp!   SP ! ;

: rp@   RP @ /cell + ;
: rp!   postpone (literal) RP , postpone ! ; immediate

: cabs ( char -- |char| )   dup 127 > if 256 swap - then ;

: >name ( xt -- caddr u )   count cabs ;

: >lfa   TO_NEXT + ;

: >nextxt   >lfa @ ;

\ This is for meta compiler convenience.
create 'exit   ' exit ,

: branch   r> @ >r ;

: (+loop)   r> swap r> + r@ over >r < invert swap >r ;

: ?stack   data_stack 99 cells + sp@  < if ." Stack underflow" cr abort then ;

: number ( caddr -- ... )
    0 0 rot count ?dup if
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

: interpret  begin bl-word here c@ while here find interpret-xt ?stack repeat ;

: bounds ( addr1 n -- addr2 addr1)   over + swap ;

: link, ( nt -- )      lastxt !  current @ >body @ , ;
: name, ( "name" -- )  bl-word NAME_LENGTH allot ;
: header, ( code -- )  align here  name,  link, ( code ) , 0 , ;

: reveal   lastxt @ current @ >body ! ;

\ ----------------------------------------------------------------------

( Core words. )

: +! ( n addr -- )   swap over @ + swap ! ;

: , ( n -- )   here !  /cell allot ;

: - ( x y -- x-y )   negate + ;

: 0=   if 0 else -1 then ;

: 1+ ( n -- n+1 )   1 + ;

variable  sink
: drop    sink ! ;
: 2drop   drop drop ;
: 3drop   2drop drop ;

: swap   >r >r rp@ cell+ @ r> r> drop ;

: over   >r >r r@ 2r> ;

: dup    sp@ @ ;
: 2dup   over over ;
: 3dup   >r >r r@ over 2r> over >r rot swap r> ;

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

\ TODO: This is wrong if "-" overflows.  If d=x-y and sX is the
\ sign bit, this computes "less than":  (sy&sx) ^ (sd&sx) ^ (sd&sy)
: <   - [ 0 invert 1 rshift invert ] literal nand invert if -1 else 0 then ;
\ : <   2dup xor 0< if drop 0< else - 0< then ;
\ create numbers -1 1 rshift invert ,  -1 ,  0 ,  1 ,  -1 1 rshift ,
\ : n   cells numbers + @ ;
\ : foo ( n -- )   5 0 do dup i n over . ." < " dup . < ." => " . cr loop drop ;
\ : bar   5 0 do i n foo loop ;

: =   - if 0 else -1 then ;

: > ( x y -- flag )   swap < ;

: >code ( xt -- cfa )   TO_CODE + ;

: >does ( xt -- dfa )   TO_DOES + ;

: >body ( xt -- pfa )   TO_BODY + ;

variable >in

: r@   rp@ cell+ @ ;

: ?dup ( 0 -- 0 | x - x x )   dup if dup then ;

: abort ( ... -- ) ( R: ... -- )   data_stack 100 cells + sp!  quit ;

: align ( -- )   'here @ aligned 'here ! ;

: aligned ( addr1 -- addr2 )   /cell + 1 - /cell negate nand invert ;

: allot ( n -- )   'here +! ;

variable base

: bl ( -- <space> )   32 ;

: /cell   /cell ; \ Metacompiler knows what to do.

: cell+ ( n1 -- n2 )   /cell + ;

: cells ( n1 -- n2 )   dup + dup + ;   \ /cell * ;

: count ( caddr -- addr n )   dup 1+ swap c@ ;

: cr ( -- )   10 emit ;

: unex   2r> r> 3drop ;
\ Put xt and 'unex on return stack, then jump to that.
: execute   ['] unex >r >r rp@ >r ;

create forth   ' lastxt ,

create compiler-words   0 ,

create context   ' forth , ' forth , 0 , 0 , 0 ,

variable current

: nt= ( ca u nt -- flag )
    >name 2>r r@ <> 2r> rot if 3drop 0 exit then
    bounds do
      dup c@ i c@ <> if drop unloop 0 exit then
      1+
    loop drop -1 ;

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

: find ( caddr -- caddr 0 | xt ? )
    count context >r begin r> dup cell+ >r @ ?dup while
       >r 2dup r> search-wordlist ?dup
       if 2nip r> drop exit then
    repeat r> 2drop 1 - 0 ;

: here ( -- addr )   'here @ ;

: i ( -- x ) ( R: x -- x )   r> r@ swap >r ;

: invert ( x -- ~x )   -1 nand ;

: key   here dup 1 0 read-file 0 = 1 = nand
        if c@ else ." Read error" abort then ;

: literal ( -- n ) ( C: n -- )
    state @ if postpone (literal) , then ; immediate

: min ( x y -- min[x,y] )
    2dup < if drop else nip then ;

: negate ( n -- -n )   invert 1+ ;

: or ( x y -- x|y )   invert swap invert nand ;

\ The literal 0 will be patched when loading core.
: quit ( R: ... -- )   0 execute ;

: rot ( x y z -- y z x )   >r swap r> swap ;

\ TODO: sm/rem

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

: unloop ( R: loop-sys -- )
    r> 2r> 2drop >r ;

: source? ( -- flag )   >in @ source nip < ;

: <source ( -- char|-1 )
    source >in @ dup rot = if
	2drop -1
    else
	+ c@  1 >in +!
    then ;

: blank?  ( char -- flag )
    dup bl =
    over 8 = or
    over 9 = or
    over 10 = or
    over 13 = or
    nip ;

: skip ( "<blanks>" -- )
    begin
	source?
    while
	<source blank? 0= if -1 >in +! exit then
    repeat ;

: bl-word ( "<blanks>string<blank>" -- )
    skip source? 0= if 0 here c! exit then
    here 1+ <source begin
	over c! 1+
	source? if <source dup blank? else 0 -1 then
    until
    drop here 1+ - NAME_LENGTH 1 - min here c! ;

\ : skip ( char "<chars>" -- char )
\     begin
\ 	source?
\     while
\ 	dup <source
\ 	over bl = if blank? else = then 0= if
\ 	    -1 >in +! exit
\ 	then
\     repeat ;

\ : word ( char "<chars>string<char>" -- caddr )
\     skip parse  NAME_LENGTH 1 - min  dup here c!  here 1+ swap cmove  here ;

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

: compile, ( xt -- )   state @ if , else execute then ;

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

: restore-input ( xn .. x1 n -- flag )
    drop  'source-id !  ''#source !  ''source !  >in !  0 ;

: save-input ( -- xn .. x1 n )
    >in @  ''source @  ''#source @  source-id  4 ;

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

: include-file ( fileid -- )
    >r save-input r> 'source-id !
    fib ''source !  #fib ''#source !  0 #fib !
    \ 0 blk !
    begin
	refill
    while
	interpret
    repeat
    source-id close-file drop
    restore-input if ." Bad restore-input" cr abort then ;

: included ( ... addr n -- ... )
    r/o open-file if cr ." Read error." cr abort then include-file ;

: r/o   s" r" drop ;

\ NOTE: THIS HAS TO BE THE LAST WORD IN THE FILE!
variable lastxt
