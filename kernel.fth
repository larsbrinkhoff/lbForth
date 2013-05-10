\ -*- forth -*- Copyright 2004, 2013 Lars Brinkhoff

( System implementation words. )

code cold \ int main (void)
  static cell dictionary[10000];
  void signal_handler (int);
  xt_t *IP = (xt_t *)warm_word.param;

  tickhere_word.param[0] = (cell)dictionary;
  SP_word.param[0] = (cell)(&data_stack_word.param[100]);
  RP_word.param[0] = (cell)(&return_stack_word.param[100]);

  siginterrupt (SIGINT, 1);
  signal (SIGINT, signal_handler);
  siginterrupt (SIGSEGV, 1);
  signal (SIGSEGV, signal_handler);

  for (;;)
    {
      if (setjmp ((void *)jmpbuf_word.param))
        EXECUTE (&sigint_word);

      for (;;)
        {
          xt_t xt = NEXT_XT;
          EXECUTE (xt);
        }
    }

  return 0;
end-code

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

code signal_handler \ void signal_handler (int i)
  sigset_t set;
  sigemptyset (&set);
  sigaddset (&set, i);
  sigprocmask (SIG_UNBLOCK, &set, 0);
  signal (i, signal_handler);
  longjmp ((void *)jmpbuf_word.param, i);
end-code

variable SP
variable RP

: sp@   SP @ /cell + ;
: sp!   SP ! ;

: rp@   RP @ /cell + ;
: rp!   postpone (literal) RP , postpone ! ; immediate

: cabs ( char -- |char| )   dup 127 > if 256 swap - then ;

: >name ( xt -- caddr u )   count cabs ;

: >lfa   C TO_NEXT + ;

: >nextxt   >lfa @ ;

code enter ( -- ) ( R: -- ret )
    RPUSH (IP);
    IP = (xt_t *)(word->param);
end-code

code exit ( R: ret -- )
    IP = RPOP (xt_t *);
end-code

code dodoes ( -- addr ) ( R: -- ret )
    PUSH (word->param);
    RPUSH (IP);
    IP = (xt_t *)(word->does);
end-code

\ This is for meta compiler convenience.
create 'exit   ' exit ,

: branch   r> @ >r ;

\ Possible, but slow, implementation of 0branch.
\ : select   0= dup invert swap rot nand invert rot rot nand invert + ;
\ : 0branch   r> dup cell+ swap @ rot select >r ;

code 0branch ( x -- )
    xt_t *addr = *(xt_t **)IP;
    cell x = POP (cell);
    if (!x)
      IP = addr;
    else
      IP++;
end-code

\ This works, but is too slow.
\ create '/cell   C sizeof(cell) ,
\ variable temp
\ : (literal)   r> temp ! temp @ temp @ '/cell @ + >r @ ;

code (literal) ( -- n )
    PUSH (*(cell *)IP);
    IP++;
end-code

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
: name, ( "name" -- )  bl-word C NAME_LENGTH allot ;
: header, ( code -- )  align here  name,  link, ( code ) , 0 , ;

: reveal   lastxt @ current @ >body ! ;

\ ----------------------------------------------------------------------

( Core words. )

code ! ( x addr -- )
    cell *addr = POP (cell *);
    cell x = POP (cell);
    *addr = x;
end-code

code @ ( addr -- x )
    cell *addr = POP (cell *);
    PUSH (*addr);
end-code

code + ( x y -- x+y )
    cell y = POP (cell);
    cell x = POP (cell);
    PUSH (x + y);
end-code

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

: :   C enter_code header, ] !csp ;

: ;   reveal postpone exit postpone [ ?csp ; immediate

\ TODO: This is wrong if "-" overflows.  If d=x-y and sX is the
\ sign bit, this computes "less than":  (sy&sx) ^ (sd&sx) ^ (sd&sy)
: <   - C ~(((ucell)-1)>>1) nand invert if -1 else 0 then ;
\ : <   2dup xor 0< if drop 0< else - 0< then ;
\ create numbers -1 1 rshift invert ,  -1 ,  0 ,  1 ,  -1 1 rshift ,
\ : n   cells numbers + @ ;
\ : foo ( n -- )   5 0 do dup i n over . ." < " dup . < ." => " . cr loop drop ;
\ : bar   5 0 do i n foo loop ;

: =   - if 0 else -1 then ;

: > ( x y -- flag )   swap < ;

: >code ( xt -- cfa )   C TO_CODE + ;

: >does ( xt -- dfa )   C TO_DOES + ;

: >body ( xt -- pfa )   C TO_BODY + ;

variable >in

code >number ( d1 addr1 n1 -- d2 addr2 n2 )
    cell n = POP (cell);
    char *addr = POP (char *);
    udcell d = POP (ucell);
    int negate = 1;
    d += POP (udcell) << 32;
    if (n > 0 && *addr == '-')
      {
	n--;
	addr++;
	negate = -1;
      }
    while (n > 0)
      {
	char c = *addr;
	int m = (c >= '0' && c <= '9') ? (c - '0') :
                (c >= 'A' && c <= 'A') ? (c - 'A' + 10) :
                (c >= 'a' && c <= 'z') ? (c - 'a' + 10) : 36;
	if (m < 0 || m >= base_word.param[0])
	  break;
	d = base_word.param[0] * d + negate * m;
	addr++;
	n--;
      }
    PUSH (d >> 32);
    PUSH (d & (cell)-1);
    PUSH (addr);
    PUSH (n);
end-code

\ This works, but is too slow.
\ : >r   r@ rp@ -4 + rp! rp@ ! rp@ 4 + ! ;

code >r  ( x -- ) ( R: -- x )
    cell x = POP (cell);
    RPUSH (x);
end-code

\ This works, but is too slow.
\ : r>   rp@ 4 + @ r@ rp@ 4 + rp! rp@ ! ;

code r> ( -- x ) ( R: x -- )
    cell x = RPOP (cell);
    PUSH (x);
end-code

: r@   rp@ cell+ @ ;

: ?dup ( 0 -- 0 | x - x x )   dup if dup then ;

: abort ( ... -- ) ( R: ... -- )   data_stack 100 cells + sp!  quit ;

: align ( -- )   'here @ aligned 'here ! ;

: aligned ( addr1 -- addr2 )   /cell + 1 - /cell negate nand invert ;

: allot ( n -- )   'here +! ;

code nand ( x y -- ~(x&y) )
    cell y = POP (cell);
    cell x = POP (cell);
    PUSH (~(x & y));
end-code

variable base

: bl ( -- <space> )   32 ;

code c! ( c addr -- )
    char *addr = POP (char *);
    cell c = POP (cell);
    *addr = c;
end-code

code c@ ( addr -- c )
    unsigned char *addr = POP (unsigned char *);
    PUSH (*addr);
end-code

: /cell   /cell ; \ Metacompiler knows what to do.

: cell+ ( n1 -- n2 )   /cell + ;

: cells ( n1 -- n2 )   dup + dup + ;   \ /cell * ;

: count ( caddr -- addr n )   dup 1+ swap c@ ;

: cr ( -- )   10 emit ;

code emit ( c -- )
    cell c = POP (cell);
    putchar (c);
end-code

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
    drop here 1+ - C NAME_LENGTH-1 min here c! ;

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
\     skip parse  C NAME_LENGTH-1 min  dup here c!  here 1+ swap cmove  here ;

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

: sigint   cr C "\11backtrace" find if execute then abort ;

\ ----------------------------------------------------------------------

( Tools extension words. )

code bye ( ... -- <no return> )
    exit (0);
end-code

\ ----------------------------------------------------------------------

( String words. )

: cmove ( addr1 addr2 n -- )   bounds do  dup c@  i c!  1+  loop drop ;

\ ----------------------------------------------------------------------

( File Access words. )

code close-file ( fileid -- ior )
    FILE *fileid = POP (FILE *);
    PUSH (fclose (fileid) == 0 ? 0 : errno);
end-code

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

code open-file ( addr n mode -- fileid ior )
    char *mode = POP (char *);
    int n = POP (cell);
    char *addr = POP (char *);
    char *name = malloc (n + 1);
    FILE *fileid;

    memcpy (name, addr, n);
    name[n] = 0;
    fileid = fopen (name, mode);
    PUSH (fileid);
    PUSH (fileid == 0 ? errno : 0 );
end-code

: r/o   s" r" drop ;

code read-file ( addr n1 fileid -- n2 ior )
    FILE *fileid = POP (FILE *);
    if (fileid == 0)
      fileid = stdin;
    cell n1 = POP (cell);
    char *addr = POP (char *);
    size_t n2;
    n2 = fread (addr, 1, n1, fileid);
    PUSH (n2);
    PUSH (ferror (fileid) ? errno : 0);
end-code

code rwx! ( start end -- ior )
    size_t end = POP (size_t);
    size_t start = POP (size_t);
    long page_size = sysconf (_SC_PAGESIZE);
    start &= -page_size;
    end = (end + page_size - 1) & -page_size;
    if (mprotect ((void *)start, end - start, PROT_READ | PROT_WRITE | PROT_EXEC))
      PUSH (-3);
    else
      PUSH (0);
end-code

\ : write-file ( addr n fileid -- ior )   0 file-io nip ;

\ NOTE: THIS HAS TO BE THE LAST WORD IN THE FILE!
variable lastxt
