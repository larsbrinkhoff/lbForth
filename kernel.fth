\ -*- forth -*-

\ Reminder: ! does ( x addr -- )
\ Reminder: cmove does ( from to length -- )

\ ----------------------------------------------------------------------

( System implementation words. )

: boot ( -- )
    \ s" boot.fth" included ;
    s" core.fth" included
    s" core-ext.fth" included
    s" tools.fth" included
    quit ;

: 'here ( -- 'here )   C &HERE ;

: 'SP ( -- 'sp )   C &SP ;

: 'RP ( -- 'rp )   C &RP ;

: data_stack ( -- addr )   C data_stack ;

: return_stack ( -- addr )   C return_stack ;

\ : cabs ( char -- |char| )   dup 127 > if 256 swap - then ;

: word= ( caddr xt -- flag )
    2dup ( >name ) c@
    ( cabs ) dup 127 > if 256 swap - then
    swap c@ = if
	-1 swap rot
	dup c@ 1+ 1 do
	    2dup i + c@
	    swap ( >name ) i + c@
	    <> if rot drop 0 rot rot leave then
	loop
	2drop
    else
	2drop
        0
    then ;

\ : >name ( xt -- addr )   ;

: >nextxt ( xt1 -- xt2 )   C TO_NEXT + @ ;

: lastxt ( -- addr )   C &lastxt ;

code docolon ( -- ) ( R: -- ret )
    RPUSH (IP);
    IP = (xt_t *)(word->param);
end-code

code dovariable ( -- addr )
    PUSH (word->param);
end-code

code doconstant ( -- addr )
    PUSH (word->param[0]);
end-code

\ code dodoes ( -- addr ) ( R: -- ret )
\     PUSH (&word->param[1]);
\     RPUSH (IP);
\     IP = (xt_t *)(word->param[0]);
\ end-code

code branch ( -- )
    xt_t *addr = *(xt_t **)IP;
    IP = addr;
end-code

code 0branch ( x -- )
    xt_t *addr = *(xt_t **)IP;
    cell x = POP (cell);
    if (!x)
      IP = addr;
    else
      IP++;
end-code

\ It's difficult to implement (literal) in Forth without creating a loop.
code (literal) ( -- n )
    PUSH (*(cell *)IP);
    IP++;
end-code

\ It's possible to implement (+loop) in Forth, but it's such a pain!
code (+loop) ( n -- flag ) ( R: limit index -- limit index+n )
    cell n = POP (cell);
    cell index = (RP[0] += n);
    cell limit = RP[1];
    PUSH (index >= limit);
end-code

: (s") ( -- addr n ) ( R: ret1 -- ret2 )
    r> dup @ swap cell+ 2dup + aligned >r swap ;

: number ( caddr -- ... )
    0 0 rot count ?dup if
	>number ?dup if
	    ." Undefined: " type cr abort
	else
	    drop nip  postpone literal
	then
    else
	2drop drop
    then ;

create interpreters
    ' compile, ,
    ' number ,
    ' execute ,

: interpret ( ... "words..." -- ... )
    begin
	source?
    while
	bl-word here find 1+ cells  interpreters + @ execute
    repeat ;

\ : dodoes_code ( -- addr )   C dodoes_code ;

: dovariable_code ( -- addr )   C dovariable_code ;

: doconstant_code ( -- addr )   C doconstant_code ;

: squote ( -- addr )   C squote ;

: bounds ( addr1 n -- addr2 addr1)   over + swap ;

: header, ( "word" -- addr )
    align here  bl-word C NAME_LENGTH allot  lastxt @ , ;

\ ----------------------------------------------------------------------

( Core words. )

code ! ( x addr -- )
    cell *addr = POP (cell *);
    cell x = POP (cell);
    *addr = x;
end-code

\ code * ( x y -- x*y )
\     cell y = POP (cell);
\     cell x = POP (cell);
\     PUSH (x * y);
\ end-code

\ code */ ( x y z -- x*y/z )
\     cell z = POP (cell);
\     cell y = POP (cell);
\     cell x = POP (cell);
\     dcell p = (dcell)x * (dcell)y;
\     PUSH (p / z);
\ end-code

\ TODO: */MOD

code + ( x y -- x+y )
    cell y = POP (cell);
    cell x = POP (cell);
    PUSH (x + y);
end-code

: +! ( n addr -- )   swap over @ + swap ! ;

: , ( n -- )   here !  /cell allot ;

: - ( x y -- x-y )   negate + ;

code /mod ( x y -- x%y x/y )
    cell y = POP (cell);
    cell x = POP (cell);
    PUSH (x % y);
    PUSH (x / y);
end-code

: 0= ( n -- flag )   0 = ;

: 1+ ( n -- n+1 )   1 + ;

code 2/ ( n -- n/2 )
    cell n = POP (cell);
    PUSH (n >> 1);
end-code

: 2drop ( x y -- )   drop drop ;

: 2dup ( x y -- x y x y )   over over ;

variable thisxt

: : ( "word" -- colon-sys )
    header, thisxt !  C docolon_code ,  ] ;

: ; ( colon-sys -- )
    thisxt @ lastxt !  postpone exit  postpone [ ; immediate

code < ( x y -- flag )
    cell y = POP (cell);
    cell x = POP (cell);
    PUSH ((x < y) ? -1 : 0);
end-code

: = ( x y -- flag )   2dup < >r > r> or invert ;

: > ( x y -- flag )   swap < ;

: >body ( xt -- pfa )   C TO_BODY + ;

: >in ( -- addr )   C &to_in ;

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
	int m = *addr - '0';
	if (m < 0 || m >= 10)
	  break;
	d = base * d + negate * m;
	addr++;
	n--;
      }
    PUSH (d >> 32);
    PUSH (d & (cell)-1);
    PUSH (addr);
    PUSH (n);
end-code

\ This works, but is too slow.
\ : >r ( x -- ) ( R: -- x )   r@ 'RP @ -4 + 'RP ! 'RP @ ! 'RP @ 4 + ! ;

code >r  ( x -- ) ( R: -- x )
    cell x = POP (cell);
    RPUSH (x);
end-code

: ?dup ( 0 -- 0 | x - x x )   dup if dup then ;

code @ ( addr -- x )
    cell *addr = POP (cell *);
    PUSH (*addr);
end-code

: abort ( ... -- ) ( R: ... -- )   C data_stack 100 cells + 'SP !  quit ;

: align ( -- )   'here @ aligned 'here ! ;

: aligned ( addr1 -- addr2 )   /cell + 1 - /cell negate nand invert ;

: allot ( n -- )   'here +! ;

code nand ( x y -- ~(x&y) )
    cell y = POP (cell);
    cell x = POP (cell);
    PUSH (~(x & y));
end-code

\ code and ( x y -- x&y )
\     cell y = POP (cell);
\     cell x = POP (cell);
\     PUSH (x & y);
\ end-code

: base ( -- 'base )   C &base ;

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

: /cell ( -- n )   C sizeof(cell) ;

: cell+ ( n1 -- n2 )   /cell + ;

: cells ( n1 -- n2 )   dup + dup + ;   \ /cell * ;

: count ( caddr -- addr n )   dup 1+ swap c@ ;

: cr ( -- )   10 emit ;

\ : create ( "word" -- )   header, lastxt !  C dovariable_code , ;

: drop ( x -- )   C &sink ! ;

: dup ( x -- x x )   >r r@ r> ;

code emit ( c -- )
    cell c = POP (cell);
    putchar (c);
end-code

code execute ( xt -- )
    xt_t xt = POP (xt_t);
    EXECUTE (xt);
end-code

code exit ( R: ret -- )
    IP = RPOP (xt_t *);
end-code

\ : immediate? ( xt -- 1 | -1 )   ( >name ) c@ 127 > if 1 else -1 then ;

: find ( caddr -- caddr 0 | xt 1 | xt -1 )
    lastxt @
    begin
	dup
    while
	2dup word= if
	    nip dup ( immediate? ) c@ 127 > if 1 else -1 then
	    exit
	then
	>nextxt
    repeat ;

\ TODO: fm/mod

: here ( -- addr )   'here @ ;

: i ( -- x ) ( R: x -- x )   r> r@ swap >r ;

: invert ( x -- ~x )   -1 nand ;

code key ( -- c )
    PUSH (getchar ());
end-code

: literal ( -- n ) ( C: n -- )
    state @ if postpone (literal) , then ; immediate

\ code m* ( x y -- x*y )
\     cell y = POP (cell);
\     cell x = POP (cell);
\     dcell z = (dcell)x * (dcell)y;
\     PUSH (z);
\ end-code

: min ( x y -- min[x,y] )
    2dup < if drop else nip then ;

: negate ( n -- -n )   invert 1+ ;

: or ( x y -- x|y )   invert swap invert nand ;

: over ( x y -- x y x )   >r >r r@ r> r> swap ;

\ The literal 0 will be patched when loading core.
: quit ( R: ... -- )   0 execute ;

\ This works, but is too slow.
\ : r> ( -- x ) ( R: x -- )    'RP @ 4 + @ r@ 'RP @ 4 + 'RP ! 'RP @ ! ;

code r> ( -- x ) ( R: x -- )
    cell x = RPOP (cell);
    PUSH (x);
end-code

: r@ ( -- x) ( R: x -- x )   'RP @ cell+ @ ;

: rot ( x y z -- y z x )   >r swap r> swap ;

code rshift ( x n -- x>>n )
    cell n = POP (cell);
    ucell x = POP (cell);
    PUSH (x >> n);
end-code

\ TODO: sm/rem

variable ''source
variable ''#source

: source ( -- addr n )   ''source @  ''#source @ @ ;

: state ( -- addr )   C &state ;

: swap ( x y -- y x )   >r >r 'RP @ cell+ @ r> r> drop ;

: type ( addr n -- )
    ?dup if
	bounds do
	    i c@ emit
	loop
    else
	drop
    then ;

\ code um* ( x y -- x*y )
\     ucell y = POP (cell);
\     ucell x = POP (cell);
\     udcell z = (udcell)x * (udcell)y;
\     PUSH (z);
\ end-code

\ TODO: um/mod

: unloop ( R: loop-sys -- )
    r> r> r> 2drop >r ;

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
    skip
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

\ code xor ( x y -- x^y )
\     cell y = POP (cell);
\     cell x = POP (cell);
\     PUSH (x ^ y);
\ end-code

: [ ( -- )
    0 state ! ; immediate

: ] ( -- )
    1 state ! ;

\ ----------------------------------------------------------------------

( Core extension words. )

: #tib ( -- addr )
    C &ntib ;

: <>  ( x y -- flag )
    = 0= ;

: 2>r ( x1 x2 -- ) ( R: -- x1 x2 )   r> rot rot swap >r >r >r ;

: compile, ( xt -- )   state @ if , else execute then ;

: nip ( x y -- y )   swap drop ;

\ : parse ( char "string<char>" -- addr n )
\     pad >r  begin
\ 	source? if <source 2dup <> else 0 0 then
\     while
\ 	r@ c!  r> 1+ >r
\     repeat  2drop  pad r> over - ;

\ : pad ( -- addr )   here 100 + ;

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
    source drop 100 bounds do
	key dup 10 = if drop leave then
	i c!
	1 #tib +!
    loop ;

: file-refill ( -- flag )
    0 >in !
    0 #fib !
    -1
    source drop 100 bounds do
	i 1 source-id read-file  if ." Read error." abort then
	dup 0=  i c@ 10 =  or  if #fib @ or 0= if drop 0 then leave then
	drop  1 #fib +!
    loop ;

: restore-input ( xn .. x1 n -- )
    drop  'source-id !  ''#source !  ''source !  >in ! ;

: save-input ( -- xn .. x1 n )
    >in @  ''source @  ''#source @  source-id  4 ;

variable 'source-id

: source-id ( -- 0 | -1 | fileid )   'source-id @ ;

: tib ( -- addr )   C tib ;

\ ----------------------------------------------------------------------

( Tools words. )

\ code .s ( -- )
\     cell *p;
\     printf ("<%d>", data_stack + 100 - SP);
\     for (p =  data_stack + 100 - 1; p >= SP; p--)
\       printf (" %d", *p);
\     putchar ('\n');
\ end-code

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

variable #fib

: include-file ( fileid -- )
    >r save-input r> 'source-id !
    C fib ''source !  #fib ''#source !  0 #fib !
    \ 0 blk !
    begin
	refill
    while
	interpret
    repeat
    source-id close-file drop
    restore-input ;

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

: r/o ( -- mode )
    C "r" ;

code read-file ( addr n1 fileid -- n2 ior )
    FILE *fileid = POP (FILE *);
    cell n1 = POP (cell);
    char *addr = POP (char *);
    size_t n2;
    n2 = fread (addr, 1, n1, fileid);
    PUSH (n2);
    PUSH (ferror (fileid) ? errno : 0);
end-code

\ : read-file ( addr n1 fileid -- n2 ior )   -1 file-io ;

\ : write-file ( addr n fileid -- ior )   0 file-io nip ;

\ code file-io ( addr n1 fileid read? -- n2 ior )
\     cell readp = POP (cell);
\     FILE *fileid = POP (FILE *);
\     cell n1 = POP (cell);
\     char *addr = POP (char *);
\     size_t n2;
\     n2 = (readp ? fread : fwrite) (addr, 1, n1, fileid);
\     PUSH (n2);
\     PUSH (ferror (fileid) ? errno : 0);
\ end-code
