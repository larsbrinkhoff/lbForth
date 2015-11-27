: noop ;

create data_stack   110 cells allot
create return_stack   256 cells allot
create jmpbuf         jmp_buf allot

variable dictionary_end

: cell    cell ; \ Metacompiler knows what to do.
: cell+   cell + ;

: 2drop   drop drop ;
: 3drop   2drop drop ;
: r@   rp@ cell+ @ ;
: rot    >r swap r> swap ;
: 2r>   r> r> r> rot >r swap ;
: 3dup   >r >r r@ over 2r> over >r rot swap r> ;
: ?dup   dup if dup then ;
: unloop    r> 2r> 2drop >r ;
forward: <
: min   2dup < if drop else nip then ;
: bounds   over + swap ;
: count    dup 1+ swap c@ ;
: i    r> r@ swap >r ;
: cr   10 emit ;
: type   bounds do i c@ emit loop ;
: perform   @ execute ;
variable state
: <   2dup xor 0< if drop 0< else - 0< then ;
: cmove ( addr1 addr2 n -- )   ?dup if bounds do count i c! loop drop
   else 2drop then ;
: cabs   127 over < if 256 swap - then ;
0 value latestxt

include dictionary.fth

: lowercase? ( c -- flag )   dup [char] a < if drop 0 exit then [ char z 1+ ] literal < ;
: upcase ( c1 -- c2 )   dup lowercase? if [ char A char a - ] literal + then ;
: c<> ( c1 c2 -- flag )   upcase swap upcase <> ;

: name= ( ca1 u1 ca2 u2 -- flag )
   2>r r@ <> 2r> rot if 3drop 0 exit then
   bounds do
      dup c@ i c@ c<> if drop unloop 0 exit then
      1+
  loop drop -1 ;
: nt= ( ca u nt -- flag )   >name name= ;

: immediate?   c@ 127 swap < if 1 else -1 then ;

: traverse-wordlist ( wid xt -- ) ( xt: nt -- continue? )
   >r >body @ begin dup while
      r@ over >r execute r> swap
      while >nextxt
   repeat then r> 2drop ;

: ?nt>xt ( -1 ca u nt -- 0 xt i? 0 | -1 ca u -1 )
   3dup nt= if >r 3drop 0 r> dup immediate? 0
   else drop -1 then ;
: (find) ( ca u wl -- ca u 0 | xt 1 | xt -1 )
   2>r -1 swap 2r> ['] ?nt>xt traverse-wordlist rot if 0 then ;
: search-wordlist ( ca u wl -- 0 | xt 1 | xt -1 )
   (find) ?dup 0= if 2drop 0 then ;

defer abort
: undef ( a u -- )   ." Undefined: " type cr abort ;
: ?undef ( a u x -- a u )   if undef then ;

: literal   compile (literal) , ; immediate
: ?literal ( x -- )   state @ if [compile] literal then ;



host also meta
\ cr .( Target size: ) t-size .
\ cr .( Target used: ) target here host also meta >host t-image host - .
\ cr .( Host unused: ) unused .
target



[undefined] 2dup [if]
.( We should not get here. )
: 2dup   over over ;
[then]

: (abort)   ." ABORT!" bye ;
' (abort) is abort

variable x
: foo   70 x !  x @ emit  1 x +!  x @ emit ;

0 value fd
create buf 22 allot

: readme   s" README.md" 0 open-file abort" Error opening file" to fd
           buf 22 fd read-file abort" Error reading file"
           buf 22 type
           fd close-file abort" Error closing file" ;

defer baz
' foo is baz

16 constant sixteen

create data_space   100 cells allot
' data_space >body ' dp >body !

' noop >code @ constant 'docol

: (does>)   r> does! ;

: name,   16 0 do 0 c, loop ;
: link,   0 , ;
: header,   here to latestxt  name, link, 0 , ;
\ : :   header, 'docol , ;
: :   header, 'dodoes , does> >r ;
: ;   compile exit ; immediate
: c   67 [compile] literal compile emit ;
: colon   : compile foo compile foo c compile cr [compile] ; ;
: test-colon   colon  latestxt execute ;

: space   ."  " ;

variable counter  char A ' counter >body !
: exclam   ." We're here: " counter @ emit 1 counter +! ." !" cr ;

forward: bar
: hello   s" hello " type ;
: test=  s" foo" name= if ." :) " else ." :( " then ;
: warm   s" foo" test=
         s" bar" test=
         s" Foo" test=
         s" FOO" test= cr
         test-colon foo baz bar cr readme ['] hello execute bye ;
: bar   sixteen cells 1+ 2 or 1 xor emit ;

code cold
   then,

   ' warm >body # I mov,
   ' data_stack >body 100 cells + # S mov,
   ' return_stack >body 256 cells + # R mov,

   next,
end-code
