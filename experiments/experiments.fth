: slow-fib   dup 1 > if dup 1- recurse swap 2 - recurse + then ;
: fast-fib   0 1 rot 0 do tuck + loop ;

: factorial   dup 2 < if drop 1 else  dup 1- recurse * then ;

: ([ex])   r@ find 0= abort" not found" execute  r> string+ >r ;
: [execute]   postpone ([ex])  bl word count nip 1+ allot align ; immediate

: make-accumulator   create ,  does> tuck +! @ ;
: counter   create 0 ,  does> 1 over +! @ ;

\ TODO: 1. find end of word, 2. relative branch targets.
\ : :inline   create immediate ] does> @ compile, ;

: dispatch ( low high "name words..." -- )
           ( name execution: index -- i*x )
    create  1+ swap  dup negate 1+ ,  do ' , loop
    does> tuck @ + cells + @ ;

\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ 
: (redef)   create immediate here 0 , 0 ,
            does> here swap @+ compile, ! ;
: :redef?   (redef) :noname swap ! ;
: :redef!   :noname dup  ' >body !+  @ ! ;

( Usage:   :redef? foo 42 + ;
           : bar foo ;
           :redef! foo 42 - ; )

\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ 
: rev   dup begin ?dup while dup 1+ roll >r 1- repeat >r nr> drop ;
( Usage:   1 2 3 4 4 rev )

\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ 
vocabulary inline   also inline definitions
: ;   postpone ; previous ; immediate
previous definitions
: ';        [ also inline ' ; previous ] literal ;
: :inline   : immediate also inline '; (]]) ;


: inline ( "name" -- )
   ' ( xt ) dup >end swap >body ( body end )
   do
      i @ ( do something special with exit ) compile,
   cell +loop ; immediate

: nt>xt ( nt -- xt )
   dup nt>compile swap nt>interpret ( cxt ixt )
   over 0= if ( no compile semantics ) nip exit then
   dup  0= if ( no interpret semantics ) drop exit then
   2dup =  if ( both same semantics ) drop exit then
   ( two different semantics ) ... ;

\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ 

\ input>r   postpone save-input postpone n>r ; immediate
\ r>input   postpone nr> postpone restore-input postpone drop ; immediate

\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ 

\ E.g. ?? exit.

: ?? ( x "word" -- )    postpone if  ' compile,  postpone then ; immediate
: 0?? ( x "word" -- )   postpone 0=  postpone ?? ; immediate

\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ 

: name-processor:   create ' , ' , ' ,
   does> >r find-name r> swap 1+ cells + @ execute ;

name-processor: skip-name        end? 2drop end?
name-processor: interpret-name   execute number execute
name-processor: compile-name     compile, number execute
name-processor: postpone-name    postpone, abort compile,
name-processor: macro-name       postpone, postpone-number, compile,

\ E.g.
\ : end? ( u1 xt -- u2 )   dup ['] (* = if drop 1
\    else ['] *) = if 1- then then ;
\ : (*   1 begin ?dup while next-name skip-name repeat ; immediate
