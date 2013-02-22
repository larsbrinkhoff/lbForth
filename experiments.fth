: slow-fib   dup 1 > if dup 1- recurse swap 2 - recurse + then ;
: fast-fib   0 1 rot 0 do tuck + loop ;

: factorial   dup 2 < if drop 1 else  dup 1- recurse * then ;

: ([ex])   r@ find 0= abort" not found" execute  r> count + aligned >r ;
: [execute]   postpone ([ex])  bl word count nip 1+ allot align ; immediate

: make-accumulator   create ,  does> tuck +! @ ;
: counter   create 0 ,  does> 1 over +! @ ;

\ TODO: 1. find end of word, 2. relative branch targets.
\ : :inline   create immediate ] does> @ compile, ;

: dispatch ( low high "name words..." -- )
           ( name execution: index -- )
    create  1+ swap  dup negate 1+ ,  do ' , loop
    does> tuck @ + cells + @ ;

: foo   if begin repeat ;
\       begin while while repeat then ;

\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ 
: (redef)   create immediate here 0 , 0 ,
            does> here swap @+ compile, ! ;
: :redef?   (redef) :noname swap ! ;
: :redef!   :noname dup  ' >body !+  @ ! ;

( Usage:   :redef? foo 42 + ;
           : bar foo ;
           :redef! foo 42 - ; )

\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ 
variable inner-xt   variable outer-xt
: (outer)    outer-xt @ execute ;
: (inner)    inner-xt @ ;
: [:noname   postpone (outer) postpone ; :noname ; immediate
: [:         postpone (outer) postpone ; 0 : ; immediate
: ;]         postpone ; ?dup if inner-xt ! :noname postpone (inner)
             else :noname then ; immediate
: ;          postpone ; outer-xt ! ; immediate

( Usage:   : foo 100 [:noname 42 + ;] execute ; )
