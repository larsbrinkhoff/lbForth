
variable xt
: (outer)    xt @ execute ;
: [:         postpone (outer) postpone ; :noname ; immediate
\ ([does>)   >r :noname r> literal ;
\ [does>     postpone ([does>) postpone [: ; immediate
: ;]         postpone ; >r :noname r> postpone literal ; immediate
: ;          postpone ; xt ! ; immediate

\ : unresolved-literal ( C: -- orig )   postpone (literal) >mark ;
\ : resolve-literal ( C: orig -- )      swap ! ;

\ : foo   noname-create , does> @ ;
\ : foo   >r :noname r> postpone literal postpone ; ;

( Usage:   : foo 100 [: 42 + ;] execute ; )

( : counter   dup . [create , does> tuck +! @ ;] ." is enclosed" ;
  100 counter value f
  1 f execute . )


\ variable temp
\ : "create"   here temp ! ;
\ : "does>"    :noname temp @ postpone literal r> postpone again postpone ; ;
\ : my-const   "create" , "does>" @ ;

\ TODO: use "code!"
: does-code!      [ ' dodoes >code @ ] literal  swap >code ! ;
: noname-create   :noname ?csp does-code! reveal lastxt @ postpone [ ;

0 value q-xt
0 value q-dest
: [:          postpone ahead  postpone begin to q-dest ; immediate
: ([create)   r>  noname-create >r  dup @ >r  cell+ >r ;
: [create     postpone ([create) >mark  0 to q-dest ; immediate
: [does>      postpone [create postpone , postpone does> postpone @ ; immediate
: ;]   	      postpone exit  postpone then
              q-dest if postpone (literal) >mark to q-xt
              else postpone r> then ; immediate
: (q;)        :noname  q-dest postpone again  postpone ;
              q-xt !  0 to q-dest ;
: ;           postpone ;  q-dest if (q;) then ; immediate

( : counter   dup . here swap , [does> tuck +! @ ;] ." is enclosed" ;
  100 counter value f
  1 f execute . )


\ Closures with shared state.
\ : counter ( n -- xt1 xt2 xt3 )
\    here swap ,  dup [does> ! ;]  over [does> @ ;]  rot [does> +! ;] ;
\ : setter   create , does> @ ! ;
\ : getter   create , does> @ @ ;
\ : counter ( n "name1 name2 name3" -- )
\    here swap ,  dup setter  dup getter  dup create , does> @ +! ;
