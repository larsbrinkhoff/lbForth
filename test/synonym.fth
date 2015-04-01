\ Test that SYNONYM works.

\ Four classes of semantics:
synonym plus +       \ Standard.
synonym comment \    \ Immediate.
synonym endif then   \ Compile-only.
synonym assign to    \ Dual.

0 value x
: ?fail   x <> abort" FAIL" ;

.( Test interpretation: )
1 2 plus assign x comment foo
x 3 ?fail
.( PASS ) cr

.( Test compilation: )
: t1   3 4 5 if plus assign x endif comment  This is a comment.
;
t1 x 7 ?fail
.( PASS ) cr

.( Test postpone: )
: t2   postpone if
       postpone plus
       postpone assign
       postpone endif
       postpone comment ; immediate
: t3   6 7 8 t2 x  This is a comment.
;
t3 x 13 ?fail
: t4   postpone assign ;
: t5   [ t4 x ] ;
9 t5
x 9 ?fail
.( PASS ) cr

.( Test tick: )
10 ' assign execute x
x 10 ?fail
: t5   [ ' assign ] literal execute ; immediate
: t6   [ 11 ] t5 x ;
x 11 ?fail
: t7   ['] assign execute ; immediate
: t8   [ 12 ] t7 x ;
x 12 ?fail
.( PASS ) cr
