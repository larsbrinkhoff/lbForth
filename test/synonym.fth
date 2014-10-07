\ Test that SYNONYM works.

\ Four classes of semantics:
synonym plus +       \ Standard.
synonym comment \    \ Immediate.
synonym endif then   \ Compile-only.
synonym assign to    \ Dual.

: ?fail   <> abort" FAIL" ;
0 value x

.( Test interpretation: )
1 2 plus assign x comment foo
x 3 ?fail
.( PASS ) cr

.( Test compilation: )
: foo   3 4 5 if plus assign x endif comment ;
;
foo x 7 ?fail
.( PASS ) cr

.( Test postpone: )
: bar   postpone if
        postpone plus
        postpone assign
        postpone endif
        postpone comment ; immediate
: baz   6 7 8 bar x ;
;
baz x 13 ?fail
.( PASS ) cr
