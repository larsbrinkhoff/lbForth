require lib/macros.fth
: :nest   reveal csp @ 0 csp ! ;
: ;nest   csp ! ] ;
: [: ( C: -- orig xt ) :nest  postpone ahead  :noname ; compile-only
: ;] ( C: orig xt -- ) swap ]] ; then literal [[ ;nest ; compile-only
