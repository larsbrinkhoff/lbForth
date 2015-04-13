: later>   2r> r> -rot 2>r >r ;
: base> ( u -- ) base @ >r base !  later> r> base ! ;

: parse-number   parse-name number ;
: b#   2 base> parse-number ; immediate
: o#   8 base> parse-number ; immediate
: d#   10 base> parse-number ; immediate
: h#   16 base> parse-number ; immediate
