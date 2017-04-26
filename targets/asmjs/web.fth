: 0inner   s" .innerHTML = '';" holds ;
: 0div   2>r 0 0 <# 0inner 2r> holds #> js-eval ;
: +inner   s" .innerHTML += '" holds ;
: +div   2>r 0 <# s"  ';" holds #s +inner 2r> holds #> js-eval ;

: 0stack   s" stack" 0div ;
: +stack   s" stack" +div ;
: .items   depth 0 ?do depth i - 1- pick +stack loop ;
: .stack   0stack .items ;

: .unused   s" unused" 0div  unused s" unused" +div ;

: .web   .stack .unused ok ;
' .web terminal-source 4 cells + !
.unused
