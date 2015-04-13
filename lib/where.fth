: bounds' ( xt -- end start)   dup >end swap >body ;
: .nt ( nt -- nt )   dup id. ;
: where' ( xt nt -- xt flag )
   dup bounds' do over i @ = if .nt leave then cell +loop drop 1 ;
: where ( "name" -- )   ' context @ ['] where' traverse-wordlist drop ;
