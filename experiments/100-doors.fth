\ Rosetta Code: 100 doors

100 constant n
: array   create here n erase n allot does> + ;
array door

: toggle   door dup c@ invert swap c! ;
: pass   dup 1+ n rot do i toggle dup +loop drop ;
: passes   0 do i pass loop ;

: .open   door c@ if ." open" else ." closed" then ;
: .door   ." door " dup 1+ . ." is " .open cr ;
: output   n 0 do i .door loop ;

: answer   100 passes  output ;
