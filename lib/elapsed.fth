include targets/x86/linux/gettime.fth
include double.fth

2variable 'elapsed

: 0elapsed   gettime 'elapsed 2! ;
: elapsed   gettime 'elapsed 2@ d- ;
: 9#   # # # # # # # # # ;
: .elapsed   elapsed (.) 0 <# 9# [char] . hold #> type ."  seconds elapsed" ;
