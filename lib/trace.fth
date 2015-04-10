defer trace
: trace-off   ['] 2drop is trace ;
trace-off
: .word   ." [" type ." ]" ;
: trace-word   ['] .word is trace ;
: .stack   ." [" type space .s ." ]" cr ;
: trace-stack   ['] .stack is trace ;
: old-:   : ;
: trace-:   >in @ parse-name 2>r >in ! : 2r> postpone sliteral postpone trace ;
defer :
' old-: is :
: trace-begin   ['] trace-: is : ;
: trace-end   ['] old-: is : ;
