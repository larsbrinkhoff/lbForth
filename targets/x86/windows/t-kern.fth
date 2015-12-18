: io-init   -10 get-std-handle to stdin ;

: cr   13 emit 10 emit ;

: r/o   2147483648 3 ; \ generic-read open-existing
: w/o   1073741824 2 ; \ generic-write create-always
: r/w   3221225472 4 ; \ generic-read/write open-always
