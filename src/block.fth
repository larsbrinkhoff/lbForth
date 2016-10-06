( Block words. )

variable blk
variable current-block
variable block-updated
create 'block   1024 allot

: block-refill   0 ;

create block-source   0 0 ' block-refill ' noop source,

: block ( n -- addr ) current-block ! 'block  0 block-updated ! ;

: buffer ( n -- addr ) current-block ! 'block  0 block-updated ! ;

: empty-buffers   0 block-updated ! ;
: save-buffers   0 block-updated ! ;
: flush   save-buffers empty-buffers ;

: load ( ... n -- ... )
    ." =========== LOAD ===========" cr
    .s cr
    blk !
    save-input drop 2>r
    0 >in !
    blk @ block 'source !  1024 #source !
    ( interpret )
    2r> 2 restore-input abort" Bad restore-input" .s ;
: thru ( x y -- )   1+ swap do i load loop ;

: update   1 block-updated ! ;

variable  scr
: list ( n -- ) dup scr !  block 1024 bounds do i @ emit loop ;

\ refill (extended semantics)
\ \ (extended semantics)
