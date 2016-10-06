( Block words. )

variable blk
variable current-block
create dummy-block   1024 allot

: block-refill   0 ;

create block-source   0 0 ' block-refill ' noop source,

: block ( n -- addr )
    current-block ! dummy-block ;

: buffer ( n -- addr )
    current-block ! dummy-block ;

: flush ( -- ) ;

: load ( ... n -- ... )
    blk !
    save-input drop 2>r
    0 >in !
    blk @ block 'source !  1024 #source !
    ( interpret )
    2r> 2 restore-input abort" Bad restore-input" ;

: save-buffers ( -- ) ;
: update ( -- ) ;

( Block extension words. )

: empty-buffers ( -- ) ;

variable  scr

: list ( n -- ) dup scr !  block 1024 bounds do i @ emit loop ;

\ refill (extended semantics)

: thru ( x y -- )   1+ swap do i load loop ;

\ \ (extended semantics)
