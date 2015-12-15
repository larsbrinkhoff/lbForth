\ Save the live image to a file.

require lib/elf.fth

: msize ( -- u ) limit @ image0 - ;
: fsize ( -- u ) here image0 - ;
: msize!   msize elf-mem-size ;
: fsize!   image0 elf-start  fsize elf-file-size ;
: dp0!    here ['] dp0 >body ! ;
: ?error   abort" write" ;
: ?size   fsize <> abort" size" ;

variable f
: open-image ( a u -- ) w/o open-file abort" open" f ! ;
: write-image   image0 fsize f @ write-file ?error ?size ;
: patch-image   fsize! msize! dp0! ;
: save-image ( a u -- ) open-image patch-image write-image f @ close-file ;

' quit is turnkey
