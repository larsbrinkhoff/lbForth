s" search.fth" included

vocabulary target

0 value image
0 value image-end
: >image ( haddr -- iaddr )   image - ;
: >index ( haddr -- index )   >image /cell / ;
: image? ( haddr -- flag )    >index 0 10240 within ;

: image-buffer:   create 10240 allot  does> swap >index + ;

image-buffer: reloc
: reloc! ( haddr -- )        1 swap reloc c! ;
: string! ( haddr -- )       2 swap reloc c! ;
: reloc? ( haddr -- flag )   reloc c@ ;

: t-address, ( a -- )    here reloc! , ;
: t-compile, ( xt -- )   t-address, ;
: t-string, ( a -- )     here string! , ;

: create-image
   ." Load target into host" cr
   \ First, load the target into the host.
   \ This makes the target immediate words available.
 \ s" kernel.fth" included
   \ hide the host words to the compiler
   \ keep the target words for the interpreter
   here to image
   ." Image starts at " image u. cr
   \ 0 revealedxt !
   also target definitions
   s" core.fth" included
   ." Words: " words
   previous
   here to image-end ;

: fix-addr ( haddr -- )
   \ We're looking at an address outside the image.
   \ It must be a host xt, so replace it with a target xt.
   dup @ find 0= abort" Bad xt" swap ! ;

: .reloc ( haddr -- )
   dup @ image? 0= if dup fix-addr then
   ."   &data[" @ >index . ." ]", ;

: "   [char] " emit ;

: save-image
   ." #include " " ." forth.h" " cr
   ." cell data[] = {" cr
   image-end image do
      i reloc? if
         i .reloc
      else
   	 ."   " i ? ." ,"
      then cr
   /cell +loop
   ." };" cr ;

: meta-compile   create-image  save-image ;

((
create code-line 128 allot

: code ( "name" -- )
    here 0 header,
    ." xt_t * REGPARM "
    count type
    ." _code (xt_t *IP, struct word *word)" cr
    ." {" cr
    begin
       code-line 128 accept
       code-line over s" end-code" compare 0=
    while
       type
    repeat
    ." }" cr ;

: C ( "word" -- )
   bl word
   \ save string, return addres
   t-string, ;
))
