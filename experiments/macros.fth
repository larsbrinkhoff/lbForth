: input>r   postpone save-input postpone n>r ; immediate
: r>input   postpone nr> postpone restore-input postpone drop ; immediate

: [[     ; immediate
\ '<>    input>r ' r>input <> ;
: '<>    >in @ ' swap >in ! <> ;
: (]])   begin dup '<> while postpone postpone repeat drop ;
: ]]     ['] [[ (]]) ; immediate

( Usage:   : foo ]] dup * [[ ; immediate   : bar 42 foo . ; )

\ Support for numbers.
: [[     ; immediate
: ppn,   number ['] literal compile, ;
finders  pp-xt  postpone, ppn, compile,
: [[<>   ['] [[ <> ;
: ]]     begin bl word find over [[<>
         while pp-xt repeat 2drop ; immediate

\ Test case: traverse-wordlist with possibly inlined xt.

: <lit? ( -- flag )    here 2 cells - @ ['] (literal) = ;
: <lit ( -- x )        here 1 cells - @ ;
: ?xt ( xt -- xt|0 )   dup xt? if else drop 0 then ;
: <xt ( -- xt|0 )      <lit? if <lit ?xt else 0 then ;

: tr-wl
   <xt ?dup if >r
      -2 cells allot
      ]] >body @ begin ?dup while dup >r [[ r> compile, ]] r> swap if >nextxt else exit then repeat [[
   else postpone traverse-wordlist then ; immediate
: tr-wl
   get-previous-xt ?dup if >r
      here 2 cells - here!
      ]] >body @ begin ?dup while
    	    dup >r [[ r> compile, ]] r> swap
    	    if >nextxt else exit then
    	 repeat [[
   else postpone traverse-wordlist then ; immediate

