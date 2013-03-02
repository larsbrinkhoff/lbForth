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
finders  pp-xt   postpone, ppn, compile,
: [[<>   ['] [[ <> ;
: ]]     begin bl word find over [[<>
         while pp-xt repeat 2drop ; immediate
