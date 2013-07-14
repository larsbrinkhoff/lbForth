( Metacompiler for 6502 FIG Forth model. )

require search.fth

(* Notes:
 * Loaded consecutively: 12-32, 33-71, 72-79.
 * SCR # 23 dated 1980; use of C;.
 * SCR # 45 text beoynd column 64.
 * SCR # 61 uses . which is defined in SCR # 76.
 * SCR # 68 unbalanced stack.
 * SCR # 72 dated dec 79; U< is undefined; text is small.
 * SCR # 76 line 13 is strange.
 *)

only forth definitions
decimal

vocabulary target
vocabulary assembler
vocabulary meta-compiler
vocabulary meta-interpreter

: scr   parse-name s" #" compare abort" #?"
   ." SCR # " parse-name type ." :  "
   16 0 do
      refill 0= abort" Refill?" parse-name 2drop interpret
   loop cr depth abort" Stack?" ;

: search-interpreter   only also meta-interpreter ;
: interpreter-defs   search-interpreter definitions previous ;
: search-assembler   search-interpreter also assembler ;
: assembler-defs   only also assembler definitions ;
: compiler-defs   only also meta-compiler definitions previous ;
: search-target   only previous target also meta-compiler ;
: target-defs   search-target also target definitions previous ;
: interest   search-interpreter  refill drop ;

: (>defs)   get-current r> 2>r set-current ;
: (defs>)   r> r> set-current >r ;
: [definitions]   ' postpone literal  postpone (>defs)  ' compile,
   postpone (defs>) ; immediate

: nop:   : postpone ; ;
: drop:   : postpone drop postpone ; ;
: label:   : 1 postpone literal postpone ; ;
: forward:   [definitions] target create ;
: immediate:   >in @ >r : r> >in !  ' compile, postpone ; immediate ;

create dummy 2 cells allot

interpreter-defs

: create   >in @ ( ." CREATE " ) parse-name type 2 spaces >in !
   [definitions] target create ;
: code   >in @ ( ." CODE " ) parse-name type 2 spaces >in !
   [definitions] target create search-assembler ;
: label   >in @ ( ." LABEL " ) parse-name type 2 spaces >in !
   here [definitions] assembler constant ;
: constant   >in @ ( ." CONSTANT " ) parse-name type 2 spaces 2dup >in !
   [definitions] meta-interpreter constant  >in !
   [definitions] target constant ;
: variable   >in @ ( ." VARIABLE " ) parse-name type 2 spaces >in !
   drop [definitions] target variable ;
: user   >in @ ( ." USER " ) parse-name type 2 spaces >in !
   [definitions] target constant ;
: :   >in @ parse-name type 2 spaces >in !
   [definitions] target create ] search-target ; immediate
: vocabulary   >in @ parse-name type 2 spaces >in !
   [definitions] target create ;
: '   ( ." TICK " ) parse-name 2drop ( type 2 spaces )  dummy cell+ ;
: 't   ( ." TICK-TARGET " ) parse-name 2drop ( type 2 spaces )  1 ;
: assembler   search-assembler ;

: decimal   ;
: object   ;
: mem   ;
: ;s   ;
: c;   ;
: byte.in   drop parse-name 2drop dummy ;
: replaced.by   drop parse-name 2drop ;
: immediate   ;
: -->   ;
: +origin   drop dummy ;
: smudge   ;
: latest   1 ;
: not      invert ;
: 2+       2 + ;
variable fence

assembler-defs  hex

\ : [compile]   ' , ; immediate
\ : compile   ['] (literal) ,  ' ,  ['] , , ; immediate
\ : lit,   compile (literal) , ;

: #    ;
: .a   A ;
: ,x   ;
: ,y   ;
: x)   ;
: )y   ;

drop: adc,  drop: dec,  drop: jmp,  drop: ora,  drop: sbc,  nop: tax,   
drop: and,  nop: dex,   drop: jsr,  nop: php,   nop: sec,   nop: tay,   
drop: asl,  nop: dey,   drop: lda,  nop: pla,   drop: sta,  nop: txa,   
drop: bit,  drop: eor,  drop: ldx,  nop: plp,   drop: stx,  nop: tya,   
nop: clc,   drop: inc,  drop: ldy,  drop: rol,  nop: pha,   nop: tsx,   
drop: cpy,  nop: inx,   drop: lsr,  nop: rts,   drop: sty,  nop: txs,   
drop: cmp,  nop: iny,   nop: nop,   

: n        4 ;
: cs       ;
: 0=       ;
: 0<       ;
: begin,   BEC14 ;
: end,     drop ;
: until,   BEC14 - abort" Until,?" ;
: if,      1F ;
: then,    drop ;
: endif,   then, ;

label: xsave   label: bot    label: pop     label: ip  label: w
label: sec     label: push   label: push0a  label: r   label: up
label: poptwo  label: next

compiler-defs

: ;   postpone [ search-interpreter ; immediate
: ;code   postpone [ search-assembler ; immediate
: [compile]   only previous target postpone [compile] search-target ; immediate

immediate: (       immediate: literal  immediate: if     immediate: else
immediate: then    immediate: begin    immediate: until  immediate: while
immediate: repeat  immediate: again    immediate: do
immediate: loop    immediate: ."

: endif   postpone then ; immediate

interpreter-defs

\ From screen 70 and 71
forward: ?EXEC    forward: !CSP    forward: CURRENT  forward: CONTEXT
forward: CREATE   forward: ]       forward: (;CODE)  forward: ?CSP
forward: COMPILE  forward: SMUDGE  forward: [        forward: ,
forward: ERROR    forward: WORD    forward: ABORT    forward: MESSAGE
forward: QUIT     forward: BLOCK   forward: MIN      forward: DR0
forward: R/W

\ Needed in screen 61, defined in 76.
forward: .
\ Needed in screen 72, not defined anywhere.
forward: u<

hex  search-interpreter
include experiments/fig.fth
decimal  only forth definitions
