\ Assembler for PDP-1.

\ Adds to FORTH vocabulary: ASSEMBLER CODE ;CODE.
\ Creates ASSEMBLER vocabulary with: END-CODE and PDP-1 opcodes.

s" search.fth" included

vocabulary assembler
also assembler definitions

1 value cross?
cross? [if] s" experiments/target-pdp1.fth" included [then]

0 value code-start

\ (;code)      r> code! ;
: start-code   here to code-start  also assembler ;
: end-code     code-start align previous ;

base @  8 base !

0 value offset

: >addr ( addr -- u )   offset - cell / 17777 and ;
: mem, ( addr u -- )    swap >addr or , ;
: i ( a1 -- a2 )        [ 10000 cells offset + ] literal + ;
: +. ( u -- addr )      cells here + ;
: >immediate ( n -- n ) dup 7777 > abort" Immediate too large"
                        dup -7777 < abort" Immediate too small"
                        dup 0< if negate 10000 or then ;

: and,    020000 mem, ;
: ior,    040000 mem, ;
: xor,    060000 mem, ;
: xct,    100000 mem, ;
\ lch,    120000 \ PDP-1D
\ idz,    120000 \ ???
\ dch,    140000 \ PDP-1D
\ jdp,    140000 \ ???
: cal,    160000 or , ;
: jda,    170000 mem, ;
: lac,    200000 mem, ;
: lio,    220000 mem, ;
: dac,    240000 mem, ;
: dap,    260000 mem, ;
: dip,    300000 mem, ;
: dio,    320000 mem, ;
: dzm,    340000 mem, ;
: tad,    360000 mem, ; \ PDP-1D
\ adm,    360000 \ ???
: add,    400000 mem, ;
: sub,    420000 mem, ;
: idx,    440000 mem, ;
: isp,    460000 mem, ;
: sad,    500000 mem, ;
: sas,    520000 mem, ;
: mul,    540000 mem, ;
: div,    560000 mem, ;
: jmp,    600000 mem, ;
: jsp,    620000 mem, ;
: skp,    640000 or , ;
: szf,    skp, ;
: szs,    skp, ;
: sza,    000100 skp, ;
: spa,    000200 skp, ;
: sma,    000400 skp, ;
: szo,    001000 skp, ;
: spi,    002000 skp, ;
: sni,    004000 skp, ; \ PDP-1D
: clo,    011600 skp, ;
: spq,    010500 skp, ; \ PDP-1D
: szm,    000500 skp, ; \ PDP-1D
: sft,    660000 or , ;
: ral,    001000 or sft, ;
: ril,    002000 or sft, ;
: rcl,    003000 or sft, ;
: sal,    005000 or sft, ;
: sil,    006000 or sft, ;
: scl,    007000 or sft, ;
: rar,    011000 or sft, ;
: rir,    012000 or sft, ;
: rcr,    013000 or sft, ;
: sar,    015000 or sft, ;
: sir,    016000 or sft, ;
: scr,    017000 or sft, ;
: 1s   	  000001 ;
: 2s   	  000003 ;
: 3s   	  000007 ;
: 4s   	  000017 ;
: 5s   	  000037 ;
: 6s   	  000077 ;
: 7s   	  000177 ;
: 8s   	  000377 ;
: 9s   	  000777 ;
: law,    700000 swap >immediate or , ;
: iot,    720000 or , ;
\ wait    010000
\ cpls    004000
: ioh,    010000 iot, ; \ I/O wait
: mse,    000301 iot, ; \ Select, DECtape, dt
: mlc,    000401 iot, ; \ 
: mrd,    000501 iot, ; \ Read
: mwr,    000601 iot, ; \ Write
: mrs,    000701 iot, ; \ Read status
: rpa,    010001 iot, ; \ Paper tape reader, alpha mode, ptr
: rpb,    000002 iot, ; \ Paper tape reader, binary mode, ptr
: tyo,    010003 iot, ; \ Typewriter, tto
: tyi,    000004 iot, ; \ Keyboard, tti
: ppa,    010005 iot, ; \ Paper tape punch, ptp
: ppb,    010006 iot, ; \ Paper tape punch, ptp
\ dpy,    010007 iot, ; \ Display point
\ lrg,    000010 iot, ; \ PDP-1D: Leave ring mode
\ erg,    000011 iot, ; \ PDP-1D: Enter ring mode
\ rro,    000017 iot, ; \ PDP-1D: Rem-rand out
\             22        \ Data comm sys, dcs
: rch,    000022 iot, ; \ Receive character
: rrc,    000122 iot, ; \ Read receive counter
: rcc,    001022 iot, ; \ Receive character and release scanner
\ rcr,    rcc, ;        ???
: rsc,    001122 iot, ; \ Release scanner
: tcb,    004022 iot, ; \ Transmit character from send buffer
: ssb,    004122 iot, ; \ Set send buffer
: tcc,    005022 iot, ; \ Transmit character from receiver counter and release
: ckn,    000027 iot, ; \ timesharing
: rrb,    000030 iot, ; \ Paper tape reader
\             32        \ Clock, clk
\ rck,    000032 iot, ; \ Read clock
: cks,    000033 iot, ; \ Check status
\             35        \ Check trap buf
\ ctb,    000035 iot, ; \ Clear trap buffer
: rbt,    000237 iot, ; \ timesharing
\ rri,    000037 iot, ; \ PDP-1D: Rem-rand in
\             45        \ Line printer, lpt
: dsc,    000050 or iot, ; \ Deactivate sequence break, timesharing
: asc,    000051 or iot, ; \ Activate sequence break, timesharing
: isb,    000052 or iot, ; \ Initiate sequence break, timesharing
: cac,    000053 iot, ; \ Clear all channels, timesharing
: lsm,    000054 iot, ; \ Leave sequence break mode
: esm,    000055 iot, ; \ Enter sequence break mode
: cbs,    000056 iot, ; \ Clear sequence break
: dia,    000060 iot, ;
: dba,    002061 iot, ; \ Drum break address
\ dia       xx61
: dcc,    000062 iot, ; \ Drum
\ dwc       xx62
\ dra       2062          Drum request address
: dra,    000063 iot, ; \ Drum
\ dcl       0063
\             64        \ Drum
\ lrm,    000064 iot, ; \ PDP-1D: Leave restrict mode
\ erm,    000065 iot, ; \ PDP-1D: Enter restrict mode
\ rnm,    000066 iot, ; \ PDP-1D: Rename memory
: rsm,    000067 iot, ; \ PDP-1D: Reset memory banks
: eem,    004074 iot, ; \ Enter extend mode
: lem,    000074 iot, ; \ Leave extend mode
: bpt,    002177 iot, ; \ timesharing
: arq,    002277 iot, ; \ timesharing
: dsm,    002377 iot, ; \ timesharing
: wat,    002477 iot, ; \ timesharing
: lei,    004577 iot, ; \ timesharing
: lea,    004677 iot, ; \ timesharing
: rer,    004777 iot, ; \ timesharing
: nmn,    005377 iot, ; \ timesharing
: nmf,    005477 iot, ; \ timesharing
: opr,    770000 xor , ;
: nop,    000000 opr, ;
: clf,    opr, ;
: stf,    000010 or opr, ;
: lia,    000020 opr, ; \ PDP-1D
: lai,    000040 opr, ; \ PDP-1D
: lap,    000300 opr, ; \ cla
: cla,    000200 opr, ;
: hlt,    000400 opr, ;
: cma,    001000 opr, ;
: lat,    002200 opr, ; \ cla
: cli,    004000 opr, ;
: cmi,    010000 opr, ; \ PDP-1D
\ swp,    000060 opr, ; \ PDP-1D
\ swp,    9s rcl, 9s rcl, ; \ PDP-1C
: clc,    001200 opr, ;

base !  previous definitions  also assembler

: code    header, reveal start-code ;
\ ;code   postpone (;code) reveal postpone [ ?csp start-code ; immediate

previous
