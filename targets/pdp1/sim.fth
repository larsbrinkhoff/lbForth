\ PDP-1 simulator.

base @  8 base !

0 value image
0 value pc
0 value ac
0 value io
0 value flags

: ones+ ( x y -- z )   + dup 1000000 and if 1+ then 777777 and ;
: ones- ( x y -- z )   777777 xor ones+ ;

: image> ( ia -- a )   cells image + ;
: image@ ( ia -- x )   image> @ ;
: image! ( x ia -- )   image> ! ;
: fetch ( -- x )       pc image@  1 +to pc ;
: indirect?            010000 and ;
: ?indirect ( ia x )   indirect? if image@ then ;
: ea ( x -- y )        dup 007777 and  swap ?indirect ;
: mem ( x -- y )       ea image@ ;
: immediate ( x -- y ) dup 007777 and  swap indirect? if 777777 xor then ;
: none ( x -- x )      ;
: positive? ( x -- f ) 100000 and 0= ;
: ?skip ( flag -- )    if 1+ to pc then ;

: and ( x -- )     ac and to ac ;
: ior ( x -- )     ac or to ac ;
: xor ( x -- )     ac xor to ac ;
: xct ( x -- )     ... ;
: lac ( x -- )     to ac ;
: lio ( x -- )     to io ;
: dac ( ia -- )    ac swap image! ;
: dap ( ia -- )    dup image@ 770000 and  ac 007777 and + swap image! ;
: dip ( ia -- )    dup image@ 007777 and  ac 770000 and + swap image! ;
: dio ( ia -- )    io swap image! ;
: dzm ( ia -- )    0 swap image! ;
: add ( x -- )     ac ones+ to ac ;
: sub ( x -- )     ac ones- to ac ;
: idx ( ia -- )    dup image@ 1 ones+  dup to ac  swap image! ;
: isp ( ia -- )    idx  ac positive? ?skip ;
: sad ( x -- )     ac <> ?skip ;
: sas ( x -- )     ac = ?skip ;
: jmp ( x -- )     to pc ;
: jsp ( x -- )     pc to ac  to pc ;
: law ( x -- )     to ac ;
: skip ( x -- )    ... ?skip ;

: opr ( x -- )
   dup 004000 and if 0 to io then		\ cli
   dup 000200 and if 0 to ac then		\ cla
   dup 002000 and if ac tw or to ac then	\ lat
   dup 000100 and if ac pc or to ac then	\ lap
   dup 001000 and if ac 777777 xor to ac then	\ cma
   dup 010000 and if io 777777 xor to io then	\ cmi
   io over 000020 and if ac to io then		\ lia
   dup 000040 and if swap to ac else nip then	\ lai
       000400 and if 1 throw then ;		\ halt

: -> ( op m insn operand -- )   , , ' , ' , ;

create decode-start
\  op     mask      insn operand
   020000 760000 -> and  mem	\ AC = AC & M[MA]
   040000 760000 -> ior  mem	\ AC = AC | M[MA]
   060000 760000 -> xor  mem	\ AC = AC ^ M[MA]
   100000 760000 -> xct  mem	\ execute M[MA]
 \ 110000 760000 -> lch
 \ 120000 760000 -> dch
   160000 770000 -> cal  none	\ M[100] = AC, AC = PC, PC = 101
   170000 770000 -> jda  mem	\ M[MA] = AC, AC = PC, PC = MA + 1
   200000 760000 -> lac  mem	\ AC = M[MA]
   220000 760000 -> lio  mem	\ IO = M[MA]
   240000 760000 -> dac  ea	\ M[MA] = AC
   260000 760000 -> dap  ea
   300000 760000 -> dip  ea
   320000 760000 -> dio  ea	\ M[MA] = IO
   340000 760000 -> dzm  mem	\ M[MA] = 0
   360000 760000 -> tad  mem
   400000 760000 -> add  mem	\ AC = AC + M[MA]
   420000 760000 -> sub  mem	\ AC = AC - M[MA]
   440000 760000 -> idx  ea	\ AC = M[MA] = M[MA] + 1
   460000 760000 -> isp  mem	\ AC = M[MA] = M[MA] + 1, skip if AC >= 0
   500000 760000 -> sad  mem	\ if (AC != M[MA]) PC = PC + 1
   520000 760000 -> sas  mem	\ if (AC == M[MA]) PC = PC + 1
   540000 760000 -> mul  mem
   560000 760000 -> div  mem
   600000 760000 -> jmp  ea	\ PC = MA
   620000 760000 -> jsp  ea	\ AC = PC, PC = MA
   640000 760000 -> skip skip
   \ 000x szf
   \ 00x0 szs
   \ 0100 sza			AC=0
   \ 0200 spa			AC:0=0
   \ 0400 sma			AC:0=1
   \ 1000 szo			OV=0 (clear flag)
   \ 2000 spi			IO:0=0
   \ 4000 sni			IO!=0 PDP-1D
 \ 651600 clo
   661000 777000 -> ral  shift
   662000 777000 -> ril  shift
   663000 777000 -> rcl  shift
   665000 777000 -> sal  shift
   666000 777000 -> sil  shift
   667000 777000 -> scl  shift
   671000 777000 -> rar  shift
   672000 777000 -> rir  shift
   673000 777000 -> rcr  shift
   675000 777000 -> sar  shift
   676000 777000 -> sir  shift
   677000 777000 -> scr  shift
   700000 760000 -> law  immediate
   720000 760000 -> iot  i/o
 \ 010000 -> wait for completion
 \ 004000 -> completion pulse
 \ 003700 -> opcode
 \ 000077 -> device
 \ 730003 tyo
 \ 730004 tyi
 \ 730005 ppa
 \ 740000 760000 -> opr  special operate PDP-1D
   760000 760000 -> opr  operate
  \ 00000 nop
  \ 0000x clf
  \ 0001x stf
  \ 00020 lia PDP-1D
  \ 00040 lai PDP-1D
  \ 00100 lap
  \ 00200 cla
  \ 00400 halt
  \ 01000 cma
  \ 02000 lat
  \ 04000 cli
  \ 10000 lia PDP-1D
  \ 10000 lia PDP-1D
   000000 000000 -> unknown none
here constant decode-end

\ FIODEC
\ CODE	LOWER	UPPER
\  00	Space
\  01	1	"
\  02	2	'
\  03	3	~
\  04	4	⊃ U+2283 <implies>
\  05	5	∨ U+2228 <or>
\  06	6	∧ U+2227 <and>
\  07	7	<
\  10	8	>
\  11	9	↑ U+2191 <up arrow>
\ (12)	*	! (ASCII addition)
\  13	Stop code
\ (14)	^	# (ASCII addition)
\ (15)	{	$ (ASCII addition)
\ (16)	\	% (ASCII addition)
\ (17)	}	& (ASCII addition)
\  20	0	→ U+2192 <right arrrow>
\  21	/	?
\  22	s	S
\  23	t	T
\  24	u	U
\  25	v	V
\  26	w	W
\  27	x	X
\  30	y	Y
\  31	z	Z
\ (32)	:	; (ASCII addition)
\  33	,	=
\  34	<black>
\  35	<red>
\  36	tab
\  37 ------------
\  40	·	_ U+00B7 <non-spacing middle dot and underline>
\  41	j	J
\  42	k	K
\  43	l	L
\  44	m	M
\  45	n	N
\  46	o	O
\  47	p	P
\  50	q	Q
\  51	r	R
\  52 ------------
\  53 ------------
\  54	-	+
\  55	)	]
\  56	-	| <non-spacing overstrike and vertical bar>
\  57	(	[
\ (60)	`	@ (ASCII addition)
\  61	a	A
\  62	b	B
\  63	c	C
\  64	d	D
\  65	e	E
\  66	f	F
\  67	g	G
\  70	h	H
\  71	i	I
\  72	Lower Case
\  73	.	× U+00D7 <multiply>
\  74	Upper Case
\  75	Backspace
\  76 ------------
\  77	Carriage Return

\ ASCII	FIODEC
\ CODE	CODE
\ 04	13		End of Transmission / Stop code
\ 08	75		Backspace
\ 09	36		Tab
\ 0D	77		Carriage Return
\ 20	00		Space
\ 21			!
\ 22	01	U	"
\ 23			#
\ 24			$
\ 25			%
\ 26			&
\ 27	02	U	'
\ 28	57	L	(
\ 29	55	L	)
\ 2A	73	U	* / × U+00D7 <multiply>
\ 2B	54	U	+
\ 2C	33	L	,
\ 2D	54	L	-
\ 2E	73	L	.
\ 2F	21	L	/
\ 30	20	L	0
\ 31	01	L	1
\ 32	02	L	2
\ 33	03	L	3
\ 34	04	L	4
\ 35	05	L	5
\ 36	06	L	6
\ 37	07	L	7
\ 38	10	L	8
\ 39	11	L	9
\ 3A			:
\ 3B			;
\ 3C	07	U	<
\ 3D	33	U	=
\ 3E	10	U	>
\ 3F	21	U	?
\ 40			@
\ 41	61	U	A
\ 42	62	U	B
\ 43	63	U	C
\ 44	64	U	D
\ 45	65	U	E
\ 46	66	U	F
\ 47	67	U	G
\ 48	70	U	H
\ 49	71	U	I
\ 4A	41	U	J
\ 4B	42	U	K
\ 4C	43	U	L
\ 4D	44	U	M
\ 4E	45	U	N
\ 4F	46	U	O
\ 50	47	U	P
\ 51	50	U	Q
\ 52	51	U	R
\ 53	22	U	S
\ 54	23	U	T
\ 55	24	U	U
\ 56	25	U	V
\ 57	26	U	W
\ 58	27	U	X
\ 59	30	U	Y
\ 5A	31	U	Z
\ 5B	57	U	[
\ 5C			\
\ 5D	55	U	]
\ 5E	06	L	^ / ∧ U+2227 <and>
\ 5F	40	U	_ <non-spacing underline>
\ 60			`
\ 61	61	L	a
\ 62	62	L	b
\ 63	63	L	c
\ 64	64	L	d
\ 65	65	L	e
\ 66	66	L	f
\ 67	67	L	g
\ 68	70	L	h
\ 69	71	L	i
\ 6A	41	L	j
\ 6B	42	L	k
\ 6C	43	L	l
\ 6D	44	L	m
\ 6E	45	L	n
\ 6F	46	L	o
\ 70	47	L	p
\ 71	50	L	q
\ 72	51	L	r
\ 73	22	L	s
\ 74	23	L	t
\ 75	24	L	u
\ 76	25	L	v
\ 77	26	L	w
\ 78	27	L	x
\ 79	30	L	y
\ 7A	31	L	z
\ 7B			{
\ 7C	56	U	| <non-spacing vertical bar>
\ 7D			}
\ 7E	03	U	~

\	0123456789ABCDEF0123456789ABCDEF
\	 !"#$%&'()*+,-./0123456789:;<=>?
\	@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_
\	`abcdefghijklmnopqrstuvwxyz{|}~

\	0123456701234567	0123456701234567
\	 123456789# ####	 "'~⊃∨∧<>↑# ####
\	0/stuvwxyz#,   #	→?STUVWXYZ#=   #
\	·jklmnopqr##-)-(	_JKLMNOPQR##+]|[
\	#abcdefghi .  # 	#ABCDEFGHI ×  # 

\	0123456701234567	0123456701234567
\	 123456789* ^}\{	 "'~⊃∨∧<>↑! #$%&
\	0/stuvwxyz:,    	→?STUVWXYZ;=    
\	·jklmnopqr  -)-(	_JKLMNOPQR  +]|[
\	`abcdefghi .    	@ABCDEFGHI ×    


\ 21	12	U	!	|+.
\ 23	14	U	#	|+=
\ 24	15	U	$	|+S
\ 25	16	U	%	/+8
\ 26	17	U	&
\ 2A	12	L	*	|+×
\ 3A	32	L	:	·+.
\ 3B	32	U	;	·+,
\ 40	70	U	@	·+C
\ 5C	16	L	\
\ 5E	14	L	^
\ 60	70	L	`
\ 7B	15	L	{
\ 7D	17	L	}

\ 12 14 15 16 17 32 37 52 53 60 76

: decode-match? ( x a -- f )   @+ rot and swap @ = ;
: decode-op ( x a -- x )       3 cells + @ execute ;
: decode-insn ( a -- )         2 cells + @ execute ;
: /decode                      4 cells ;

: decode ( x -- )
   decode-end decode-start do
      dup i decode-match? if i decode-op i decode-insn leave then
   /decode +loop ;

base !

: run-pdp1 ( start image -- )
   to image  to pc
   begin fetch decode again ;
