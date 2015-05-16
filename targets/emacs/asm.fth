\ Copyright 2015 Lars Brinkhoff

\ Assembler for Emacs bytecodes.

\ Adds to FORTH vocabulary: ASSEMBLER CODE ;CODE.
\ Creates ASSEMBLER vocabulary with: END-CODE and Emacs bytecodes.

require search.fth
require lib/base.fth
require lib/common.fth

vocabulary assembler
also assembler definitions

base @  octal

\ Assembler state.
variable opcode
variable operand   defer ?operand,

\ Set opcode.
: opcode!   2@ >r opcode ! ;
: opcode,   opcode @ c, ;

\ Set operand.
: 2c,   dup c, 10 rshift c, ;
: !op   is ?operand, operand ! ;
: !op0   0 ['] drop !op ;
: !op1   ['] c, !op ;
: !op2   ['] 2c, !op ;
: o3   opcode 7 !bits !op0 ;
:noname does> 7 and cells + perform ;
create select  ' o3 , ' o3 , ' o3 , ' o3 , ' o3 , ' o3 , ' !op1 , ' !op2 ,
execute
: !op#   dup select ;
: !op300   opcode 77 !bits  !op0 ;

: relative   here - 176 + ;

\ Define instruction formats.
: instruction,   opcode! opcode,  operand @ ?operand, ;
: mnemonic ( u a "name" -- ) create 2,  does> instruction, ;
: format:   create ] !csp  does> mnemonic ;

\ Instruction formats.
format: op0   !op0 ;
format: op#   !op# ;
format: op1   !op1 ;
format: opR   relative !op1 ;
format: op2   !op2 ;
format: op300   !op300 ;

\ Instruction mnemonics.
000 op0 illegal, \ Bstack_ref+0 is not implemented: use dup.
000 op# stack-ref,
010 op# varref,
020 op# varset,
030 op# varbind,
040 op# call,
050 op# unbind,
060 op0 pophandler,
061 op2 pushconditioncase,
062 op2 pushcatch,
070 op0 nth,
071 op0 symbolp,
072 op0 consp,
073 op0 stringp,
074 op0 listp,
075 op0 eq,
076 op0 memq,
077 op0 not,
100 op0 car,
101 op0 cdr,
102 op0 cons,
103 op0 list1,
104 op0 list2,
105 op0 list3,
106 op0 list4,
107 op0 length,
110 op0 aref,
111 op0 aset,
112 op0 symbol-value,
113 op0 symbol-function,
114 op0 set,
115 op0 fset,
116 op0 get,
117 op0 substring,
120 op0 concat2,
121 op0 concat3,
122 op0 concat4,
123 op0 sub1,
124 op0 add1,
125 op0 eqlsign,
126 op0 gtr,
127 op0 lss,
130 op0 leq,
131 op0 geq,
132 op0 diff,
133 op0 negate,
134 op0 plus,
135 op0 max,
136 op0 min,
137 op0 mult,
140 op0 point,
141 op0 save-current-buffer, \ Obsolete.
142 op0 goto-char,
143 op0 insert,
144 op0 point-max,
145 op0 point-min,
146 op0 char-after,
147 op0 following-char,
150 op0 preceding-char,
151 op0 current-column,
152 op0 indent-to,
153 op0 scan-buffer, \ Obsolete in v18.
154 op0 eolp,
155 op0 eobp,
156 op0 bolp,
157 op0 bobp,
160 op0 current-buffer,
161 op0 set-buffer,
162 op0 save-current-buffer-1,
163 op0 set-mark, \ Obsolete in v18
164 op0 interactive-p, \ Obsolete in 24.1.
165 op0 forward-char,
166 op0 forward-word,
167 op0 skip-chars-forward,
170 op0 skip-chars-backward,
171 op0 forward-line,
172 op0 char-syntax,
173 op0 buffer-substring,
174 op0 delete-region,
175 op0 narrow-to-region,
176 op0 widen,
177 op0 end-of-line,
201 op2 constant2,
202 op2 goto,
203 op2 gotoifnil,
204 op2 gotoifnonnil,
205 op2 gotoifnilelsepop,
206 op2 gotoifnonnilelsepop,
207 op0 return,
210 op0 discard,
211 op0 dup,
212 op0 save-excursion,
213 op0 save-window-excursion, \ Obsolete in 24.1.
214 op0 save-restriction,
215 op0 catch,
216 op0 unwind-protect,
217 op0 condition-case,
220 op0 temp-output-buffer-setup, \ Obsolete in 24.1.
221 op0 temp-output-buffer-show,  \ Obsolete in 24.1.
222 op0 unbind-all, \ Obsolete.
223 op0 set-marker,
224 op0 match-beginning,
225 op0 match-end,
226 op0 upcase,
227 op0 downcase,
230 op0 stringeqlsign,
231 op0 stringlss,
232 op0 equal,
233 op0 nthcdr,
234 op0 elt,
235 op0 member,
236 op0 assq,
237 op0 nreverse,
240 op0 setcar,
241 op0 setcdr,
242 op0 car-safe,
243 op0 cdr-safe,
244 op0 nconc,
245 op0 quo,
246 op0 rem,
247 op0 numberp,
250 op0 integerp,
252 opR Rgoto,
253 opR Rgotoifnil,
254 opR Rgotoifnonnil,
255 opR Rgotoifnilelsepop,
256 opR Rgotoifnonnilelsepop,
257 op1 listN,
260 op1 concatN,
261 op1 insertN,
262 op1 stack-set,
263 op2 stack-set2,
266 op1 discardN,
300 op300 constant,

: constant,   dup o# 77 > if constant2, else constant, then ;
: stack-set,   dup o# 377 > if stack-set2, else stack-set, then ;
: discard-preserving-tos,   o# 200 or discardN, ;

\ Runtime for ;CODE.  CODE! is defined elsewhere.
: (;code)   r> code! ;

\ Enter and exit assembler mode.
: start-code   also assembler ;
: end-code     align previous ;

only forth definitions  also assembler

\ Standard assembler entry points.
: code    create ( parse-name header, ?code, ) start-code  ;
: ;code   postpone (;code) reveal postpone [ ?csp start-code ; immediate

base !  previous

0 [if]
: return?   o# 207 = ;
: .byte   ." \" (.) ;
: dasm    8 base> begin c@+ dup .byte return? until drop ;
: bytecode   ." #[0 " [char] " emit dasm [char] " emit ."  [42] 2]" cr ;

code bar
   0 constant,
   dup,
   1 discardN,
   here 2 + Rgoto,
   return,
end-code

bar bytecode
[then]
