require targets/riscv/asm.fth

: h@ ( a -- u ) dup c@ swap 1+ c@ 8 lshift + ;
: w@ ( a -- u ) dup h@ swap 2 + h@ 16 lshift + ;
: fail? ( c a -- a' f ) 4 - tuck w@ <> ;
: .fail   cr ." FAIL: " source 5 - type cr ;
: ?fail   fail? if .fail abort then ;
: check   here begin depth 1- while ?fail repeat drop ;

.( Assembler test: )
code assembler-test
   hex

   nop,                     00000013 check
   ecall,                   00000073 check

   123 # x11 x10 addi,      12358513 check
   0 # x1 x2 slti,          0000A113 check
   -1 # x2 x1 sltui,        FFF13093 check
   1 # x10 x31 xori,        00154F93 check
   2 # x31 x1 ori,          002FE093 check
   -2 # x31 x1 andi,        FFEFF093 check

   x10 x11 x12 add,         00A58633 check
   x12 x11 x10 sub,         40C58533 check
   x12 x11 x10 sll,         00C59533 check
   x12 x11 x10 slt,         00C5A533 check
   x12 x11 x10 sltu,        00C5B533 check
   x12 x11 x10 xor,         00C5C533 check
   x12 x11 x10 srl,         00C5D533 check
   x12 x11 x10 sra,         40C5D533 check
   x12 x11 x10 or,          00C5E533 check
   x12 x11 x10 and,         00C5F533 check
   x12 x11 x10 mul,         02C58533 check
   x12 x11 x10 mulh,        02C59533 check
   x12 x11 x10 mulhsu,      02C5A533 check
   x12 x11 x10 mulhu,       02C5B533 check
   x12 x11 x10 div,         02C5C533 check
   x12 x11 x10 divu,        02C5D533 check
   x12 x11 x10 rem,         02C5E533 check
   x12 x11 x10 remu,        02C5F533 check

   001 # x2 x1 slli,        00111093 check
   00F # x1 x2 srli,        00F0D113 check
   01F # x3 x4 srai,        41F1D213 check

   7FF # x1 lui,            000000B7 check
   800 # x1 lui,            000010B7 check
   1000 # x10 lui,          00001537 check
   12345000 # x11 lui,      123455B7 check

   x2 ) x1 lb,              00010083 check
   0 x2 )# x1 lh,           00011083 check
   0FFF x2 )# x1 lw,        FFF12083 check
   x2 ) x1 lbu,             00014083 check
   0 x2 )# x1 lhu,          00015083 check
   x2 ) x1 sb,              00110023 check
   0 x2 )# x1 sh,           00111023 check
   0FFF x2 )# x1 sw,        FE112FA3 check

   here x10 jal,            0000056F check
   x11 ) x10 jalr,          00058567 check
   0FFF x12 )# x11 jalr,    FFF605E7 check

   create l \ label
   l x11 x10 beq,           00B50063 check
   l x11 x10 bne,           FEB51EE3 check
   l x11 x10 blt,           FEB54CE3 check
   l x11 x10 bltu,          FEB56AE3 check
   l x11 x10 bge,           FEB558E3 check
   l x11 x10 bgeu,          FEB576E3 check

   ahead, then,             00000263 check
   begin, again,            00000063 check
   x0 x0 =, if, then,       00001263 check
   x0 x0 <>, if, then,      00000263 check
   x0 x0 <, if, then,       00005263 check
   begin, x0 x0 =, until,   00001063 check
   begin, x0 x0 <>, until,  00000063 check
   begin, x0 x0 <, until,   00005063 check

   02A # x10 li,            02A00513 check
   x2 x3 not,               FFF14193 check
   x4 x5 mv,                00020293 check
   x6 x7 neg,               406003B3 check
   here 4 + j,              0040006F check
   x10 ) jr,                00050067 check
   ret,                     00008067 check

   decimal
end-code
.( PASS ) cr
