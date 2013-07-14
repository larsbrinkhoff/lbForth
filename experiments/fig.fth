0 [if]
SCR # 3
  0 **********************  fig-FORTH  MODEL  **********************
  1
  2                      Through the courtesy of
  3
  4                       FORTH INTEREST GROUP
  5                          P. O. BOX 1105
  6                      SAN CARLOS, CA. 94070
  7
  8
  9                            RELEASE 1
 10                      WITH COMPILER SECURITY
 11                              AND
 12                      VARIABLE LENGTH NAMES
 13
 14
 15        Further distribution must include the above notice.


SCR # 4
  0 (  ERROR MESSAGES  )
  1 EMPTY STACK
  2 DICTIONARY FULL
  3 HAS INCORRECT ADDRESS MODE
  4 ISN'T UNIQUE
  5
  6 DISC RANGE ?
  7 FULL STACK
  8 DISC ERROR !
  9
 10
 11
 12
 13
 14
 15 FORTH INTEREST GROUP                                 MAY 1, 1979


SCR # 5
  0 (  ERROR MESSAGES  )
  1 COMPILATION ONLY, USE IN DEFINITION
  2 EXECUTION ONLY
  3 CONDITIONALS NOT PAIRED
  4 DEFINITION NOT FINISHED
  5 IN PROTECTED DICTIONARY
  6 USE ONLY WHEN LOADING
  7 OFF CURRENT EDITING SCREEN
  8 DECLARE VOCABULARY
  9
 10
 11
 12
 13
 14
 15
[then]

FORTH INTEREST GROUP                                 MAY 1, 1979

SCR # 6
  0 (  INPUT-OUTPUT,  TIM                              WFR-780519 )
  1 CODE EMIT   XSAVE STX,  BOT 1+ LDA,  7F # AND,
  2             72C6 JSR,  XSAVE LDX,  POP JMP,
  3 CODE KEY   XSAVE STX,  BEGIN,  BEGIN,  8 # LDX,
  4       BEGIN,  6E02 LDA,  .A LSR,  CS END,  7320 JSR,
  5       BEGIN,  731D JSR,  0 X) CMP,  0 X) CMP,  0 X) CMP,
  6       0 X) CMP,  0 X) CMP,  6E02 LDA,  .A LSR,  PHP,  TYA,
  7       .A LSR,  PLP,  CS IF,  80 # ORA,  THEN,  TAY,  DEX,
  8       0= END,  731D JSR,  FF # EOR,  7F # AND,  0= NOT END,
  9       7F # CMP,  0= NOT END,  XSAVE LDX,  PUSH0A JMP,
 10 CODE CR  XSAVE STX,  728A JSR,  XSAVE LDX, NEXT JMP,
 11
 12 CODE ?TERMINAL   1 # LDA,  6E02 BIT,  0= NOT IF,
 13      BEGIN,  731D JSR,  6E02 BIT,  0= END,  INY,  THEN,
 14      TYA,  PUSH0A  JMP,
 15 DECIMAL       ;S


SCR # 7
  0 (  INPUT-OUTPUT,  APPLE                            WFR-780730 )
  1 CODE HOME   FC58 JSR,  NEXT JMP,
  2 CODE SCROLL   FC70 JSR,  NEXT JMP,
  3
  4 HERE  '  KEY  2  -  !    ( POINT KEY TO HERE )
  5    FD0C JSR,  7F # AND,  PUSH0A JMP,
  6 HERE  ' EMIT  2  -  !   (  POINT EMIT TO HERE  )
  7    BOT 1+ LDA,  80 # ORA,  FDED JSR,  POP JMP,
  8 HERE  '  CR  2  -  !      ( POINT CR TO HERE )
  9     FD8E JSR,  NEXT JMP,
 10 HERE  '  ?TERMINAL  2  -  !   ( POINT ?TERM TO HERE )
 11    C000 BIT,  0<
 12       IF,  BEGIN,  C010 BIT,  C000 BIT,  0< NOT END,  INY,
 13         THEN,  TYA,  PUSH0A JMP,
 14
 15 DECIMAL    ;S


SCR # 8
  0 (  INPUT-OUTPUT,  SYM-1                            WFR-781015 )
  1 HEX
  2 CODE KEY    8A58 JSR,  7F # AND,  PUSH0A JMP,
  3
  4 CODE EMIT   BOT 1+ LDA,    8A47 JSR,  POP JMP,
  5
  6 CODE CR    834D JSR,  NEXT JMP,
  7
  8 CODE ?TERMINAL  ( BREAK TEST FOR ANY KEY )
  9     8B3C JSR,  CS
 10     IF,  BEGIN,  8B3C JSR,  CS NOT  END,  INY,  THEN,
 11            TYA,  PUSH0A  JMP,
 12
 13
 14
 15 DECIMAL    ;S

FORTH INTEREST GROUP                                 MAY 1, 1979

SCR # 12
  0 (  COLD AND WARM ENTRY,  USER PARAMETERS          WFR-79APR29 )
  1 ASSEMBLER   OBJECT  MEM  HEX
  2 NOP,   HERE JMP,   ( WORD ALIGNED VECTOR TO COLD )
  3 NOP,   HERE JMP,   ( WORD ALIGNED VECTOR TO WARM )
  4 0000 ,   0001 ,  ( CPU, AND REVISION PARAMETERS )
  5 0000   ,        ( TOPMOST WORD IN FORTH VOCABULARY )
  6   7F   ,        ( BACKSPACE CHARACTER )
  7 3BA0   ,        ( INITIAL USER AREA )
  8 009E   ,        ( INITIAL TOP OF STACK )
  9 01FF   ,        ( INITIAL TOP OF RETURN STACK )
 10 0100   ,        ( TERMINAL INPUT BUFFER )
 11 001F   ,        ( INITIAL NAME FIELD WIDTH )
 12 0001   ,        ( INITIAL WARNING = 1 )
 13 0200   ,        ( INITIAL FENCE )
 14 0000   ,        ( COLD START VALUE FOR DP )
 15 0000   ,        ( COLD START VALUE FOR VOC-LINK )  -->


SCR # 13
  0 (  START OF NUCLEUS,  LIT, PUSH, PUT, NEXT        WFR-78DEC26 )
  1 CODE LIT                   ( PUSH FOLLOWING LITERAL TO STACK *)
  2      IP )Y LDA,  PHA,  IP INC,  0= IF,  IP 1+ INC,  THEN,
  3      IP )Y LDA,        IP INC,  0= IF,  IP 1+ INC,  THEN,
  4 LABEL PUSH      ( PUSH ACCUM AS HI-BYTE, ML STACK AS LO-BYTE *)
  5      DEX,  DEX,
  6 LABEL PUT          ( REPLACE BOTTOM WITH ACCUM. AND ML STACK *)
  7      BOT 1+ STA,  PLA,  BOT STA,
  8 LABEL NEXT           ( EXECUTE NEXT FORTH ADDRESS, MOVING IP *)
  9      1 # LDY,  IP )Y LDA,  W 1+ STA,     ( FETCH CODE ADDRESS )
 10          DEY,  IP )Y LDA,  W    STA,
 11      CLC,  IP LDA,  2 # ADC,  IP STA,         ( MOVE IP AHEAD )
 12      CS IF,  IP 1+ INC,  THEN,
 13      W 1 - JMP,   ( JUMP INDIR. VIA W THRU CODE FIELD TO CODE )
 14
 15 -->


SCR # 14
  0 (  SETUP                                           WFR-790225 )
  1                     HERE  2+  ,           ( MAKE SILENT WORD *)
  2      IP )Y LDA,  PHA,  TYA,  'T LIT 0B +  0=  NOT  END,
  3
  4 LABEL SETUP  ( MOVE # ITEMS FROM STACK TO 'N' AREA OF Z-PAGE *)
  5     .A ASL,  N 1 - STA,
  6     BEGIN,  BOT LDA,  N ,Y STA,  INX,  INY,
  7          N 1 - CPY,  0= END,  0 # LDY,  RTS,
  8
  9 CODE EXECUTE              ( EXECUTE A WORD BY ITS CODE FIELD *)
 10                                       ( ADDRESS ON THE STACK *)
 11    BOT LDA,  W STA,  BOT 1+ LDA,  W 1+ STA,
 12    INX,  INX,  W 1 - JMP,
 13
 14
 15 -->

FORTH INTEREST GROUP                                 MAY 1, 1979

SCR # 15
  0 (  BRANCH, 0BRANCH     W/16-BIT OFFSET            WFR-79APR01 )
  1 CODE BRANCH            ( ADJUST IP BY IN-LINE 16 BIT LITERAL *)
  2    CLC,  IP )Y LDA,  IP    ADC,        PHA,
  3    INY,  IP )Y LDA,  IP 1+ ADC,  IP 1+ STA,
  4                            PLA,  IP    STA,  NEXT 2+ JMP,
  5
  6 CODE 0BRANCH           ( IF BOT IS ZERO, BRANCH FROM LITERAL *)
  7    INX,  INX,  FE ,X LDA,  FF ,X ORA,
  8    '  BRANCH  0= NOT END,  ( USE 'BRANCH' FOR FALSE )
  9  LABEL BUMP:                    ( TRUE JUST MOVES IP 2 BYTES *)
 10    CLC,  IP LDA,  2 # ADC,  IP STA,
 11    CS IF,  IP 1+ INC, THEN,  NEXT JMP,
 12
 13 -->
 14
 15


SCR # 16
  0 (  LOOP CONTROL                                   WFR-79MAR20 )
  1 CODE (LOOP)      ( INCREMENT LOOP INDEX, LOOP UNTIL => LIMIT *)
  2     XSAVE STX,  TSX,  R INC,  0= IF,  R 1+ INC,  THEN,
  3   LABEL L1:  CLC,  R 2+ LDA,  R SBC,  R 3 + LDA,  R 1+ SBC,
  4   LABEL L2:    XSAVE LDX,      ( LIMIT-INDEX-1 )
  5     .A ASL, '  BRANCH  CS END, ( BRANCH UNTIL D7 SIGN=1 )
  6     PLA,  PLA,  PLA,  PLA,  BUMP: JMP,  ( ELSE EXIT LOOP )
  7
  8 CODE (+LOOP)          ( INCREMENT INDEX BY STACK VALUE +/-   *)
  9     INX,  INX,  XSAVE STX,  ( POP INCREMENT )
 10     FF ,X LDA,  PHA,  PHA,  FE ,X LDA,  TSX,  INX,  INX,
 11     CLC,  R ADC,  R STA,  PLA,  R 1 + ADC,  R 1 + STA,
 12     PLA,  L1:  0< END,     ( AS FOR POSITIVE INCREMENT )
 13     CLC,  R    LDA,  R 2+  SBC, ( INDEX-LIMIT-1 )
 14           R 1+ LDA,  R 3 + SBC,    L2: JMP,
 15 -->


SCR # 17
  0 (  (DO-                                           WFR-79MAR30 )
  1
  2 CODE (DO)             ( MOVE TWO STACK ITEMS TO RETURN STACK *)
  3    SEC 1+ LDA,  PHA,  SEC LDA,  PHA,
  4    BOT 1+ LDA,  PHA,  BOT LDA,  PHA,
  5
  6 LABEL POPTWO    INX,  INX,
  7 LABEL POP       INX,  INX,  NEXT JMP,
  8
  9 CODE I                    ( COPY CURRENT LOOP INDEX TO STACK *)
 10                           ( THIS WILL LATER BE POINTED TO 'R' )
 11
 12 -->
 13
 14
 15

FORTH INTEREST GROUP                                 MAY 1, 1979

SCR # 18
  0 (  DIGIT                                           WFR-781202 )
  1 CODE DIGIT     ( CONVERT ASCII CHAR-SECOND, WITH BASE-BOTTOM *)
  2                    ( IF OK RETURN DIGIT-SECOND, TRUE-BOTTOM; *)
  3                                    ( OTHERWISE FALSE-BOTTOM. *)
  4   SEC,  SEC LDA,   30 # SBC,
  5   0< NOT IF,  0A # CMP, ( ADJUST FOR ASCII LETTER )
  6             0< NOT IF,  SEC,  07 # SBC,  0A # CMP,
  7                        0< NOT IF,
  8  SWAP ( AT COMPILE TIME )  THEN,  BOT CMP, ( TO BASE )
  9                       0< IF,  SEC STA,  1 # LDA,
 10                       PHA,  TYA,  PUT JMP,
 11                       ( STORE RESULT SECOND AND RETURN TRUE )
 12    THEN,  THEN,  THEN,   ( CONVERSION FAILED )
 13    TYA,  PHA,  INX,  INX,  PUT JMP,  ( LEAVE BOOLEAN FALSE )
 14
 15 -->


SCR # 19
  0 (  FIND FOR VARIABLE LENGTH NAMES                  WFR-790225 )
  1 CODE (FIND)  ( HERE, NFA ... PFA, LEN BYTE, TRUE; ELSE FALSE *)
  2      2 # LDA,  SETUP JSR,  XSAVE STX,
  3 BEGIN,  0 # LDY,  N )Y LDA,  N 2+ )Y EOR,  3F # AND,  0=
  4    IF, ( GOOD ) BEGIN, INY, N )Y LDA,  N 2+ )Y EOR,  .A ASL,  0=
  5           IF, ( STILL GOOD ) SWAP  CS ( LOOP TILL D7 SET )
  6       END,    XSAVE LDX,  DEX,  DEX,  DEX,  DEX,  CLC,
  7               TYA,  5 # ADC,  N ADC,  SEC STA,  0 # LDY,
  8         TYA,   N 1+ ADC,  SEC 1+ STA,  BOT 1+ STY,
  9      N )Y LDA,  BOT STA,  1 # LDA,  PHA,  PUSH JMP, ( FALSE )
 10      THEN,  CS  NOT ( AT LAST CHAR? )  IF,  SWAP  THEN,
 11     BEGIN,  INY,  N )Y LDA,  0< END, ( TO LAST CHAR )
 12   THEN,  INY,  ( TO LINK )  N )Y LDA,  TAX,  INY,
 13          N )Y LDA,  N 1+ STA,  N STX,  N ORA, ( 0 LINK ? )
 14     0= END, ( LOOP FOR ANOTHER NAME )
 15     XSAVE LDX,  0 # LDA, PHA,  PUSH JMP, ( FALSE )   -->


SCR # 20
  0 (  ENCLOSE                                         WFR-780926 )
  1 CODE ENCLOSE   ( ENTER WITH ADDRESS-2, DELIM-1.  RETURN WITH *)
  2    ( ADDR-4, AND OFFSET TO FIRST CH-3, END WORD-2, NEXT CH-1 *)
  3   2 # LDA,  SETUP JSR,  TXA,  SEC,  8 # SBC,  TAX,
  4   SEC 1+ STY,  BOT 1+ STY,  ( CLEAR HI BYTES )   DEY,
  5   BEGIN,  INY,  N 2+ )Y LDA,  ( FETCH CHAR )
  6      N CMP,  0=  NOT END,  ( STEP OVER LEADING DELIMITERS )
  7   BOT 4 + STY,  ( SAVE OFFSET TO FIRST CHAR )
  8   BEGIN,  N 2+ )Y LDA,  0=
  9       IF, ( NULL )  SEC STY, ( IN EW )  BOT STY,  ( IN NC )
 10             TYA,  BOT 4 + CMP,  0=
 11           IF, ( Y=FC )  SEC INC, ( BUMP EW )  THEN,  NEXT JMP,
 12        THEN,  SEC STY,  ( IN EW )  INY,  N CMP, ( DELIM ? )
 13      0= END,  ( IS DELIM )  BOT STY, ( IN NC )  NEXT JMP,
 14
 15 -->

FORTH INTEREST GROUP                                 MAY 1, 1979

SCR # 21
  0 (  TERMINAL VECTORS                               WFR-79MAR30 )
  1 (  THESE WORDS ARE CREATED WITH NO EXECUTION CODE, YET.       )
  2 (  THEIR CODE FIELDS WILL BE FILLED WITH THE ADDRESS OF THEIR )
  3 (  INSTALLATION SPECIFIC CODE.                                )
  4
  5 CODE EMIT             ( PRINT ASCII VALUE ON BOTTOM OF STACK *)
  6
  7 CODE KEY        ( ACCEPT ONE TERMINAL CHARACTER TO THE STACK *)
  8
  9 CODE ?TERMINAL      ( 'BREAK' LEAVES 1 ON STACK; OTHERWISE 0 *)
 10
 11 CODE CR         ( EXECUTE CAR. RETURN, LINE FEED ON TERMINAL *)
 12
 13 -->
 14
 15


SCR # 22
  0 (  CMOVE,                                         WFR-79MAR20 )
  1 CODE CMOVE   ( WITHIN MEMORY; ENTER W/  FROM-3, TO-2, QUAN-1 *)
  2   3 # LDA,  SETUP JSR,        ( MOVE 3 ITEMS TO 'N' AREA )
  3   BEGIN,  BEGIN,  N CPY,  0=  ( DECREMENT BYTE COUNTER AT 'N' )
  4              IF,  N 1+ DEC,  0<         ( EXIT WHEN DONE )
  5                   IF,  NEXT JMP,  THEN,  THEN,
  6          N 4 + )Y LDA,  N 2+ )Y STA,  INY,  0=
  7       END,          ( LOOP TILL Y WRAPS, 22 CYCLES/BYTE  )
  8       N 5 + INC,  N 3 + INC,      ( BUMP HI BYTES OF POINTERS )
  9    JMP,  ( BACK TO FIRST 'BEGIN' )
 10
 11 -->
 12
 13
 14
 15


SCR # 23
  0 (  U*,  UNSIGNED MULTIPLY FOR 16 BITS          RS-WFR-80AUG16 )
  1 CODE U*        ( 16 BIT MULTIPLICAND-2,  16 BIT MULTIPLIER-1 *)
  2              ( 32 BIT UNSIGNED PRODUCT: LO WORD-2, HI WORD-1 *)
  3    SEC    LDA,  N    STA,  SEC    STY,
  4    SEC 1+ LDA,  N 1+ STA,  SEC 1+ STY,  ( multiplicand to n )
  5    10 # LDY,
  6    BEGIN,  BOT 2+ ASL,  BOT 3 + ROL,  BOT ROL,  BOT 1+ ROL,
  7             ( double product while sampling D15 of multiplier )
  8          CS IF, ( set ) CLC,
  9             ( add multiplicand to partial product 32 bits )
 10                N    LDA,  BOT 2 + ADC,  BOT 2 + STA,
 11                N 1+ LDA,  BOT 3 + ADC,  BOT 3 + STA,
 12              CS IF,  BOT INC,  0= IF,  BOT 1+ INC, ENDIF, ENDIF,
 13            ENDIF,  DEY,  0=    ( corrected for carry bug )
 14       UNTIL,      NEXT JMP,   C;
 15 -->

FORTH INTEREST GROUP                               Aug 23, 1980

SCR # 24
  0 (  U/,  UNSIGNED DIVIDE FOR 31 BITS               WFR-79APR29 )
  1 CODE  U/         ( 31 BIT DIVIDEND-2, -3,  16 BIT DIVISOR-1  *)
  2                  ( 16 BIT REMAINDER-2,  16 BIT QUOTIENT-1    *)
  3   SEC 2 + LDA,  SEC     LDY,  SEC 2 + STY,  .A ASL,  SEC    STA,
  4   SEC 3 + LDA,  SEC 1+  LDY,  SEC 3 + STY,  .A ROL,  SEC 1+ STA,
  5   10 # LDA,  N STA,
  6   BEGIN,  SEC 2 + ROL,  SEC 3 + ROL,  SEC,
  7           SEC 2 + LDA,  BOT     SBC,  TAY,
  8           SEC 3 + LDA,  BOT 1+  SBC,
  9           CS IF,  SEC 2+ STY,  SEC 3 + STA,  THEN,
 10           SEC ROL,  SEC 1+ ROL,
 11           N DEC,  0=
 12       END,    POP  JMP,
 13 -->
 14
 15


SCR # 25
  0 (  LOGICALS                                       WFR-79APR20 )
  1
  2 CODE AND           ( LOGICAL BITWISE AND OF BOTTOM TWO ITEMS *)
  3    BOT    LDA,  SEC    AND,  PHA,
  4    BOT 1+ LDA,  SEC 1+ AND,  INX,  INX,  PUT JMP,
  5
  6 CODE OR           ( LOGICAL BITWISE 'OR' OF BOTTOM TWO ITEMS *)
  7    BOT    LDA,  SEC     ORA,  PHA,
  8    BOT 1+ LDA,  SEC 1 + ORA,  INX,  INX,  PUT JMP,
  9
 10 CODE XOR        ( LOGICAL 'EXCLUSIVE-OR' OF BOTTOM TWO ITEMS *)
 11    BOT    LDA,  SEC    EOR,  PHA,
 12    BOT 1+ LDA,  SEC 1+ EOR,  INX,  INX,  PUT JMP,
 13
 14 -->
 15


SCR # 26
  0 (  STACK INITIALIZATION                           WFR-79MAR30 )
  1 CODE SP@                      ( FETCH STACK POINTER TO STACK *)
  2                TXA,
  3 LABEL PUSH0A   PHA,  0 # LDA,  PUSH JMP,
  4
  5 CODE SP!                                 ( LOAD SP FROM 'S0' *)
  6    06 # LDY,  UP )Y LDA,  TAX,  NEXT JMP,
  7
  8 CODE RP!                                   ( LOAD RP FROM R0 *)
  9    XSAVE STX,  08 # LDY,  UP )Y LDA,  TAX,  TXS,
 10               XSAVE LDX,  NEXT JMP,
 11
 12 CODE ;S              ( RESTORE IP REGISTER FROM RETURN STACK *)
 13    PLA,  IP STA,  PLA,  IP 1+ STA,  NEXT JMP,
 14
 15 -->

FORTH INTEREST GROUP                                 MAY 1, 1979

SCR # 27
  0 (  RETURN STACK WORDS                             WFR-79MAR29 )
  1 CODE LEAVE          ( FORCE EXIT OF DO-LOOP BY SETTING LIMIT *)
  2    XSAVE STX,  TSX,  R LDA,  R 2+ STA,            ( TO INDEX *)
  3    R 1+ LDA,  R 3 + STA,  XSAVE LDX,  NEXT JMP,
  4
  5 CODE >R              ( MOVE FROM COMP. STACK TO RETURN STACK *)
  6    BOT 1+ LDA,  PHA,  BOT LDA,  PHA,  INX,  INX,  NEXT JMP,
  7
  8 CODE R>              ( MOVE FROM RETURN STACK TO COMP. STACK *)
  9    DEX,  DEX,  PLA,  BOT STA,  PLA,  BOT 1+ STA,  NEXT JMP,
 10
 11 CODE R      ( COPY THE BOTTOM OF RETURN STACK TO COMP. STACK *)
 12    XSAVE STX,  TSX,  R LDA,  PHA,  R 1+ LDA,
 13    XSAVE LDX,  PUSH JMP,
 14 '   R    -2  BYTE.IN  I  !
 15 -->


SCR # 28
  0 (  TESTS AND LOGICALS                             WFR-79MAR19 )
  1
  2 CODE 0=           ( REVERSE LOGICAL STATE OF BOTTOM OF STACK *)
  3    BOT LDA,  BOT 1+ ORA,  BOT 1+ STY,
  4    0= IF,  INY,  THEN,  BOT STY,  NEXT JMP,
  5
  6 CODE 0<            ( LEAVE TRUE IF NEGATIVE; OTHERWISE FALSE *)
  7    BOT 1+ ASL,  TYA,  .A ROL,  BOT 1+ STY,  BOT STA,  NEXT JMP,
  8
  9
 10 -->
 11
 12
 13
 14
 15


SCR # 29
  0 (  MATH                                           WFR-79MAR19 )
  1 CODE +         ( LEAVE THE SUM OF THE BOTTOM TWO STACK ITEMS *)
  2    CLC,  BOT LDA,  SEC ADC,  SEC STA,  BOT 1+ LDA,  SEC 1+ ADC,
  3          SEC 1+ STA,  INX,  INX,  NEXT JMP,
  4 CODE D+            ( ADD TWO DOUBLE INTEGERS, LEAVING DOUBLE *)
  5    CLC,  BOT 2 + LDA,  BOT 6 + ADC,  BOT 6 + STA,
  6          BOT 3 + LDA,  BOT 7 + ADC,  BOT 7 + STA,
  7          BOT     LDA,  BOT 4 + ADC,  BOT 4 + STA,
  8          BOT 1 + LDA,  BOT 5 + ADC,  BOT 5 + STA,  POPTWO JMP,
  9 CODE MINUS         ( TWOS COMPLEMENT OF BOTTOM SINGLE NUMBER *)
 10    SEC,  TYA,  BOT    SBC,  BOT    STA,
 11          TYA,  BOT 1+ SBC,  BOT 1+ STA,  NEXT JMP,
 12 CODE DMINUS        ( TWOS COMPLEMENT OF BOTTOM DOUBLE NUMBER *)
 13    SEC,  TYA,  BOT 2 + SBC,  BOT 2 + STA,
 14          TYA,  BOT 3 + SBC,  BOT 3 + STA,
 15       1  BYTE.IN  MINUS  JMP,               -->

FORTH INTEREST GROUP                                 MAY 1, 1979

SCR # 30
  0 (  STACK MANIPULATION                             WFR-79MAR29 )
  1 CODE OVER              ( DUPLICATE SECOND ITEM AS NEW BOTTOM *)
  2    SEC LDA,  PHA,  SEC 1+ LDA,  PUSH JMP,
  3
  4 CODE DROP                           ( DROP BOTTOM STACK ITEM *)
  5    POP  -2  BYTE.IN  DROP  ! ( C.F. VECTORS DIRECTLY TO 'POP' )
  6
  7 CODE SWAP        ( EXCHANGE BOTTOM AND SECOND ITEMS ON STACK *)
  8    SEC LDA,  PHA,  BOT LDA,  SEC STA,
  9    SEC 1+ LDA,  BOT 1+ LDY,  SEC 1+ STY,  PUT JMP,
 10
 11 CODE DUP                    ( DUPLICATE BOTTOM ITEM ON STACK *)
 12    BOT LDA,  PHA,  BOT 1+ LDA,  PUSH JMP,
 13
 14 -->
 15


SCR # 31
  0 (  MEMORY INCREMENT,                              WFR-79MAR30 )
  1
  2 CODE +!   ( ADD SECOND TO MEMORY 16 BITS ADDRESSED BY BOTTOM *)
  3    CLC,  BOT X) LDA,  SEC ADC,  BOT X) STA,
  4    BOT INC,  0= IF,  BOT 1+ INC,  THEN,
  5    BOT X) LDA,  SEC 1+ ADC,  BOT X) STA,  POPTWO JMP,
  6
  7 CODE TOGGLE          ( BYTE AT ADDRESS-2, BIT PATTERN-1  ... *)
  8        SEC X) LDA,  BOT EOR,  SEC X) STA,  POPTWO JMP,
  9
 10 -->
 11
 12
 13
 14
 15


SCR # 32
  0 (  MEMORY FETCH AND STORE                          WFR-781202 )
  1 CODE @                   ( REPLACE STACK ADDRESS WITH 16 BIT *)
  2     BOT X) LDA,  PHA,             ( CONTENTS OF THAT ADDRESS *)
  3     BOT INC,  0= IF, BOT 1+ INC,  THEN,  BOT X) LDA,  PUT JMP,
  4
  5 CODE C@      ( REPLACE STACK ADDRESS WITH POINTED 8 BIT BYTE *)
  6     BOT X) LDA,  BOT STA,  BOT 1+ STY,  NEXT JMP,
  7
  8 CODE !         ( STORE SECOND AT 16 BITS ADDRESSED BY BOTTOM *)
  9     SEC LDA,  BOT X) STA,  BOT INC,  0= IF,  BOT 1+ INC,  THEN,
 10     SEC 1+ LDA,  BOT X) STA,  POPTWO JMP,
 11
 12 CODE C!           ( STORE SECOND AT BYTE ADDRESSED BY BOTTOM *)
 13     SEC LDA,  BOT X) STA,  POPTWO JMP,
 14
 15 DECIMAL     ;S

FORTH INTEREST GROUP                                 MAY 1, 1979

SCR # 33
  0 (  :,  ;,                                         WFR-79MAR30 )
  1
  2 : :                 ( CREATE NEW COLON-DEFINITION UNTIL ';' *)
  3                    ?EXEC !CSP CURRENT   @         CONTEXT    !
  4                CREATE  ]     ;CODE   IMMEDIATE
  5      IP 1+ LDA,  PHA,  IP LDA,  PHA,  CLC,  W LDA,  2 # ADC,
  6      IP STA,  TYA,  W 1+ ADC,  IP 1+ STA,  NEXT JMP,
  7
  8
  9 : ;                             ( TERMINATE COLON-DEFINITION *)
 10                     ?CSP  COMPILE     ;S
 11                   SMUDGE  [    ;   IMMEDIATE
 12
 13
 14
 15 -->


SCR # 34
  0 (  CONSTANT,  VARIABLE, USER                      WFR-79MAR30 )
  1 : CONSTANT              ( WORD WHICH LATER CREATES CONSTANTS *)
  2                       CREATE  SMUDGE  ,     ;CODE
  3        2 # LDY,  W )Y LDA,  PHA,  INY,  W )Y LDA,  PUSH JMP,
  4
  5 : VARIABLE              ( WORD WHICH LATER CREATES VARIABLES *)
  6      CONSTANT  ;CODE
  7        CLC,  W LDA,  2 # ADC,  PHA,  TYA,  W 1+ ADC,  PUSH JMP,
  8
  9
 10 : USER                                ( CREATE USER VARIABLE *)
 11      CONSTANT  ;CODE
 12        2 # LDY,  CLC,  W )Y LDA,  UP ADC,  PHA,
 13        0 # LDA,  UP 1+ ADC,  PUSH JMP,
 14
 15 -->


SCR # 35
  0 (  DEFINED CONSTANTS                              WFR-78MAR22 )
  1 HEX
  2 00  CONSTANT  0       01  CONSTANT  1
  3 02  CONSTANT  2       03  CONSTANT  3
  4 20  CONSTANT  BL                               ( ASCII BLANK *)
  5 40  CONSTANT  C/L                 ( TEXT CHARACTERS PER LINE *)
  6
  7 3BE0    CONSTANT   FIRST   ( FIRST BYTE RESERVED FOR BUFFERS *)
  8 4000    CONSTANT   LIMIT            ( JUST BEYOND TOP OF RAM *)
  9   80    CONSTANT   B/BUF            ( BYTES PER DISC BUFFER  *)
 10    8     CONSTANT  B/SCR  ( BLOCKS PER SCREEN = 1024 B/BUF / *)
 11
 12            00  +ORIGIN
 13 : +ORIGIN  LITERAL  +  ; ( LEAVES ADDRESS RELATIVE TO ORIGIN *)
 14 -->
 15

FORTH INTEREST GROUP                                 MAY 1, 1979

SCR # 36
  0 (  USER VARIABLES                                 WFR-78APR29 )
  1 HEX              ( 0 THRU 5 RESERVED,    REFERENCED TO $00A0 *)
  2 ( 06 USER  S0 )             ( TOP OF EMPTY COMPUTATION STACK *)
  3 ( 08 USER  R0 )                  ( TOP OF EMPTY RETURN STACK *)
  4 0A   USER  TIB                       ( TERMINAL INPUT BUFFER *)
  5 0C   USER  WIDTH                  ( MAXIMUM NAME FIELD WIDTH *)
  6 0E   USER  WARNING                   ( CONTROL WARNING MODES *)
  7 10   USER  FENCE                    ( BARRIER FOR FORGETTING *)
  8 12   USER  DP                           ( DICTIONARY POINTER *)
  9 14   USER  VOC-LINK                   ( TO NEWEST VOCABULARY *)
 10 16   USER  BLK                        ( INTERPRETATION BLOCK *)
 11 18   USER  IN                      ( OFFSET INTO SOURCE TEXT *)
 12 1A   USER  OUT                     ( DISPLAY CURSOR POSITION *)
 13 1C   USER  SCR                              ( EDITING SCREEN *)
 14 -->
 15


SCR # 37
  0 (  USER VARIABLES, CONT.                          WFR-79APR29 )
  1 1E   USER  OFFSET                 ( POSSIBLY TO OTHER DRIVES *)
  2 20   USER  CONTEXT               ( VOCABULARY FIRST SEARCHED *)
  3 22   USER  CURRENT          ( SEARCHED SECOND, COMPILED INTO *)
  4 24   USER  STATE                         ( COMPILATION STATE *)
  5 26   USER  BASE                   ( FOR NUMERIC INPUT-OUTPUT *)
  6 28   USER  DPL                      ( DECIMAL POINT LOCATION *)
  7 2A   USER  PLO                          ( OUTPUT FIELD WIDTH *)
  8 2C   USER  CSP                        ( CHECK STACK POSITION *)
  9 2E   USER  R#                     (  EDITING CURSOR POSITION *)
 10 30   USER  HLD        ( POINTS TO LAST CHARACTER HELD IN PAD *)
 11 -->
 12
 13
 14
 15


SCR # 38
  0 (  HI-LEVEL MISC.                                 WFR-79APR29 )
  1 : 1+      1   +  ;           ( INCREMENT STACK NUMBER BY ONE *)
  2 : 2+      2   +  ;           ( INCREMENT STACK NUMBER BY TWO *)
  3 : HERE    DP  @  ;        ( FETCH NEXT FREE ADDRESS IN DICT. *)
  4 : ALLOT   DP  +! ;                ( MOVE DICT. POINTER AHEAD *)
  5 : ,   HERE  !  2  ALLOT  ;     ( ENTER STACK NUMBER TO DICT. *)
  6 : C,   HERE  C!  1   ALLOT  ;    ( ENTER STACK BYTE TO DICT. *)
  7 : -   MINUS  +  ;                 ( LEAVE DIFF. SEC - BOTTOM *)
  8 : =   -  0=  ;                   ( LEAVE BOOLEAN OF EQUALITY *)
  9 : <   -  0<  ;                  ( LEAVE BOOLEAN OF SEC < BOT *)
 10 : >   SWAP  <  ;                ( LEAVE BOOLEAN OF SEC > BOT *)
 11 : ROT   >R  SWAP  R>  SWAP  ;       ( ROTATE THIRD TO BOTTOM *)
 12 : SPACE     BL  EMIT  ;            ( PRINT BLANK ON TERMINAL *)
 13 : -DUP     DUP  IF  DUP  ENDIF  ;       ( DUPLICATE NON-ZERO *)
 14 -->
 15

FORTH INTEREST GROUP                                 MAY 1, 1979

SCR # 39
  0 (  VARIABLE LENGTH NAME SUPPORT                   WFR-79MAR30 )
  1 : TRAVERSE                          ( MOVE ACROSS NAME FIELD *)
  2          ( ADDRESS-2, DIRECTION-1, I.E. -1=R TO L, +1=L TO R *)
  3        SWAP
  4        BEGIN  OVER  +  7F  OVER  C@  <  UNTIL  SWAP  DROP  ;
  5
  6 : LATEST       CURRENT  @  @  ;         ( NFA OF LATEST WORD *)
  7
  8
  9 ( FOLLOWING HAVE LITERALS DEPENDENT ON COMPUTER WORD SIZE )
 10
 11 : LFA    4  -  ;                ( CONVERT A WORDS PFA TO LFA *)
 12 : CFA    2  -  ;                ( CONVERT A WORDS PFA TO CFA *)
 13 : NFA    5  -  -1  TRAVERSE  ;  ( CONVERT A WORDS PFA TO NFA *)
 14 : PFA     1  TRAVERSE  5  +  ;  ( CONVERT A WORDS NFA TO PFA *)
 15     -->


SCR # 40
  0 (  ERROR PROCEEDURES, PER SHIRA                   WFR-79MAR23 )
  1 : !CSP     SP@  CSP  !  ;     ( SAVE STACK POSITION IN 'CSP' *)
  2
  3 : ?ERROR          ( BOOLEAN-2,  ERROR TYPE-1,  WARN FOR TRUE *)
  4          SWAP  IF         ERROR    ELSE  DROP  ENDIF  ;
  5
  6 : ?COMP   STATE @  0= 11 ?ERROR ;   ( ERROR IF NOT COMPILING *)
  7
  8 : ?EXEC   STATE  @  12  ?ERROR  ;   ( ERROR IF NOT EXECUTING *)
  9
 10 : ?PAIRS  -  13  ?ERROR  ;  ( VERIFY STACK VALUES ARE PAIRED *)
 11
 12 : ?CSP   SP@  CSP @ -  14  ?ERROR  ; ( VERIFY STACK POSITION *)
 13
 14 : ?LOADING                        ( VERIFY LOADING FROM DISC *)
 15          BLK  @  0=  16  ?ERROR  ;   -->


SCR # 41
  0 (  COMPILE,  SMUDGE,  HEX, DECIMAL                WFR-79APR20 )
  1
  2 : COMPILE          ( COMPILE THE EXECUTION ADDRESS FOLLOWING *)
  3         ?COMP  R>  DUP  2+  >R  @  ,  ;
  4
  5 : [    0  STATE  !  ;  IMMEDIATE          ( STOP COMPILATION *)
  6
  7 : ]    C0  STATE  !  ;             ( ENTER COMPILATION STATE *)
  8
  9 : SMUDGE    LATEST  20  TOGGLE  ;   ( ALTER LATEST WORD NAME *)
 10
 11 : HEX      10  BASE  !  ;         ( MAKE HEX THE IN-OUT BASE *)
 12
 13 : DECIMAL  0A  BASE  !  ;     ( MAKE DECIMAL THE IN-OUT BASE *)
 14 -->
 15

FORTH INTEREST GROUP                                 MAY 1, 1979

SCR # 42
  0 (  ;CODE                                          WFR-79APR20 )
  1
  2 : (;CODE)     ( WRITE CODE FIELD POINTING TO CALLING ADDRESS *)
  3         R>  LATEST  PFA  CFA  !  ;
  4
  5
  6 : ;CODE                      ( TERMINATE A NEW DEFINING WORD *)
  7       ?CSP  COMPILE  (;CODE)
  8       [COMPILE]  [  SMUDGE  ;   IMMEDIATE
  9 -->
 10
 11
 12
 13
 14
 15


SCR # 43
  0 (  <BUILD,  DOES>                                 WFR-79MAR20 )
  1
  2 : <BUILDS   0  CONSTANT  ;  ( CREATE HEADER FOR 'DOES>' WORD *)
  3
  4 : DOES>          ( REWRITE PFA WITH CALLING HI-LEVEL ADDRESS *)
  5                              ( REWRITE CFA WITH 'DOES>' CODE *)
  6              R>  LATEST  PFA  !  ;CODE
  7         IP 1+ LDA,  PHA,  IP LDA,  PHA,  ( BEGIN FORTH NESTING )
  8         2 # LDY,  W )Y LDA,  IP STA,       ( FETCH FIRST PARAM )
  9         INY,  W )Y LDA,  IP 1+ STA,      ( AS NEXT INTERP. PTR )
 10         CLC,  W LDA,  4 # ADC,  PHA,  ( PUSH ADDRESS OF PARAMS )
 11         W 1+ LDA,  00 # ADC,  PUSH JMP,
 12
 13 -->
 14
 15


SCR # 44
  0 (  TEXT OUTPUTS                                   WFR-79APR02 )
  1 : COUNT    DUP 1+ SWAP C@  ;  ( LEAVE TEXT ADDR. CHAR. COUNT *)
  2 : TYPE            ( TYPE STRING FROM ADDRESS-2, CHAR.COUNT-1 *)
  3         -DUP  IF OVER + SWAP
  4                  DO I C@ EMIT LOOP  ELSE DROP ENDIF ;
  5 : -TRAILING   ( ADJUST CHAR. COUNT TO DROP TRAILING BLANKS *)
  6         DUP  0  DO  OVER  OVER  +  1  -  C@
  7         BL  -  IF  LEAVE  ELSE  1  -  ENDIF  LOOP  ;
  8 : (.")             ( TYPE IN-LINE STRING, ADJUSTING RETURN *)
  9         R  COUNT  DUP  1+  R>  +  >R  TYPE  ;
 10
 11
 12 : ."     22  STATE  @      ( COMPILE OR PRINT QUOTED STRING *)
 13     IF  COMPILE  (.")       WORD    HERE  C@  1+  ALLOT
 14         ELSE        WORD   HERE   COUNT  TYPE  ENDIF  ;
 15                IMMEDIATE    -->

FORTH INTEREST GROUP                                 MAY 1, 1979

SCR # 45
  0 (  TERMINAL INPUT                                 WFR-79APR29 )
  1
  2 : EXPECT            ( TERMINAL INPUT MEMORY-2,  CHAR LIMIT-1 *)
  3     OVER  +  OVER  DO  KEY  DUP  0E  +ORIGIN ( BS )  @  =
  4     IF  DROP  08  OVER  I  =  DUP  R>  2  -  +  >R  -
  5        ELSE ( NOT BS )  DUP  0D  =
  6            IF ( RET ) LEAVE  DROP  BL  0  ELSE  DUP  ENDIF
  7            I  C!  0  I  1+  !
  8        ENDIF EMIT  LOOP  DROP  ;
  9 : QUERY     TIB  @  50  EXPECT  0  IN  !  ;
 10 8081  HERE
 11 : X   BLK @                            ( END-OF-TEXT IS NULL *)
 12       IF ( DISC ) 1 BLK +!  0 IN !  BLK @  7  AND  0=
 13          IF ( SCR END )  ?EXEC  R>  DROP  ENDIF ( disc dependent )
 14        ELSE  ( TERMINAL )    R>  DROP
 15          ENDIF  ;   !   IMMEDIATE     -->


SCR # 46
  0 (  FILL, ERASE, BLANKS, HOLD, PAD                 WFR-79APR02 )
  1 : FILL               ( FILL MEMORY BEGIN-3,  QUAN-2,  BYTE-1 *)
  2         SWAP  >R  OVER  C!  DUP  1+  R>  1  -  CMOVE  ;
  3
  4 : ERASE           ( FILL MEMORY WITH ZEROS  BEGIN-2,  QUAN-1 *)
  5         0  FILL  ;
  6
  7 : BLANKS                  ( FILL WITH BLANKS BEGIN-2, QUAN-1 *)
  8         BL  FILL  ;
  9
 10 : HOLD                               ( HOLD CHARACTER IN PAD *)
 11         -1  HLD  +!   HLD  @  C!  ;
 12
 13 : PAD        HERE  44  +  ;     ( PAD IS 68 BYTES ABOVE HERE *)
 14         ( DOWNWARD HAS NUMERIC OUTPUTS; UPWARD MAY HOLD TEXT *)
 15 -->


SCR # 47
  0 (  WORD,                                          WFR-79APR02 )
  1 : WORD         ( ENTER WITH DELIMITER, MOVE STRING TO 'HERE' *)
  2    BLK  @  IF  BLK  @        BLOCK    ELSE  TIB  @  ENDIF
  3    IN  @  +  SWAP    ( ADDRESS-2, DELIMITER-1 )
  4    ENCLOSE         ( ADDRESS-4, START-3, END-2, TOTAL COUNT-1 )
  5    HERE  22  BLANKS      ( PREPARE FIELD OF 34 BLANKS )
  6    IN  +!          ( STEP OVER THIS STRING )
  7    OVER  -  >R     ( SAVE CHAR COUNT )
  8    R  HERE  C!     ( LENGTH STORED FIRST )
  9    +  HERE  1+
 10    R>  CMOVE  ;    ( MOVE STRING FROM BUFFER TO HERE+1 )
 11
 12
 13
 14
 15 -->

FORTH INTEREST GROUP                                 MAY 1, 1979

SCR # 48
  0 (  (NUMBER-,  NUMBER,  -FIND,                     WFR-79APR29 )
  1 : (NUMBER)    ( CONVERT DOUBLE NUMBER, LEAVING UNCONV. ADDR. *)
  2    BEGIN  1+  DUP  >R  C@  BASE  @  DIGIT
  3       WHILE  SWAP  BASE  @  U*  DROP  ROT  BASE  @  U*  D+
  4       DPL  @  1+  IF  1  DPL  +!  ENDIF  R>  REPEAT  R>  ;
  5
  6 : NUMBER   ( ENTER W/ STRING ADDR.  LEAVE DOUBLE NUMBER *)
  7       0  0  ROT  DUP  1+  C@  2D  =  DUP  >R  +  -1
  8    BEGIN  DPL  !  (NUMBER)  DUP  C@  BL  -
  9       WHILE  DUP  C@  2E  -  0  ?ERROR    0  REPEAT
 10       DROP  R>  IF  DMINUS  ENDIF  ;
 11
 12 : -FIND       ( RETURN PFA-3, LEN BYTE-2, TRUE-1; ELSE FALSE *)
 13       BL  WORD         HERE  CONTEXT  @  @  (FIND)
 14       DUP  0=  IF  DROP  HERE  LATEST  (FIND)  ENDIF  ;
 15 -->


SCR # 49
  0 (  ERROR HANDLER                                  WFR-79APR20 )
  1
  2 : (ABORT)           ABORT    ;  ( USER ALTERABLE ERROR ABORT *)
  3
  4 : ERROR              ( WARNING:  -1=ABORT, 0=NO DISC, 1=DISC *)
  5     WARNING  @  0<           ( PRINT TEXT LINE REL TO SCR #4 *)
  6     IF  (ABORT)  ENDIF  HERE  COUNT  TYPE  ."   ? "
  7           MESSAGE    SP!  IN  @  BLK  @        QUIT    ;
  8
  9 : ID.   ( PRINT NAME FIELD FROM ITS HEADER ADDRESS *)
 10      PAD  020  5F  FILL  DUP  PFA  LFA  OVER  -
 11      PAD  SWAP  CMOVE  PAD  COUNT  01F  AND  TYPE  SPACE  ;
 12 -->
 13
 14
 15


SCR # 50
  0 (  CREATE                                         WFR-79APR28 )
  1
  2 : CREATE              ( A SMUDGED CODE HEADER TO PARAM FIELD *)
  3                      ( WARNING IF DUPLICATING A CURRENT NAME *)
  4       TIB  HERE  0A0  +  <  2  ?ERROR  ( 6502 only )
  5       -FIND    ( CHECK IF UNIQUE IN CURRENT AND CONTEXT )
  6       IF ( WARN USER )  DROP  NFA  ID.
  7                         4         MESSAGE    SPACE  ENDIF
  8       HERE  DUP  C@  WIDTH  @        MIN    1+  ALLOT
  9       DP  C@  0FD  =  ALLOT   ( 6502 only )
 10       DUP  A0  TOGGLE  HERE  1  -  80  TOGGLE ( DELIMIT BITS )
 11       LATEST  ,  CURRENT  @  !
 12       HERE  2+  ,  ;
 13 -->
 14
 15

FORTH INTEREST GROUP                                 MAY 1, 1979

SCR # 51
  0 (  LITERAL,  DLITERAL,  [COMPILE],  ?STACK        WFR-79APR29 )
  1
  2 : [COMPILE]         ( FORCE COMPILATION OF AN IMMEDIATE WORD *)
  3       -FIND  0=  0  ?ERROR  DROP  CFA  ,  ;  IMMEDIATE
  4
  5 : LITERAL                     ( IF COMPILING, CREATE LITERAL *)
  6       STATE  @  IF  COMPILE  LIT  ,  ENDIF  ;  IMMEDIATE
  7
  8 : DLITERAL             ( IF COMPILING, CREATE DOUBLE LITERAL *)
  9       STATE  @  IF  SWAP  [COMPILE]  LITERAL
 10                           [COMPILE]  LITERAL  ENDIF ; IMMEDIATE
 11
 12 (  FOLLOWING DEFINITION IS INSTALLATION DEPENDENT )
 13 : ?STACK    ( QUESTION UPON OVER OR UNDERFLOW OF STACK *)
 14       09E  SP@  <  1  ?ERROR   SP@  020  <  7  ?ERROR  ;
 15 -->


SCR # 52
  0 (  INTERPRET,                                     WFR-79APR18 )
  1
  2 : INTERPRET   ( INTERPRET OR COMPILE SOURCE TEXT INPUT WORDS *)
  3       BEGIN  -FIND
  4          IF  ( FOUND )  STATE  @  <
  5                 IF  CFA  ,  ELSE  CFA  EXECUTE  ENDIF  ?STACK
  6             ELSE  HERE  NUMBER  DPL  @  1+
  7                 IF  [COMPILE]  DLITERAL
  8                   ELSE   DROP  [COMPILE]  LITERAL  ENDIF  ?STACK
  9           ENDIF  AGAIN  ;
 10 -->
 11
 12
 13
 14
 15


SCR # 53
  0 (  IMMEDIATE,  VOCAB,  DEFIN,  FORTH,  (      DJK-WFR-79APR29 )
  1 : IMMEDIATE        ( TOGGLE PREC. BIT OF LATEST CURRENT WORD *)
  2          LATEST  40  TOGGLE  ;
  3
  4 : VOCABULARY  ( CREATE VOCAB WITH 'V-HEAD' AT VOC INTERSECT. *)
  5        <BUILDS  A081  ,  CURRENT  @  CFA  ,
  6        HERE  VOC-LINK  @  ,  VOC-LINK  !
  7        DOES>  2+  CONTEXT  !  ;
  8
  9 VOCABULARY  FORTH     IMMEDIATE       ( THE TRUNK VOCABULARY *)
 10
 11 : DEFINITIONS        ( SET THE CONTEXT ALSO AS CURRENT VOCAB *)
 12        CONTEXT  @  CURRENT  !  ;
 13
 14 : (              ( SKIP INPUT TEXT UNTIL RIGHT PARENTHESIS *)
 15        29  WORD  ;   IMMEDIATE   -->

FORTH INTEREST GROUP                                 MAY 1, 1979

SCR # 54
  0 (  QUIT,  ABORT                                  WFR-79MAR30 )
  1
  2 : QUIT                   ( RESTART,  INTERPRET FROM TERMINAL *)
  3       0  BLK  !  [COMPILE]  [
  4       BEGIN  RP!  CR  QUERY  INTERPRET
  5              STATE  @  0=  IF  ."  OK"  ENDIF  AGAIN  ;
  6
  7 : ABORT                  ( WARM RESTART, INCLUDING REGISTERS *)
  8       SP!  DECIMAL            DR0
  9       CR  ." FORTH-65 V 4.0"
 10       [COMPILE]  FORTH  DEFINITIONS  QUIT  ;
 11
 12
 13 -->
 14
 15


SCR # 55
  0 (  COLD START                                     WFR-79APR29 )
  1 CODE COLD               ( COLD START, INITIALIZING USER AREA *)
  2    HERE  02  +ORIGIN  !  ( POINT COLD ENTRY TO HERE )
  3          0C +ORIGIN LDA,  'T FORTH 4 + STA,  ( FORTH VOCAB. )
  4          0D +ORIGIN LDA,  'T FORTH 5 + STA,
  5          15 # LDY, ( INDEX TO VOC-LINK ) 0= IF, ( FORCED )
  6     HERE  06 +ORIGIN !   ( POINT RE-ENTRY TO HERE )
  7       0F # LDY,  ( INDEX TO WARNING )   THEN, ( FROM IF, )
  8       10 +ORIGIN LDA,   UP    STA,  ( LOAD UP )
  9       11 +ORIGIN LDA,   UP 1+ STA,
 10        BEGIN,  0C +ORIGIN ,Y LDA,  ( FROM LITERAL AREA )
 11                        UP )Y STA,  ( TO USER AREA )
 12             DEY,  0< END,
 13      'T ABORT  100  /MOD  # LDA,  IP 1+ STA,
 14                           # LDA,  IP    STA,
 15       6C # LDA,  W 1 - STA,    'T RP! JMP, ( RUN )  -->


SCR # 56
  0 (  MATH UTILITY                               DJK-WFR-79APR29 )
  1 CODE S->D                  ( EXTEND SINGLE INTEGER TO DOUBLE *)
  2       BOT 1+ LDA,  0< IF, DEY, THEN,  TYA, PHA, PUSH JMP,
  3
  4 : +-    0< IF MINUS ENDIF ;   ( APPLY SIGN TO NUMBER BENEATH *)
  5
  6 : D+-                  ( APPLY SIGN TO DOUBLE NUMBER BENEATH *)
  7         0<  IF  DMINUS  ENDIF  ;
  8
  9 : ABS     DUP  +-   ;                 ( LEAVE ABSOLUTE VALUE *)
 10 : DABS    DUP  D+-  ;        ( DOUBLE INTEGER ABSOLUTE VALUE *)
 11
 12 : MIN                         ( LEAVE SMALLER OF TWO NUMBERS *)
 13         OVER  OVER  >  IF  SWAP  ENDIF  DROP  ;
 14 : MAX                          ( LEAVE LARGER OF TWO NUMBERS *)
 15         OVER  OVER  <  IF  SWAP  ENDIF  DROP  ; -->

FORTH INTEREST GROUP                                 MAY 1, 1979

SCR # 57
  0 (  MATH PACKAGE                               DJK-WFR-79APR29 )
  1 : M*     ( LEAVE SIGNED DOUBLE PRODUCT OF TWO SINGLE NUMBERS *)
  2         OVER  OVER  XOR  >R  ABS  SWAP  ABS  U*  R>  D+-  ;
  3 : M/              ( FROM SIGNED DOUBLE-3-2, SIGNED DIVISOR-1 *)
  4                ( LEAVE SIGNED REMAINDER-2, SIGNED QUOTIENT-1 *)
  5         OVER  >R  >R  DABS  R  ABS  U/
  6         R>  R  XOR  +-  SWAP  R>  +-  SWAP  ;
  7 : *      U*  DROP  ;                        ( SIGNED PRODUCT *)
  8 : /MOD   >R  S->D  R>  M/  ;           ( LEAVE REM-2, QUOT-1 *)
  9 : /      /MOD  SWAP  DROP  ;                ( LEAVE QUOTIENT *)
 10 : MOD    /MOD  DROP  ;                     ( LEAVE REMAINDER *)
 11 : */MOD              ( TAKE RATION OF THREE NUMBERS, LEAVING *)
 12          >R  M*  R>  M/  ;               ( REM-2, QUOTIENT-1 *)
 13 : */     */MOD  SWAP  DROP  ;   ( LEAVE RATIO OF THREE NUMBS *)
 14 : M/MOD   ( DOUBLE, SINGLE DIVISOR ...  REMAINDER, DOUBLE *)
 15           >R  0  R  U/  R>  SWAP  >R  U/  R>   ;   -->


SCR # 58
  0 (  DISC UTILITY,  GENERAL USE                     WFR-79APR02 )
  1 FIRST  VARIABLE  USE           ( NEXT BUFFER TO USE, STALEST *)
  2 FIRST  VARIABLE  PREV      ( MOST RECENTLY REFERENCED BUFFER *)
  3
  4 : +BUF     ( ADVANCE ADDRESS-1 TO NEXT BUFFER. RETURNS FALSE *)
  5       84 ( I.E. B/BUF+4 )  +  DUP  LIMIT  =     ( IF AT PREV *)
  6       IF  DROP  FIRST  ENDIF  DUP  PREV  @  -  ;
  7
  8 : UPDATE     ( MARK THE BUFFER POINTED TO BY PREV AS ALTERED *)
  9       PREV  @  @  8000  OR  PREV  @  !  ;
 10
 11 : EMPTY-BUFFERS   ( CLEAR BLOCK BUFFERS; DON'T WRITE TO DISC *)
 12       FIRST  LIMIT  OVER  -  ERASE  ;
 13
 14 : DR0      0  OFFSET  !  ;                 ( SELECT DRIVE #0 *)
 15 : DR1   07D0  OFFSET  !  ;   -->           ( SELECT DRIVE #1 *)


SCR # 59
  0 (  BUFFER                                         WFR-79APR02 )
  1 : BUFFER                 ( CONVERT BLOCK# TO STORAGE ADDRESS *)
  2     USE  @  DUP  >R   ( BUFFER ADDRESS TO BE ASSIGNED )
  3     BEGIN  +BUF  UNTIL ( AVOID PREV )  USE  !  ( FOR NEXT TIME )
  4     R  @  0<  ( TEST FOR UPDATE IN THIS BUFFER )
  5     IF ( UPDATED, FLUSH TO DISC )
  6        R  2+ ( STORAGE LOC. )
  7        R  @  7FFF  AND  ( ITS BLOCK # )
  8        0         R/W     ( WRITE SECTOR TO DISC )
  9       ENDIF
 10     R  !  ( WRITE NEW BLOCK # INTO THIS BUFFER )
 11     R  PREV  !  ( ASSIGN THIS BUFFER AS 'PREV' )
 12     R>  2+  ( MOVE TO STORAGE LOCATION )  ;
 13
 14 -->
 15

FORTH INTEREST GROUP                                 MAY 1, 1979

SCR # 60
  0 (  BLOCK                                          WFR-79APR02 )
  1 : BLOCK         ( CONVERT BLOCK NUMBER TO ITS BUFFER ADDRESS *)
  2    OFFSET  @  +  >R   ( RETAIN BLOCK # ON RETURN STACK )
  3    PREV  @  DUP  @  R  -  DUP  +  ( BLOCK = PREV ? )
  4    IF ( NOT PREV )
  5       BEGIN  +BUF  0=  ( TRUE UPON REACHING 'PREV' )
  6          IF ( WRAPPED )  DROP  R  BUFFER
  7              DUP  R  1         R/W    ( READ SECTOR FROM DISC )
  8              2  - ( BACKUP )
  9            ENDIF
 10            DUP  @  R  -  DUP  +  0=
 11         UNTIL  ( WITH BUFFER ADDRESS )
 12       DUP  PREV  !
 13      ENDIF
 14      R>  DROP    2+  ;
 15 -->


SCR # 61
  0 (  TEXT OUTPUT FORMATTING                         WFR-79MAY03 )
  1
  2 : (LINE)        ( LINE#, SCR#, ...  BUFFER ADDRESS, 64 COUNT *)
  3          >R  C/L  B/BUF  */MOD  R>  B/SCR  *  +
  4          BLOCK  +  C/L  ;
  5
  6 : .LINE   ( LINE#,  SCR#,  ...  PRINTED *)
  7          (LINE)  -TRAILING  TYPE  ;
  8
  9 : MESSAGE      ( PRINT LINE RELATIVE TO SCREEN #4 OF DRIVE 0 *)
 10     WARNING  @
 11     IF  ( DISC IS AVAILABLE )
 12         -DUP  IF  4  OFFSET  @  B/SCR  /  -  .LINE  ENDIF
 13         ELSE  ." MSG # "          .    ENDIF  ;
 14 -->
 15


SCR # 62
  0 (  LOAD,  -->                                     WFR-79APR02 )
  1
  2 : LOAD                         ( INTERPRET SCREENS FROM DISC *)
  3     BLK  @  >R  IN  @  >R  0  IN  !  B/SCR  *  BLK !
  4     INTERPRET  R>  IN  !  R>  BLK  !  ;
  5
  6 : -->               ( CONTINUE INTERPRETATION ON NEXT SCREEN *)
  7      ?LOADING  0  IN  !  B/SCR  BLK  @  OVER
  8      MOD  -  BLK  +!  ;    IMMEDIATE
  9
 10 -->
 11
 12
 13
 14
 15

FORTH INTEREST GROUP                                 MAY 1, 1979

SCR # 63
  0 (  INSTALLATION DEPENDENT TERMINAL I-O,  TIM      WFR-79APR26 )
  1 ( EMIT )        ASSEMBLER
  2   HERE  -2  BYTE.IN  EMIT  !       ( VECTOR EMITS' CF TO HERE )
  3   XSAVE STX,  BOT LDA,  7F # AND, 72C6 JSR,  XSAVE LDX,
  4   CLC,  1A # LDY,  UP )Y LDA,  01 # ADC,  UP )Y STA,
  5              INY,  UP )Y LDA,  00 # ADC,  UP )Y STA,  POP JMP,
  6                                       ( AND INCREMENT 'OUT' )
  7 ( KEY )
  8       HERE  -2  BYTE.IN  KEY  !   ( VECTOR KEYS' CF TO HERE )
  9       XSAVE STX,  BEGIN,  8 # LDX,
 10       BEGIN,  6E02 LDA,  .A LSR,  CS END,  7320 JSR,
 11       BEGIN,  731D JSR,  0 X) CMP,  0 X) CMP,  0 X) CMP,
 12       0 X) CMP,  0 X) CMP,  6E02 LDA,  .A LSR,  PHP,  TYA,
 13       .A LSR,  PLP,  CS IF,  80 # ORA,  THEN,  TAY,  DEX,
 14       0= END,  731D JSR,  FF # EOR,  7F # AND,  0= NOT END,
 15       XSAVE LDX,   PUSH0A JMP,     -->


SCR # 64
  0 (  INSTALLATION DEPENDENT TERMINAL I-O,  TIM      WFR-79APR02 )
  1
  2 ( ?TERMINAL )
  3      HERE  -2  BYTE.IN  ?TERMINAL  !    ( VECTOR LIKEWISE )
  4      1 # LDA, 6E02 BIT, 0= NOT  IF,
  5      BEGIN,  731D JSR,  6E02 BIT,  0= END,  INY,  THEN,
  6       TYA,    PUSH0A JMP,
  7
  8 ( CR )
  9     HERE  -2  BYTE.IN  CR  !  ( VECTOR CRS' CF TO HERE )
 10     XSAVE STX,  728A JSR,  XSAVE LDX, NEXT JMP,
 11
 12 -->
 13
 14
 15


SCR # 65
  0 (  INSTALLATION DEPENDENT DISC                    WFR-79APR02 )
  1 6900    CONSTANT  DATA                     ( CONTROLLER PORT *)
  2 6901    CONSTANT  STATUS                   ( CONTROLLER PORT *)
  3
  4
  5 : #HL            ( CONVERT DECIMAL DIGIT FOR DISC CONTROLLER *)
  6       0  0A  U/  SWAP  30  +  HOLD  ;
  7
  8 -->
  9
 10
 11
 12
 13
 14
 15

FORTH INTEREST GROUP                                 MAY 1, 1979

SCR # 66
  0 (  D/CHAR,  ?DISC,                                WFR-79MAR23 )
  1 CODE D/CHAR    ( TEST CHAR-1. EXIT TEST BOOL-2, NEW CHAR-1 *)
  2       DEX,  DEX,  BOT 1+ STY,  C0 # LDA,
  3    BEGIN,  STATUS BIT,  0= NOT END,  ( TILL CONTROL READY )
  4       DATA LDA,  BOT STA,  ( SAVE CHAR )
  5       SEC CMP,  0= IF,  INY,  THEN,  SEC STY,  NEXT JMP,
  6
  7 : ?DISC         ( UPON NAK SHOW ERR MSG, QUIT.  ABSORBS TILL *)
  8       1  D/CHAR  >R  0=                ( EOT, EXCEPT FOR SOH *)
  9     IF  ( NOT SOH )  R  15 =
 10          IF ( NAK )  CR
 11              BEGIN  4  D/CHAR  EMIT
 12                 UNTIL ( PRINT ERR MSG TIL EOT )  QUIT
 13            ENDIF  ( FOR ENQ, ACK )
 14           BEGIN  4  D/CHAR  DROP  UNTIL  ( AT EOT )
 15      ENDIF  R>  DROP  ;   -->


SCR # 67
  0 (  BLOCK-WRITE                                     WFR-790103 )
  1 CODE BLOCK-WRITE     ( SEND TO DISC FROM ADDRESS-2,  COUNT-1 *)
  2       2 # LDA,  SETUP JSR,                 ( WITH EOT AT END *)
  3    BEGIN,  02 # LDA,
  4       BEGIN,    STATUS BIT,  0= END,  ( TILL IDLE )
  5       N CPY, 0=
  6         IF, ( DONE )  04 # LDA,  STATUS STA,  DATA STA,
  7              NEXT JMP,
  8         THEN,
  9       N 2+ )Y LDA,  DATA STA,  INY,
 10       0=  END,   ( FORCED TO BEGIN )
 11
 12 -->
 13
 14
 15


SCR # 68
  0 (  BLOCK-READ,                                     WFR-790103 )
  1
  2 CODE BLOCK-READ   ( BUF.ADDR-1. EXIT AT 128 CHAR OR CONTROL *)
  3     1 # LDA,  SETUP JSR,
  4    BEGIN,  C0 # LDA,
  5       BEGIN,   STATUS BIT,  0=  NOT END,  ( TILL FLAG )
  6       50 ( BVC, D6=DATA )
  7      IF,  DATA LDA,  N )Y STA,  INY,      SWAP
  8           0< END,  ( LOOP TILL 128 BYTES )
  9        THEN,   ( OR D6=0, SO D7=1, )
 10        NEXT JMP,
 11
 12 -->
 13
 14
 15

FORTH INTEREST GROUP                                 MAY 1, 1979

SCR # 69
  0 (  R/W   FOR PERSCI 1070 CONTROLLER               WFR-79MAY03 )
  1 0A  ALLOT  HERE      ( WORKSPACE TO PREPARE DISC CONTROL TEXT )
  2         ( IN FORM:  C TT SS /D,  TT=TRACK, SS=SECTOR, D=DRIVE )
  3                                  ( C = 1 TO READ, 0 TO WRITE *)
  4 : R/W                                ( READ/WRITE DISC BLOCK *)
  5                ( BUFFER ADDRESS-3, BLOCK #-2, 1=READ 0=WRITE *)
  6    LITERAL  HLD  ! ( JUST AFTER WORKSPACE )   SWAP
  7    0  OVER  >  OVER  0F9F  >  OR  6  ?ERROR
  8    07D0  ( 2000 SECT/DR )  /MOD  #HL  DROP  2F  HOLD  BL  HOLD
  9    1A  /MOD  SWAP 1+ #HL  #HL  DROP  BL  HOLD  ( SECTOR 01-26 )
 10                      #HL  #HL  DROP  BL  HOLD  ( TRACK  00-76 )
 11    DUP
 12    IF  49 ( 1=READ)  ELSE 4F ( 0=WRITE )  ENDIF
 13    HOLD  HLD  @  0A  BLOCK-WRITE  ( SEND TEXT ) ?DISC
 14    IF  BLOCK-READ  ELSE  B/BUF  BLOCK-WRITE  ENDIF
 15    ?DISC  ;     -->


SCR # 70
  0 (  FORWARD REFERENCES                             WFR-79MAR30 )
  1 00  BYTE.IN  :         REPLACED.BY  ?EXEC
  2 02  BYTE.IN  :         REPLACED.BY  !CSP
  3 04  BYTE.IN  :         REPLACED.BY  CURRENT
  4 08  BYTE.IN  :         REPLACED.BY  CONTEXT
  5 0C  BYTE.IN  :         REPLACED.BY  CREATE
  6 0E  BYTE.IN  :         REPLACED.BY  ]
  7 10  BYTE.IN  :         REPLACED.BY  (;CODE)
  8 00  BYTE.IN  ;         REPLACED.BY  ?CSP
  9 02  BYTE.IN  ;         REPLACED.BY  COMPILE
 10 06  BYTE.IN  ;         REPLACED.BY  SMUDGE
 11 08  BYTE.IN  ;         REPLACED.BY  [
 12 00  BYTE.IN  CONSTANT  REPLACED.BY  CREATE
 13 02  BYTE.IN  CONSTANT  REPLACED.BY  SMUDGE
 14 04  BYTE.IN  CONSTANT  REPLACED.BY  ,
 15 06  BYTE.IN  CONSTANT  REPLACED.BY  (;CODE)    -->


SCR # 71
  0 (  FORWARD REFERENCES                             WFR-79APR29 )
  1 02  BYTE.IN  VARIABLE    REPLACED.BY  (;CODE)
  2 02  BYTE.IN  USER        REPLACED.BY  (;CODE)
  3 06  BYTE.IN  ?ERROR      REPLACED.BY  ERROR
  4 0F  BYTE.IN  ."          REPLACED.BY  WORD
  5 1D  BYTE.IN  ."          REPLACED.BY  WORD
  6 00  BYTE.IN  (ABORT)     REPLACED.BY  ABORT
  7 19  BYTE.IN  ERROR       REPLACED.BY  MESSAGE
  8 25  BYTE.IN  ERROR       REPLACED.BY  QUIT
  9 0C  BYTE.IN  WORD        REPLACED.BY  BLOCK
 10 1E  BYTE.IN  CREATE      REPLACED.BY  MESSAGE
 11 2C  BYTE.IN  CREATE      REPLACED.BY  MIN
 12 04  BYTE.IN  ABORT       REPLACED.BY  DR0
 13 2C  BYTE.IN  BUFFER      REPLACED.BY  R/W
 14 30  BYTE.IN  BLOCK       REPLACED.BY  R/W      DECIMAL  ;S
 15

FORTH INTEREST GROUP                                 MAY 1, 1979

SCR # 72
  0 (  ',  FORGET,                                DJK-WFR-79DEC02 )
  1 : '          ( FIND NEXT WORDS PFA; COMPILE IT, IF COMPILING *)
  2     -FIND  0=  0  ?ERROR  DROP  [COMPILE]  LITERAL  ;
  3                                     IMMEDIATE
  4 HEX
  5 : FORGET                      ( Dave Kilbridge's Smart Forget )
  6     [COMPILE]  '  NFA    DUP  FENCE  @  U<  15  ?ERROR
  7     >R  VOC-LINK  @  ( start with latest vocabulary )
  8   BEGIN  R  OVER  U<  WHILE  [COMPILE]  FORTH  DEFINITIONS
  9       @  DUP  VOC-LINK  !  REPEAT  ( unlink from voc list )
 10   BEGIN  DUP  4  -     ( start with phantom nfa )
 11       BEGIN  PFA  LFA  @  DUP  R  U<  UNTIL
 12       OVER  2  -  !  @  -DUP  0=  UNTIL ( end of list ? )
 13     R>  DP  !  ;    -->
 14
 15
0 [IF]
SCR # 72
  0 (  ',  FORGET,  \                                 WFR-79APR28 )
  1 HEX    3   WIDTH
  2 : '          ( FIND NEXT WORDS PFA; COMPILE IT, IF COMPILING *)
  3     -FIND  0=  0  ?ERROR  DROP  [COMPILE]  LITERAL  ;
  4                                     IMMEDIATE
  5
  6 : FORGET            ( FOLLOWING WORD FROM CURRENT VOCABULARY *)
  7     CURRENT  @  CONTEXT  @  -  18  ?ERROR
  8     [COMPILE]  '  DUP  FENCE  @  <  15  ?ERROR
  9     DUP  NFA  DP  !  LFA  @  CURRENT  @  !  ;
 10
 11
 12
 13 -->
 14
 15
[THEN]


SCR # 73
  0 (  CONDITIONAL COMPILER, PER SHIRA                WFR-79APR01 )
  1 : BACK     HERE  -  ,  ;           ( RESOLVE BACKWARD BRANCH *)
  2
  3 : BEGIN    ?COMP  HERE  1  ;                  IMMEDIATE
  4
  5 : ENDIF    ?COMP 2 ?PAIRS  HERE  OVER  -  SWAP  !  ;  IMMEDIATE
  6
  7 : THEN     [COMPILE]  ENDIF  ;    IMMEDIATE
  8
  9 : DO       COMPILE  (DO)  HERE  3  ;            IMMEDIATE
 10
 11 : LOOP     3  ?PAIRS  COMPILE  (LOOP)  BACK  ;  IMMEDIATE
 12
 13 : +LOOP    3  ?PAIRS  COMPILE  (+LOOP)  BACK  ;     IMMEDIATE
 14
 15 : UNTIL    1  ?PAIRS  COMPILE  0BRANCH  BACK  ; IMMEDIATE  -->


SCR # 74
  0 (  CONDITIONAL COMPILER                           WFR-79APR01 )
  1 : END      [COMPILE]  UNTIL  ;  IMMEDIATE
  2
  3 : AGAIN    1  ?PAIRS  COMPILE  BRANCH   BACK  ;   IMMEDIATE
  4
  5 : REPEAT   >R  >R  [COMPILE]  AGAIN
  6               R>  R>  2  -  [COMPILE]  ENDIF  ;  IMMEDIATE
  7
  8 : IF       COMPILE  0BRANCH   HERE  0  ,  2  ;  IMMEDIATE
  9
 10 : ELSE     2  ?PAIRS  COMPILE  BRANCH  HERE  0  ,
 11            SWAP  2  [COMPILE]  ENDIF  2  ;      IMMEDIATE
 12
 13 : WHILE   [COMPILE]  IF  2+  ;    IMMEDIATE
 14
 15 -->

FORTH INTEREST GROUP                                 MAY 1, 1979

SCR # 75
  0 (  NUMERIC PRIMITIVES                             WFR-79APR01 )
  1 : SPACES     0  MAX  -DUP  IF  0  DO  SPACE  LOOP  ENDIF  ;
  2
  3 : <#     PAD  HLD  !  ;
  4
  5 : #>     DROP  DROP  HLD  @  PAD  OVER  -  ;
  6
  7 : SIGN   ROT  0<  IF  2D  HOLD  ENDIF  ;
  8
  9 : #                     ( CONVERT ONE DIGIT, HOLDING IN PAD * )
 10          BASE @ M/MOD ROT 9 OVER < IF  7 + ENDIF 30  +  HOLD  ;
 11
 12 : #S     BEGIN  #  OVER  OVER  OR  0=  UNTIL  ;
 13 -->
 14
 15


SCR # 76
  0 (  OUTPUT OPERATORS                               WFR-79APR20 )
  1 : D.R        ( DOUBLE INTEGER OUTPUT, RIGHT ALIGNED IN FIELD *)
  2        >R  SWAP  OVER  DABS  <#  #S  SIGN  #>
  3        R>  OVER  -  SPACES  TYPE  ;
  4
  5 : D.     0  D.R  SPACE  ;            ( DOUBLE INTEGER OUTPUT *)
  6
  7 : .R     >R  S->D  R>  D.R  ;       ( ALIGNED SINGLE INTEGER *)
  8
  9 : .      S->D  D.  ;                 ( SINGLE INTEGER OUTPUT *)
 10
 11 : ?      @  .  ;                  ( PRINT CONTENTS OF MEMORY *)
 12
 13    .  CFA       MESSAGE  2A  +  !  ( PRINT MESSAGE NUMBER )
 14 -->
 15


SCR # 77
  0 (  PROGRAM DOCUMENTATION                          WFR-79APR20 )
  1 HEX
  2 : LIST                      ( LIST SCREEN BY NUMBER ON STACK *)
  3           DECIMAL   CR  DUP  SCR  !
  4          ." SCR # "   .  10  0  DO  CR  I  3  .R  SPACE
  5          I  SCR  @  .LINE  LOOP  CR  ;
  6
  7 : INDEX      ( PRINT FIRST LINE OF EACH SCREEN FROM-2,  TO-1 *)
  8          0C  EMIT ( FORM FEED )  CR  1+  SWAP
  9          DO  CR  I  3  .R  SPACE
 10              0  I  .LINE
 11              ?TERMINAL  IF  LEAVE  ENDIF  LOOP  ;
 12 : TRIAD     ( PRINT 3 SCREENS ON PAGE, CONTAINING # ON STACK *)
 13          0C  EMIT ( FF )  3  /  3  *  3  OVER  +  SWAP
 14          DO  CR  I  LIST  LOOP  CR
 15          0F  MESSAGE  CR  ;     DECIMAL   -->

FORTH INTEREST GROUP                                 MAY 1, 1979

SCR # 78
  0 (  TOOLS                                          WFR-79APR20 )
  1 HEX
  2 : VLIST                            ( LIST CONTEXT VOCABULARY *)
  3                80  OUT  !    CONTEXT  @  @
  4      BEGIN  OUT  @  C/L  >  IF  CR  0  OUT  !  ENDIF
  5             DUP  ID.  SPACE  SPACE    PFA  LFA  @
  6             DUP  0=  ?TERMINAL  OR  UNTIL  DROP  ;
  7 -->
  8
  9
 10
 11
 12
 13
 14
 15


SCR # 79
  0 (  TOOLS                                          WFR-79MAY03 )
  1 HEX
  2
  3 CREATE  MON         ( CALL MONITOR, SAVING RE-ENTRY TO FORTH *)
  4         0  C,    4C C,   ' LIT 18 + ,    SMUDGE
  5
  6
  7
  8
  9
 10 DECIMAL
 11 HERE             FENCE  !
 12 HERE       28  +ORIGIN  !   ( COLD START FENCE )
 13 HERE       30  +ORIGIN  !   ( COLD START DP )
 14 LATEST     12  +ORIGIN  !   ( TOPMOST WORD )
 15 '  FORTH  6  +  32  +ORIGIN  !  ( COLD VOC-LINK )  ;S


SCR # 80
  0 -->
  1
  2
  3
  4
  5
  6
  7
  8
  9
 10
 11
 12
 13
 14
 15

FORTH INTEREST GROUP                                 MAY 1, 1979
