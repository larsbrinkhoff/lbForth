\ To test some of the ANS Forth Core Extension word set

\ This program was written by Gerry Jackson in 2006, with contributions from
\ others where indicated, and is in the public domain - it can be distributed
\ and/or modified in any way but please retain this notice.

\ This program is distributed in the hope that it will be useful,
\ but WITHOUT ANY WARRANTY; without even the implied warranty of
\ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

\ The tests are not claimed to be comprehensive or correct 

\ ------------------------------------------------------------------------------
\ Version 0.6 1 April 2012 Tests placed in the public domain.
\             SAVE-INPUT & RESTORE-INPUT tests, position
\             of T{ moved so that tests work with ttester.fs
\             CONVERT test deleted - obsolete word removed from Forth 200X
\             IMMEDIATE VALUEs tested
\             RECURSE with :NONAME tested
\             PARSE and .( tested
\             Parsing behaviour of C" added
\         0.5 14 September 2011 Removed the double [ELSE] from the
\             initial SAVE-INPUT & RESTORE-INPUT test
\         0.4 30 November 2009  max-int replaced with max-intx to
\             avoid redefinition warnings.
\         0.3  6 March 2009 { and } replaced with T{ and }T
\                           CONVERT test now independent of cell size
\         0.2  20 April 2007 ANS Forth words changed to upper case
\                            Tests qd3 to qd6 by Reinhold Straub
\         0.1  Oct 2006 First version released
\ ------------------------------------------------------------------------------
\ This is only a partial test of the core extension words.
\ The tests are based on John Hayes test program for the core word set

\ Words tested in this file are:
\     TRUE FALSE :NONAME ?DO VALUE TO CASE OF ENDOF ENDCASE PARSE
\     C" CONVERT COMPILE, [COMPILE] SAVE-INPUT RESTORE-INPUT .(
\ ------------------------------------------------------------------------------
\ Assumptions:
\     - tester.fth or ttester.fth has been included prior to this file
\ ------------------------------------------------------------------------------
TESTING Core Extension words

DECIMAL

0 INVERT 1 RSHIFT CONSTANT max-intx  \ 01...1


TESTING TRUE FALSE

T{ TRUE  -> 0 INVERT }T
T{ FALSE -> 0 }T

\ ------------------------------------------------------------------------------
TESTING :NONAME with and without RECURSEs

VARIABLE nn1
VARIABLE nn2
:NONAME 1234 ; nn1 !
:NONAME 9876 ; nn2 !
T{ nn1 @ EXECUTE -> 1234 }T
T{ nn2 @ EXECUTE -> 9876 }T

T{ :NONAME ( n -- 0,1,..n ) DUP IF DUP >R 1- RECURSE R> THEN ;
   CONSTANT rn1 -> }T
T{ 0 rn1 EXECUTE -> 0 }T
T{ 4 rn1 EXECUTE -> 0 1 2 3 4 }T

:NONAME  ( n -- n1 )    \ Multiple RECURSEs in one definition
   1- DUP
   CASE 0 OF EXIT ENDOF
        1 OF 11 SWAP RECURSE ENDOF
        2 OF 22 SWAP RECURSE ENDOF
        3 OF 33 SWAP RECURSE ENDOF
        DROP ABS RECURSE EXIT
   ENDCASE
; CONSTANT rn2

T{  1 rn2 EXECUTE -> 0 }T
T{  2 rn2 EXECUTE -> 11 0 }T
T{  4 rn2 EXECUTE -> 33 22 11 0 }T
T{ 25 rn2 EXECUTE -> 33 22 11 0 }T

\ ------------------------------------------------------------------------------
TESTING ?DO

: qd ?DO I LOOP ;
T{ 789 789 qd -> }T
T{ -9876 -9876 qd -> }T
T{ 5 0 qd -> 0 1 2 3 4 }T

: qd1 ?DO I 10 +LOOP ;
T{ 50 1 qd1 -> 1 11 21 31 41 }T
T{ 50 0 qd1 -> 0 10 20 30 40 }T

: qd2 ?DO I 3 > IF LEAVE ELSE I THEN LOOP ;
T{ 5 -1 qd2 -> -1 0 1 2 3 }T

: qd3 ?DO I 1 +LOOP ;
T{ 4  4 qd3 -> }T
T{ 4  1 qd3 -> 1 2 3 }T
T{ 2 -1 qd3 -> -1 0 1 }T

: qd4 ?DO I -1 +LOOP ;
T{  4 4 qd4 -> }T
T{  1 4 qd4 -> 4 3 2 1 }T
T{ -1 2 qd4 -> 2 1 0 -1 }T

: qd5 ?DO I -10 +LOOP ;
T{   1 50 qd5 -> 50 40 30 20 10 }T
T{   0 50 qd5 -> 50 40 30 20 10 0 }T
T{ -25 10 qd5 -> 10 0 -10 -20 }T

VARIABLE iters
VARIABLE incrmnt

: qd6 ( limit start increment -- )
   incrmnt !
   0 iters !
   ?DO
      1 iters +!
      I
      iters @  6 = IF LEAVE THEN
      incrmnt @
   +LOOP iters @
;

T{  4  4 -1 qd6 -> 0 }T
T{  1  4 -1 qd6 -> 4 3 2 1 4 }T
T{  4  1 -1 qd6 -> 1 0 -1 -2 -3 -4 6 }T
T{  4  1  0 qd6 -> 1 1 1 1 1 1 6 }T
T{  0  0  0 qd6 -> 0 }T
T{  1  4  0 qd6 -> 4 4 4 4 4 4 6 }T
T{  1  4  1 qd6 -> 4 5 6 7 8 9 6 }T
T{  4  1  1 qd6 -> 1 2 3 3 }T
T{  4  4  1 qd6 -> 0 }T
T{  2 -1 -1 qd6 -> -1 -2 -3 -4 -5 -6 6 }T
T{ -1  2 -1 qd6 -> 2 1 0 -1 4 }T
T{  2 -1  0 qd6 -> -1 -1 -1 -1 -1 -1 6 }T
T{ -1  2  0 qd6 -> 2 2 2 2 2 2 6 }T
T{ -1  2  1 qd6 -> 2 3 4 5 6 7 6 }T
T{  2 -1  1 qd6 -> -1 0 1 3 }T

\ ------------------------------------------------------------------------------
TESTING VALUE TO

T{ 111 VALUE val1 -999 VALUE val2 -> }T
T{ val1 -> 111 }T
T{ val2 -> -999 }T
T{ 222 TO val1 -> }T
T{ val1 -> 222 }T
T{ : vd1 val1 ; -> }T
T{ vd1 -> 222 }T
T{ : vd2 TO val2 ; -> }T
T{ val2 -> -999 }T
T{ -333 vd2 -> }T
T{ val2 -> -333 }T
T{ val1 -> 222 }T
T{ 123 VALUE val3 IMMEDIATE val3 -> 123 }T
T{ : vd3 val3 LITERAL ; vd3 -> 123 }T

\ ------------------------------------------------------------------------------
TESTING CASE OF ENDOF ENDCASE

: cs1 CASE 1 OF 111 ENDOF
           2 OF 222 ENDOF
           3 OF 333 ENDOF
           >R 999 R>
      ENDCASE
;

T{ 1 cs1 -> 111 }T
T{ 2 cs1 -> 222 }T
T{ 3 cs1 -> 333 }T
T{ 4 cs1 -> 999 }T

: cs2 >R CASE -1 OF CASE R@ 1 OF 100 ENDOF
                            2 OF 200 ENDOF
                           >R -300 R>
                    ENDCASE
                 ENDOF
              -2 OF CASE R@ 1 OF -99  ENDOF
                            >R -199 R>
                    ENDCASE
                 ENDOF
                 >R 299 R>
         ENDCASE R> DROP
;

T{ -1 1 cs2 ->  100 }T
T{ -1 2 cs2 ->  200 }T
T{ -1 3 cs2 -> -300 }T
T{ -2 1 cs2 -> -99  }T
T{ -2 2 cs2 -> -199 }T
T{  0 2 cs2 ->  299 }T

\ ------------------------------------------------------------------------------
TESTING C"

T{ : cq1 C" 123" ; -> }T
T{ cq1 COUNT EVALUATE -> 123 }T
T{ : cq2 C" " ; -> }T
T{ cq2 COUNT EVALUATE -> }T
T{ : cq3 C" 2345"COUNT EVALUATE ; cq3 -> 2345 }T

\ ------------------------------------------------------------------------------
TESTING COMPILE, [COMPILE]

:NONAME DUP + ; CONSTANT dup+
T{ : q dup+ COMPILE, ; -> }T
T{ : as1 [ q ] ; -> }T
T{ 123 as1 -> 246 }T

T{ : [c1] [COMPILE] DUP ; IMMEDIATE -> }T
T{ 123 [c1] -> 123 123 }T                 \ With default compilation semantics
T{ :  [c2] [COMPILE] [c1] ; -> }T
T{ 234 [c2] -> 234 234 }T                 \ With an immediate word
T{ : [cif] [COMPILE] IF ; IMMEDIATE -> }T
T{ : [c3] [cif] 111 ELSE 222 THEN ; -> }T \ With special compilation semantics
T{ -1 [c3] -> 111 }T
T{  0 [c3] -> 222 }T

\ ------------------------------------------------------------------------------
\ Cannot automatically test SAVE-INPUT and RESTORE-INPUT from a console source

TESTING SAVE-INPUT and RESTORE-INPUT with a file source

VARIABLE siv -1 siv !

: NeverExecuted
	." This should never be executed" ABORT
;

T{ 11111 SAVE-INPUT

siv @

[IF]
	0 siv !
	RESTORE-INPUT
	NeverExecuted
[ELSE]

TESTING the -[ELSE]- part is executed
22222

[THEN]

   -> 11111 0 22222 }T	\ 0 comes from RESTORE-INPUT

TESTING SAVE-INPUT and RESTORE-INPUT with a string source

VARIABLE si_inc 0 si_inc !

: si1
	si_inc @ >IN +!
	15 si_inc !
;

: s$ S" SAVE-INPUT si1 RESTORE-INPUT 12345" ;

T{ s$ EVALUATE si_inc @ -> 0 2345 15 }T

TESTING nested SAVE-INPUT and RESTORE-INPUT

: read_a_line
	REFILL 0=
	ABORT" REFILL failed"
;

0 si_inc !

2VARIABLE 2res -1. 2res 2!

: si2
	read_a_line
	read_a_line
	SAVE-INPUT
	read_a_line
	read_a_line
	s$ EVALUATE 2res 2!
	RESTORE-INPUT
;

\ WARNING: do not delete or insert lines of text after si2 is called
\ otherwise the next test will fail

T{ si2
33333					\ This line should be ignored
2res 2@ 44444		\ RESTORE-INPUT should return to this line

55555
TESTING the nested results
 -> 0 0 2345 44444 55555 }T

\ End of warning

\ ------------------------------------------------------------------------------
TESTING .(

T{ S" A string"2DROP -> }T
T{ CR .( You should see -9876: ) -9876 . -> }T
T{ CR .( Repeated: ).( -9876)CR -> }T

\ ------------------------------------------------------------------------------
TESTING PARSE

T{ CHAR | PARSE 1234| DUP ROT ROT EVALUATE -> 4 1234 }T
T{ CHAR ^ PARSE  23 45 ^ DUP ROT ROT EVALUATE -> 7 23 45 }T
: pa1 [CHAR] $ PARSE DUP >R PAD SWAP CHARS MOVE PAD R> ;
T{ pa1 3456
   DUP ROT ROT EVALUATE -> 4 3456 }T
T{ CHAR a PARSE a SWAP DROP -> 0 }T
T{ CHAR z PARSE
   SWAP DROP -> 0 }T
T{ CHAR " PARSE 4567 "DUP ROT ROT EVALUATE -> 5 4567 }T
 
\ ------------------------------------------------------------------------------

CR .( End of Core Extension word tests) CR


