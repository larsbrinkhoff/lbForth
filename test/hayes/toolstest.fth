\ To test some of the ANS Forth Programming Tools and extension wordset

\ This program was written by Gerry Jackson in 2006, with contributions from
\ others where indicated, and is in the public domain - it can be distributed
\ and/or modified in any way but please retain this notice.

\ This program is distributed in the hope that it will be useful,
\ but WITHOUT ANY WARRANTY; without even the implied warranty of
\ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

\ The tests are not claimed to be comprehensive or correct 

\ ------------------------------------------------------------------------------
\ Version 0.6  1 April 2012 Tests placed in the public domain.
\              Further tests on [IF] [ELSE] [THEN]
\         0.5  30 November 2009 <true> and <false> replaced with TRUE and FALSE
\         0.4  6 March 2009 ENDIF changed to THEN. {...} changed to T{...}T
\         0.3  20 April 2007 ANS Forth words changed to upper case
\         0.2  30 Oct 2006 updated following GForth test to avoid
\              changing stack depth during a colon definition
\         0.1  Oct 2006 First version released

\ ------------------------------------------------------------------------------
\ The tests are based on John Hayes test program

\ Words tested in this file are:
\     AHEAD [IF] [ELSE] [THEN] CS-PICK CS-ROLL
\     

\ Words not tested:
\     .S ? DUMP SEE WORDS
\     ;CODE ASSEMBLER BYE CODE EDITOR FORGET STATE 
\ ------------------------------------------------------------------------------
\ Assumptions and dependencies:
\     - tester.fth or ttester.fth has been loaded prior to this file
\ ------------------------------------------------------------------------------

DECIMAL

\ ------------------------------------------------------------------------------
TESTING AHEAD

T{ : pt1 AHEAD 1111 2222 THEN 3333 ; -> }T
T{ pt1 -> 3333 }T

\ ------------------------------------------------------------------------------
TESTING [IF] [ELSE] [THEN]

T{ TRUE  [IF] 111 [ELSE] 222 [THEN] -> 111 }T
T{ FALSE [IF] 111 [ELSE] 222 [THEN] -> 222 }T

T{ TRUE  [IF] 1     \ Code spread over more than 1 line
             2
          [ELSE]
             3
             4
          [THEN] -> 1 2 }T
T{ FALSE [IF]
             1 2
          [ELSE]
             3 4
          [THEN] -> 3 4 }T

T{ TRUE  [IF] 1 TRUE  [IF] 2 [ELSE] 3 [THEN] [ELSE] 4 [THEN] -> 1 2 }T
T{ FALSE [IF] 1 TRUE  [IF] 2 [ELSE] 3 [THEN] [ELSE] 4 [THEN] -> 4 }T
T{ TRUE  [IF] 1 FALSE [IF] 2 [ELSE] 3 [THEN] [ELSE] 4 [THEN] -> 1 3 }T
T{ FALSE [IF] 1 FALSE [IF] 2 [ELSE] 3 [THEN] [ELSE] 4 [THEN] -> 4 }T

\ ------------------------------------------------------------------------------
TESTING immediacy of [IF] [ELSE] [THEN]

T{ : pt2 [  0 ] [IF] 1111 [ELSE] 2222 [THEN]  ; pt2 -> 2222 }T
T{ : pt3 [ -1 ] [IF] 3333 [ELSE] 4444 [THEN]  ; pt3 -> 3333 }T
: pt9 bl WORD FIND ;
T{ pt9 [IF]   NIP -> 1 }T
T{ pt9 [ELSE] NIP -> 1 }T
T{ pt9 [THEN] NIP -> 1 }T

\ -----------------------------------------------------------------------------
TESTING [IF] and [ELSE] carry out a text scan by parsing and discarding words
\ so that an [ELSE] or [THEN] in a comment or string is recognised

: pt10 REFILL DROP REFILL DROP ;

T{ 0  [IF]            \ Words ignored up to [ELSE] 2
      [THEN] -> 2 }T
T{ -1 [IF] 2 [ELSE] 3 s" [THEN] 4 pt10 ignored to end of line"
      [THEN]          \ Precaution in case [THEN] in string isn't recognised
   -> 2 4 }T

\ ------------------------------------------------------------------------------
TESTING CS-PICK and CS-ROLL

\ Test pt5 based on example in ANS document p 176.

: ?repeat
   0 CS-PICK POSTPONE UNTIL
; IMMEDIATE

VARIABLE pt4

T{ : pt5  ( n1 -- )
      pt4 !
      BEGIN
         -1 pt4 +!
         pt4 @ 4 > 0= ?repeat \ Back to BEGIN if false
         111
         pt4 @ 3 > 0= ?repeat
         222
         pt4 @ 2 > 0= ?repeat
         333
         pt4 @ 1 =
      UNTIL
; -> }T

T{ 6 pt5 -> 111 111 222 111 222 333 111 222 333 }T


T{ : ?DONE POSTPONE IF 1 CS-ROLL ; IMMEDIATE -> }T  \ Same as WHILE
T{ : pt6 
      >R
      BEGIN
         R@
      ?DONE
         R@
         R> 1- >R
      REPEAT
      R> DROP
   ; -> }T

T{ 5 pt6 -> 5 4 3 2 1 }T

: mix_up 2 CS-ROLL ; IMMEDIATE  \ cs-rot

: pt7    ( f3 f2 f1 -- ? )
   IF 1111 ROT ROT         ( -- 1111 f3 f2 )     ( cs: -- orig1 )
      IF 2222 SWAP         ( -- 1111 2222 f3 )   ( cs: -- orig1 orig2 )
         IF                                      ( cs: -- orig1 orig2 orig3 )
            3333 mix_up    ( -- 1111 2222 3333 ) ( cs: -- orig2 orig3 orig1 )
         THEN                                    ( cs: -- orig2 orig3 )
         4444        \ Hence failure of first IF comes here and falls through
      THEN                                      ( cs: -- orig2 )
      5555           \ Failure of 3rd IF comes here
   THEN                                         ( cs: -- )
   6666              \ Failure of 2nd IF comes here
;

T{ -1 -1 -1 pt7 -> 1111 2222 3333 4444 5555 6666 }T
T{  0 -1 -1 pt7 -> 1111 2222 5555 6666 }T
T{  0  0 -1 pt7 -> 1111 0    6666 }T
T{  0  0  0 pt7 -> 0    0    4444 5555 6666 }T

: [1cs-roll] 1 CS-ROLL ; IMMEDIATE

T{ : pt8 
      >R
      AHEAD 111
      BEGIN 222 
         [1cs-roll]
         THEN
         333
         R> 1- >R
         R@ 0<
      UNTIL
      R> DROP
   ; -> }T

T{ 1 pt8 -> 333 222 333 }T

\ ------------------------------------------------------------------------------

CR .( End of Programming Tools word tests) CR
