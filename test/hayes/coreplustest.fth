\ Additional tests on the the ANS Forth Core word set

\ This program was written by Gerry Jackson in 2007, with contributions from
\ others where indicated, and is in the public domain - it can be distributed
\ and/or modified in any way but please retain this notice.

\ This program is distributed in the hope that it will be useful,
\ but WITHOUT ANY WARRANTY; without even the implied warranty of
\ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

\ The tests are not claimed to be comprehensive or correct 

\ ------------------------------------------------------------------------------
\ Version 0.3  1 April 2012 Tests placed in the public domain.
\              Testing multiple ELSE's.
\              Further tests on DO +LOOPs.
\              Ackermann function added to test RECURSE.
\              >IN manipulation in interpreter mode
\              Immediate CONSTANTs, VARIABLEs and CREATEd words tests.
\              :NONAME with RECURSE moved to core extension tests.
\              Parsing behaviour of S" ." and ( tested
\         0.2  6 March 2009 { and } replaced with T{ and }T
\              Added extra RECURSE tests
\         0.1  20 April 2007 Created
\ ------------------------------------------------------------------------------
\ The tests are based on John Hayes test program for the core word set
\
\ This file provides some more tests on Core words where the original Hayes
\ tests are thought to be incomplete
\
\ Words tested in this file are:
\     DO +LOOP RECURSE ELSE >IN IMMEDIATE
\ ------------------------------------------------------------------------------
\ Assumptions and dependencies:
\     - tester.fth or ttester.fth has been loaded prior to this file
\     - core.fr has been loaded so that constants MAX-INT, MIN-INT and
\       MAX-UINT are defined
\ ------------------------------------------------------------------------------

DECIMAL

TESTING DO +LOOP with run-time increment, negative increment, infinite loop
\ Contributed by Reinhold Straub

VARIABLE iterations
VARIABLE increment
: gd7 ( limit start increment -- )
   increment !
   0 iterations !
   DO
      1 iterations +!
      I
      iterations @  6 = IF LEAVE THEN
      increment @
   +LOOP iterations @
;

T{  4  4 -1 gd7 -> 4 1 }T
T{  1  4 -1 gd7 -> 4 3 2 1 4 }T
T{  4  1 -1 gd7 -> 1 0 -1 -2 -3 -4 6 }T
T{  4  1  0 gd7 -> 1 1 1 1 1 1 6 }T
T{  0  0  0 gd7 -> 0 0 0 0 0 0 6 }T
T{  1  4  0 gd7 -> 4 4 4 4 4 4 6 }T
T{  1  4  1 gd7 -> 4 5 6 7 8 9 6 }T
T{  4  1  1 gd7 -> 1 2 3 3 }T
T{  4  4  1 gd7 -> 4 5 6 7 8 9 6 }T
T{  2 -1 -1 gd7 -> -1 -2 -3 -4 -5 -6 6 }T
T{ -1  2 -1 gd7 -> 2 1 0 -1 4 }T
T{  2 -1  0 gd7 -> -1 -1 -1 -1 -1 -1 6 }T
T{ -1  2  0 gd7 -> 2 2 2 2 2 2 6 }T
T{ -1  2  1 gd7 -> 2 3 4 5 6 7 6 }T
T{  2 -1  1 gd7 -> -1 0 1 3 }T
T{ -20 30 -10 gd7 -> 30 20 10 0 -10 -20 6 }T
T{ -20 31 -10 gd7 -> 31 21 11 1 -9 -19 6 }T
T{ -20 29 -10 gd7 -> 29 19 9 -1 -11 5 }T

\ ------------------------------------------------------------------------------
TESTING DO +LOOP with large and small increments

\ Contributed by Andrew Haley

MAX-UINT 8 RSHIFT 1+ CONSTANT ustep
ustep NEGATE CONSTANT -ustep
MAX-INT 7 RSHIFT 1+ CONSTANT step
step NEGATE CONSTANT -step

VARIABLE bump

T{ : gd8 bump ! DO 1+ bump @ +LOOP ; -> }T

T{ 0 MAX-UINT 0 ustep gd8 -> 256 }T
T{ 0 0 MAX-UINT -ustep gd8 -> 256 }T

T{ 0 MAX-INT MIN-INT step gd8 -> 256 }T
T{ 0 MIN-INT MAX-INT -step gd8 -> 256 }T

\ Two's complement arithmetic, wraps around modulo wordsize
\ Only tested if the Forth system does wrap around, use of conditional
\ compilation deliberately avoided

MAX-INT 1+ MIN-INT = CONSTANT +wrap?
MIN-INT 1- MAX-INT = CONSTANT -wrap?
MAX-UINT 1+ 0=       CONSTANT +uwrap?
0 1- MAX-UINT =      CONSTANT -uwrap?

: gd9  ( n limit start step f result -- )
   >R IF gd8 ELSE 2DROP 2DROP R@ THEN -> R> }T
;

T{ 0 0 0  ustep +uwrap? 256 gd9
T{ 0 0 0 -ustep -uwrap?   1 gd9
T{ 0 MIN-INT MAX-INT  step +wrap? 1 gd9
T{ 0 MAX-INT MIN-INT -step -wrap? 1 gd9

\ ------------------------------------------------------------------------------
TESTING DO +LOOP with maximum and minimum increments

: (-mi) MAX-INT DUP NEGATE + 0= IF MAX-INT NEGATE ELSE -32767 THEN ;
(-mi) CONSTANT -max-int

T{ 0 1 0 MAX-INT gd8  -> 1 }T
T{ 0 -max-int NEGATE -max-int OVER gd8  -> 2 }T

T{ 0 MAX-INT  0 MAX-INT gd8  -> 1 }T
T{ 0 MAX-INT  1 MAX-INT gd8  -> 1 }T
T{ 0 MAX-INT -1 MAX-INT gd8  -> 2 }T
T{ 0 MAX-INT dup 1- MAX-INT gd8  -> 1 }T

T{ 0 MIN-INT 1+   0 MIN-INT gd8  -> 1 }T
T{ 0 MIN-INT 1+  -1 MIN-INT gd8  -> 1 }T
T{ 0 MIN-INT 1+   1 MIN-INT gd8  -> 2 }T
T{ 0 MIN-INT 1+ DUP MIN-INT gd8  -> 1 }T

\ ------------------------------------------------------------------------------
TESTING multiple RECURSEs in one colon definition

: ack ( m n -- u )    \ Ackermann function, from Rosetta Code
   OVER 0= IF  NIP 1+ EXIT  THEN       \ ack(0, n) = n+1
   SWAP 1- SWAP                        ( -- m-1 n )
   DUP  0= IF  1+  RECURSE EXIT  THEN  \ ack(m, 0) = ack(m-1, 1)
   1- OVER 1+ SWAP RECURSE RECURSE     \ ack(m, n) = ack(m-1, ack(m,n-1))
;

T{ 0 0 ack ->  1 }T
T{ 3 0 ack ->  5 }T
T{ 2 4 ack -> 11 }T

\ ------------------------------------------------------------------------------
TESTING multiple ELSE's in an IF statement
\ Discussed on comp.lang.forth and accepted as valid ANS Forth

: melse IF 1 ELSE 2 ELSE 3 ELSE 4 ELSE 5 THEN ;
T{ 0 melse -> 2 4 }T
T{ -1 melse -> 1 3 5 }T

\ ------------------------------------------------------------------------------
TESTING manipulation of >IN in interpreter mode

T{ 123456 depth over 9 < 35 and + 3 + >in ! -> 123456 23456 3456 456 56 6 }T
T{ 14145 8115 ?dup 0= 34 and >in +! tuck mod 14 >in ! GCD calculation -> 15 }T

\ ------------------------------------------------------------------------------
TESTING IMMEDIATE with CONSTANT  VARIABLE and CREATE [ ... DOES> ]

T{ 123 CONSTANT iw1 IMMEDIATE iw1 -> 123 }T
T{ : iw2 iw1 LITERAL ; iw2 -> 123 }T
T{ VARIABLE iw3 IMMEDIATE 234 iw3 ! iw3 @ -> 234 }T
T{ : iw4 iw3 [ @ ] LITERAL ; iw4 -> 234 }T
T{ :noname [ 345 ] iw3 [ ! ] ; DROP iw3 @ -> 345 }T
T{ CREATE iw5 456 , IMMEDIATE -> }T
T{ :noname iw5 [ @ iw3 ! ] ; DROP iw3 @ -> 456 }T
T{ : iw6 CREATE , IMMEDIATE DOES> @ 1+ ; -> }T
T{ 111 iw6 iw7 iw7 -> 112 }T
T{ : iw8 iw7 LITERAL 1+ ; iw8 -> 113 }T
T{ : iw9 CREATE , DOES> @ 2 + IMMEDIATE ; -> }T
: find-iw bl word find nip ;  ( -- 0 | 1 | -1 )
T{ 222 iw9 iw10 find-iw iw10 -> -1 }T   \ iw10 is not immediate
T{ iw10 find-iw iw10 -> 224 1 }T        \ iw10 becomes immediate

\ ------------------------------------------------------------------------------
TESTING parsing behaviour of S" ." and (
\ which should parse to just beyond the terminating character no space needed

T{ S" A string"2DROP -> }T
T{ ( A comment)1234 -> 1234 }T
T{ : pb1 cr ." You should see 2345: "." 2345"( A comment); pb1 -> }T
 
\ ------------------------------------------------------------------------------

CR .( End of additional Core tests) CR
