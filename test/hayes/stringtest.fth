\ To test the ANS Forth String word set

\ This program was written by Gerry Jackson in 2006, with contributions from
\ others where indicated, and is in the public domain - it can be distributed
\ and/or modified in any way but please retain this notice.

\ This program is distributed in the hope that it will be useful,
\ but WITHOUT ANY WARRANTY; without even the implied warranty of
\ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

\ The tests are not claimed to be comprehensive or correct 

\ ------------------------------------------------------------------------------
\ Version 0.6 1 April 2012 Tests placed in the public domain.
\         0.5 29 April 2010 Added tests for SEARCH and COMPARE with
\             all strings zero length (suggested by Krishna Myneni).
\             SLITERAL test amended in line with comp.lang.forth
\             discussion
\         0.4 30 November 2009 <true> and <false> replaced with TRUE
\             and FALSE
\         0.3 6 March 2009 { and } replaced with T{ and }T
\         0.2 20 April 2007 ANS Forth words changed to upper case
\         0.1 Oct 2006 First version released

\ ------------------------------------------------------------------------------
\ The tests are based on John Hayes test program for the core word set
\ and requires those files to have been loaded

\ Words tested in this file are:
\     -TRAILING /STRING BLANK CMOVE CMOVE> COMPARE SEARCH SLITERAL
\
\ ------------------------------------------------------------------------------
\ Assumptions and dependencies:
\     - tester.fth or ttester.fth has been loaded prior to this file
\     - COMPARE is case sensitive
\ ------------------------------------------------------------------------------

TESTING String word set

DECIMAL

T{ :  s1 S" abcdefghijklmnopqrstuvwxyz" ; -> }T
T{ :  s2 S" abc"   ; -> }T
T{ :  s3 S" jklmn" ; -> }T
T{ :  s4 S" z"     ; -> }T
T{ :  s5 S" mnoq"  ; -> }T
T{ :  s6 S" 12345" ; -> }T
T{ :  s7 S" "      ; -> }T
T{ :  s8 S" abc  " ; -> }T
T{ :  s9 S"      " ; -> }T
T{ : s10 S"    a " ; -> }T

\ ------------------------------------------------------------------------------
TESTING -TRAILING

T{  s1 -TRAILING -> s1 }T
T{  s8 -TRAILING -> s8 2 - }T
T{  s7 -TRAILING -> s7 }T
T{  s9 -TRAILING -> s9 DROP 0 }T
T{ s10 -TRAILING -> s10 1- }T

\ ------------------------------------------------------------------------------
TESTING /STRING

T{ s1  5 /STRING -> s1 SWAP 5 + SWAP 5 - }T
T{ s1 10 /STRING -4 /STRING -> s1 6 /STRING }T
T{ s1  0 /STRING -> s1 }T

\ ------------------------------------------------------------------------------
TESTING SEARCH

T{ s1 s2 SEARCH -> s1 TRUE }T
T{ s1 s3 SEARCH -> s1  9 /STRING TRUE }T
T{ s1 s4 SEARCH -> s1 25 /STRING TRUE }T
T{ s1 s5 SEARCH -> s1 FALSE }T
T{ s1 s6 SEARCH -> s1 FALSE }T
T{ s1 s7 SEARCH -> s1 TRUE }T
T{ s7 PAD 0 SEARCH -> s7 TRUE }T

\ ------------------------------------------------------------------------------
TESTING COMPARE

T{ s1 s1 COMPARE -> 0 }T
T{ s1 PAD SWAP CMOVE -> }T
T{ s1 PAD OVER COMPARE -> 0 }T
T{ s1 PAD 6 COMPARE -> 1 }T
T{ PAD 10 s1 COMPARE -> -1 }T
T{ s1 PAD 0 COMPARE -> 1 }T
T{ PAD 0 s1 COMPARE -> -1 }T
T{ s1 s6 COMPARE ->  1 }T
T{ s6 s1 COMPARE -> -1 }T
T{ s7 PAD 0 COMPARE -> 0 }T

: "abdde"  S" abdde" ;
: "abbde"  S" abbde" ;
: "abcdf"  S" abcdf" ;
: "abcdee" S" abcdee" ;

T{ s1 "abdde" COMPARE -> -1 }T
T{ s1 "abbde" COMPARE ->  1 }T
T{ s1 "abcdf"  COMPARE -> -1 }T
T{ s1 "abcdee" COMPARE ->  1 }T

: s11 S" 0abc" ;
: s12 S" 0aBc" ;

T{ s11 s12  COMPARE -> 1 }T
T{ s12 s11  COMPARE -> -1 }T

\ ------------------------------------------------------------------------------
TESTING CMOVE and CMOVE>

PAD 30 CHARS 0 FILL
T{ s1 PAD SWAP CMOVE -> }T
T{ s1 PAD s1 SWAP DROP COMPARE -> 0 }T
T{ s6 PAD 10 CHARS + SWAP CMOVE -> }T
T{ S" abcdefghij12345pqrstuvwxyz" PAD s1 SWAP DROP COMPARE -> 0 }T
T{ PAD 15 CHARS + PAD CHAR+ 6 CMOVE -> }T
T{ S" apqrstuhij12345pqrstuvwxyz" PAD 26 COMPARE -> 0 }T
T{ PAD PAD 3 CHARS + 7 CMOVE -> }T
T{ S" apqapqapqa12345pqrstuvwxyz" PAD 26 COMPARE -> 0 }T
T{ PAD PAD CHAR+ 10 CMOVE -> }T
T{ S" aaaaaaaaaaa2345pqrstuvwxyz" PAD 26 COMPARE -> 0 }T
T{ s7 PAD 14 CHARS + SWAP CMOVE -> }T
T{ S" aaaaaaaaaaa2345pqrstuvwxyz" PAD 26 COMPARE -> 0 }T

PAD 30 CHARS 0 FILL

T{ s1 PAD SWAP CMOVE> -> }T
T{ s1 PAD s1 SWAP DROP COMPARE -> 0 }T
T{ s6 PAD 10 CHARS + SWAP CMOVE> -> }T
T{ S" abcdefghij12345pqrstuvwxyz" PAD s1 SWAP DROP COMPARE -> 0 }T
T{ PAD 15 CHARS + PAD CHAR+ 6 CMOVE> -> }T
T{ S" apqrstuhij12345pqrstuvwxyz" PAD 26 COMPARE -> 0 }T
T{ PAD 13 CHARS + PAD 10 CHARS + 7 CMOVE> -> }T
T{ S" apqrstuhijtrstrstrstuvwxyz" PAD 26 COMPARE -> 0 }T
T{ PAD 12 CHARS + PAD 11 CHARS + 10 CMOVE> -> }T
T{ S" apqrstuhijtvvvvvvvvvvvwxyz" PAD 26 COMPARE -> 0 }T
T{ s7 PAD 14 CHARS + SWAP CMOVE> -> }T
T{ S" apqrstuhijtvvvvvvvvvvvwxyz" PAD 26 COMPARE -> 0 }T

\ ------------------------------------------------------------------------------
TESTING BLANK

: s13 S" aaaaa      a" ;   \ Don't move this down or might corrupt PAD

T{ PAD 25 CHAR a FILL -> }T
T{ PAD 5 CHARS + 6 BLANK -> }T
T{ PAD 12 s13 COMPARE -> 0 }T

\ ------------------------------------------------------------------------------
TESTING SLITERAL

T{ HERE DUP s1 DUP ALLOT ROT SWAP CMOVE s1 SWAP DROP 2CONSTANT s1a -> }T
T{ : s14 [ s1a ] SLITERAL ; -> }T
T{ s1a s14 COMPARE -> 0 }T
T{ s1a DROP s14 DROP = -> FALSE }T 

\ ------------------------------------------------------------------------------

CR .( End of String word tests) CR
