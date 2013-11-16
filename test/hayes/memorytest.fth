\ To test the ANS Forth Memory-Allocation word set

\ This program was written by Gerry Jackson in 2006, with contributions from
\ others where indicated, and is in the public domain - it can be distributed
\ and/or modified in any way but please retain this notice.

\ This program is distributed in the hope that it will be useful,
\ but WITHOUT ANY WARRANTY; without even the implied warranty of
\ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

\ The tests are not claimed to be comprehensive or correct 

\ ------------------------------------------------------------------------------
\ Version 0.7 1 April 2012  Tests placed in the public domain.
\         0.6 30 January 2011 CHECKMEM modified to work with ttester.fs
\         0.5 30 November 2009 <false> replaced with FALSE
\         0.4 9 March 2009 Aligned test improved and data space pointer tested
\         0.3 6 March 2009 { and } replaced with T{ and }T
\         0.2 20 April 2007  ANS Forth words changed to upper case
\         0.1 October 2006 First version released

\ ------------------------------------------------------------------------------
\ The tests are based on John Hayes test program for the core word set
\ and requires those files to have been loaded

\ Words tested in this file are:
\     ALLOCATE FREE RESIZE
\     
\ ------------------------------------------------------------------------------
\ Assumptions and dependencies:
\     - that 'addr -1 ALLOCATE' and 'addr -1 RESIZE' will return an error
\     - tester.fth or ttester.fth has been loaded prior to this file
\     - testing FREE failing is not done as it is likely to crash the
\       system
\ ------------------------------------------------------------------------------

TESTING Memory-Allocation word set

DECIMAL

\ ------------------------------------------------------------------------------
TESTING ALLOCATE FREE RESIZE

VARIABLE addr1
VARIABLE datsp

HERE datsp !
T{ 100 ALLOCATE SWAP addr1 ! -> 0 }T
T{ addr1 @ ALIGNED -> addr1 @ }T   \ Test address is aligned
T{ HERE -> datsp @ }T            \ Check data space pointer is unchanged
T{ addr1 @ FREE -> 0 }T

T{ 99 ALLOCATE SWAP addr1 ! -> 0 }T
T{ addr1 @ ALIGNED -> addr1 @ }T
T{ addr1 @ FREE -> 0 }T

T{ 50 ALLOCATE SWAP addr1 ! -> 0 }T

: writemem 0 DO I 1+ OVER C! 1+ LOOP DROP ;	( ad n -- )

\ checkmem is defined this way to maintain compatibility with both
\ tester.fth and ttester.fth which differ in their definitions of T{

: checkmem  ( ad n --- )
   0
   DO
      >R
      T{ R@ C@ -> R> I 1+ SWAP >R }T
      R> 1+
   LOOP
   DROP
;

addr1 @ 50 writemem addr1 @ 50 checkmem

T{ addr1 @ 28 RESIZE SWAP addr1 ! -> 0 }T
addr1 @ 28 checkmem

T{ addr1 @ 200 RESIZE SWAP addr1 ! -> 0 }T
addr1 @ 28 checkmem

\ ------------------------------------------------------------------------------
TESTING failure of RESIZE and ALLOCATE (unlikely to be enough memory)

T{ addr1 @ -1 RESIZE 0= -> addr1 @ FALSE }T

T{ addr1 @ FREE -> 0 }T

T{ -1 ALLOCATE SWAP DROP 0= -> FALSE }T		\ Memory allocate failed

\ ------------------------------------------------------------------------------

CR .( End of Memory-Allocation word tests) CR
