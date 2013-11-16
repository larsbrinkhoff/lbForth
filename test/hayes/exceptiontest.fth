\ To test the ANS Forth Exception word set and extension words

\ This program was written by Gerry Jackson in 2006, with contributions from
\ others where indicated, and is in the public domain - it can be distributed
\ and/or modified in any way but please retain this notice.

\ This program is distributed in the hope that it will be useful,
\ but WITHOUT ANY WARRANTY; without even the implied warranty of
\ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

\ The tests are not claimed to be comprehensive or correct 

\ ------------------------------------------------------------------------------
\ Version 0.4 1 April 2012  Tests placed in the public domain.
\         0.3 6 March 2009 { and } replaced with T{ and }T
\         0.2 20 April 2007 ANS Forth words changed to upper case
\         0.1 Oct 2006 First version released

\ ------------------------------------------------------------------------------
\ The tests are based on John Hayes test program for the core word set
\
\ Words tested in this file are:
\     CATCH THROW ABORT ABORT"
\
\ ------------------------------------------------------------------------------
\ Assumptions and dependencies:
\     - the forth system under test throws an exception with throw
\       code -13 for a word not found by the text interpreter. The
\       undefined word used is $$qweqweqwert$$,  if this happens to be
\       a valid word in your system change the definition of t7 below
\     - tester.fth or ttester.fth has been loaded prior to this file
\     - CASE, OF, ENDOF and ENDCASE from the core extension wordset
\       are present and work correctly
\ ------------------------------------------------------------------------------
TESTING CATCH THROW

DECIMAL

: t1 9 ;
: c1 1 2 3 ['] t1 CATCH ;
T{ c1 -> 1 2 3 9 0 }T			\ No THROW executed

: t2 8 0 THROW ;
: c2 1 2 ['] t2 CATCH ;
T{ c2 -> 1 2 8 0 }T				\ 0 THROW does nothing

: t3 7 8 9 99 THROW ;
: c3 1 2 ['] t3 CATCH ;
T{ c3 -> 1 2 99 }T				\ Restores stack to CATCH depth

: t4 1- DUP 0> IF RECURSE ELSE 999 THROW -222 THEN ;
: c4 3 4 5 10 ['] t4 CATCH -111 ;
T{ c4 -> 3 4 5 0 999 -111 }T	\ Test return stack unwinding

: t5 2DROP 2DROP 9999 THROW ;
: c5 1 2 3 4 ['] t5 CATCH				\ Test depth restored correctly
	DEPTH >R DROP 2DROP 2DROP R> ;	\ after stack has been emptied
T{ c5 -> 5 }T

\ ------------------------------------------------------------------------------
TESTING ABORT ABORT"

-1	CONSTANT exc_abort
-2 CONSTANT exc_abort"
-13 CONSTANT exc_undef
: t6 ABORT ;

\ The 77 in t10 is necessary for the second ABORT" test as the data stack
\ is restored to a depth of 2 when THROW is executed. The 77 ensures the top
\ of stack value is known for the results check

: t10 77 SWAP ABORT" This should not be displayed" ;
: c6 CATCH
	CASE exc_abort  OF 11 ENDOF
	     exc_abort" OF 12 ENDOF
		  exc_undef  OF 13 ENDOF
	ENDCASE
;

T{ 1 2 ' t6 c6  -> 1 2 11 }T     \ Test that ABORT is caught
T{ 3 0 ' t10 c6 -> 3 77 }T	      \ ABORT" does nothing
T{ 4 5 ' t10 c6 -> 4 77 12 }T    \ ABORT" caught, no message

\ ------------------------------------------------------------------------------
TESTING a system generated exception

: t7 S" 333 $$qweqweqwert$$ 334" EVALUATE 335 ;
: t8 S" 222 t7 223" EVALUATE 224 ;
: t9 S" 111 112 t8 113" EVALUATE 114 ;

T{ 6 7 ' t9 c6 3 -> 6 7 13 3 }T			\ Test unlinking of sources

\ ------------------------------------------------------------------------------

CR .( End of Exception word tests) CR

