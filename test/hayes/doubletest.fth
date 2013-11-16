\ To test the ANS Forth Double-Number word set and double number extensions

\ This program was written by Gerry Jackson in 2006, with contributions from
\ others where indicated, and is in the public domain - it can be distributed
\ and/or modified in any way but please retain this notice.

\ This program is distributed in the hope that it will be useful,
\ but WITHOUT ANY WARRANTY; without even the implied warranty of
\ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

\ The tests are not claimed to be comprehensive or correct 
\ ------------------------------------------------------------------------------
\ Version 0.6   1 April 2012 Tests placed in the public domain.
\               Immediate 2CONSTANTs and 2VARIABLEs tested
\         0.5   20 November 2009 Various constants renamed to avoid
\               redefinition warnings. <true> and <false> replaced
\               with TRUE and FALSE
\         0.4   6 March 2009 { and } replaced with T{ and }T
\               Tests rewritten to be independent of word size and
\               tests re-ordered
\         0.3   20 April 2007 ANS Forth words changed to upper case
\         0.2   30 Oct 2006 Updated following GForth test to include
\               various constants from core.fr
\         0.1   Oct 2006 First version released
\ ------------------------------------------------------------------------------
\ The tests are based on John Hayes test program for the core word set

\ Words tested in this file are:
\     2CONSTANT 2LITERAL 2VARIABLE D+ D- D. D.R D0< D0= D2* D2/
\     D< D= D>S DABS DMAX DMIN DNEGATE M*/ M+ 2ROT DU<
\ Also tests the interpreter and compiler reading a double number
\ ------------------------------------------------------------------------------
\ Assumptions and dependencies:
\     - tester.fth or ttester.fth has been included prior to this file
\     - core words and core extension words have been tested
\ ------------------------------------------------------------------------------
\ Constant definitions

DECIMAL
0 INVERT        CONSTANT 1sd
1sd 1 RSHIFT    CONSTANT max-intd   \ 01...1
max-intd INVERT CONSTANT min-intd   \ 10...0
max-intd 2/     CONSTANT hi-int     \ 001...1
min-intd 2/     CONSTANT lo-int     \ 110...1

\ ------------------------------------------------------------------------------
TESTING interpreter and compiler reading a double number

T{ 1. -> 1 0 }T
T{ -2. -> -2 -1 }T
T{ : rdl1 3. ; rdl1 -> 3 0 }T
T{ : rdl2 -4. ; rdl2 -> -4 -1 }T

\ ------------------------------------------------------------------------------
TESTING 2CONSTANT

T{ 1 2 2CONSTANT 2c1 -> }T
T{ 2c1 -> 1 2 }T
T{ : cd1 2c1 ; -> }T
T{ cd1 -> 1 2 }T
T{ : cd2 2CONSTANT ; -> }T
T{ -1 -2 cd2 2c2 -> }T
T{ 2c2 -> -1 -2 }T
T{ 4 5 2CONSTANT 2c3 IMMEDIATE 2c3 -> 4 5 }T
T{ : cd6 2c3 2LITERAL ; cd6 -> 4 5 }T

\ ------------------------------------------------------------------------------
\ Some 2CONSTANTs for the following tests

1sd max-intd 2CONSTANT max-2int  \ 01...1
0   min-intd 2CONSTANT min-2int  \ 10...0
max-2int 2/  2CONSTANT hi-2int   \ 001...1
min-2int 2/  2CONSTANT lo-2int   \ 110...0

\ ------------------------------------------------------------------------------
TESTING DNEGATE

T{ 0. DNEGATE -> 0. }T
T{ 1. DNEGATE -> -1. }T
T{ -1. DNEGATE -> 1. }T
T{ max-2int DNEGATE -> min-2int SWAP 1+ SWAP }T
T{ min-2int SWAP 1+ SWAP DNEGATE -> max-2int }T

\ ------------------------------------------------------------------------------
TESTING D+ with small integers

T{  0.  5. D+ ->  5. }T
T{ -5.  0. D+ -> -5. }T
T{  1.  2. D+ ->  3. }T
T{  1. -2. D+ -> -1. }T
T{ -1.  2. D+ ->  1. }T
T{ -1. -2. D+ -> -3. }T
T{ -1.  1. D+ ->  0. }T

TESTING D+ with mid range integers

T{  0  0  0  5 D+ ->  0  5 }T
T{ -1  5  0  0 D+ -> -1  5 }T
T{  0  0  0 -5 D+ ->  0 -5 }T
T{  0 -5 -1  0 D+ -> -1 -5 }T
T{  0  1  0  2 D+ ->  0  3 }T
T{ -1  1  0 -2 D+ -> -1 -1 }T
T{  0 -1  0  2 D+ ->  0  1 }T
T{  0 -1 -1 -2 D+ -> -1 -3 }T
T{ -1 -1  0  1 D+ -> -1  0 }T
T{ min-intd 0 2DUP D+ -> 0 1 }T
T{ min-intd S>D min-intd 0 D+ -> 0 0 }T

TESTING D+ with large double integers

T{ hi-2int 1. D+ -> 0 hi-int 1+ }T
T{ hi-2int 2DUP D+ -> 1sd 1- max-intd }T
T{ max-2int min-2int D+ -> -1. }T
T{ max-2int lo-2int D+ -> hi-2int }T
T{ hi-2int min-2int D+ 1. D+ -> lo-2int }T
T{ lo-2int 2DUP D+ -> min-2int }T

\ ------------------------------------------------------------------------------
TESTING D- with small integers

T{  0.  5. D- -> -5. }T
T{  5.  0. D- ->  5. }T
T{  0. -5. D- ->  5. }T
T{  1.  2. D- -> -1. }T
T{  1. -2. D- ->  3. }T
T{ -1.  2. D- -> -3. }T
T{ -1. -2. D- ->  1. }T
T{ -1. -1. D- ->  0. }T

TESTING D- with mid-range integers

T{  0  0  0  5 D- ->  0 -5 }T
T{ -1  5  0  0 D- -> -1  5 }T
T{  0  0 -1 -5 D- ->  1  4 }T
T{  0 -5  0  0 D- ->  0 -5 }T
T{ -1  1  0  2 D- -> -1 -1 }T
T{  0  1 -1 -2 D- ->  1  2 }T
T{  0 -1  0  2 D- ->  0 -3 }T
T{  0 -1  0 -2 D- ->  0  1 }T
T{  0  0  0  1 D- ->  0 -1 }T
T{ min-intd 0 2DUP D- -> 0. }T
T{ min-intd S>D max-intd 0 D- -> 1 1sd }T

TESTING D- with large integers

T{ max-2int max-2int D- -> 0. }T
T{ min-2int min-2int D- -> 0. }T
T{ max-2int hi-2int  D- -> lo-2int DNEGATE }T
T{ hi-2int  lo-2int  D- -> max-2int }T
T{ lo-2int  hi-2int  D- -> min-2int 1. D+ }T
T{ min-2int min-2int D- -> 0. }T
T{ min-2int lo-2int  D- -> lo-2int }T

\ ------------------------------------------------------------------------------
TESTING D0< D0=

T{ 0. D0< -> FALSE }T
T{ 1. D0< -> FALSE }T
T{ min-intd 0 D0< -> FALSE }T
T{ 0 max-intd D0< -> FALSE }T
T{ max-2int  D0< -> FALSE }T
T{ -1. D0< -> TRUE }T
T{ min-2int D0< -> TRUE }T

T{ 1. D0= -> FALSE }T
T{ min-intd 0 D0= -> FALSE }T
T{ max-2int  D0= -> FALSE }T
T{ -1 max-intd D0= -> FALSE }T
T{ 0. D0= -> TRUE }T
T{ -1. D0= -> FALSE }T
T{ 0 min-intd D0= -> FALSE }T

\ ------------------------------------------------------------------------------
TESTING D2* D2/

T{ 0. D2* -> 0. D2* }T
T{ min-intd 0 D2* -> 0 1 }T
T{ hi-2int D2* -> max-2int 1. D- }T
T{ lo-2int D2* -> min-2int }T

T{ 0. D2/ -> 0. }T
T{ 1. D2/ -> 0. }T
T{ 0 1 D2/ -> min-intd 0 }T
T{ max-2int D2/ -> hi-2int }T
T{ -1. D2/ -> -1. }T
T{ min-2int D2/ -> lo-2int }T

\ ------------------------------------------------------------------------------
TESTING D< D=

T{  0.  1. D< -> TRUE  }T
T{  0.  0. D< -> FALSE }T
T{  1.  0. D< -> FALSE }T
T{ -1.  1. D< -> TRUE  }T
T{ -1.  0. D< -> TRUE  }T
T{ -2. -1. D< -> TRUE  }T
T{ -1. -2. D< -> FALSE }T
T{ -1. max-2int D< -> TRUE }T
T{ min-2int max-2int D< -> TRUE }T
T{ max-2int -1. D< -> FALSE }T
T{ max-2int min-2int D< -> FALSE }T
T{ max-2int 2DUP -1. D+ D< -> FALSE }T
T{ min-2int 2DUP  1. D+ D< -> TRUE  }T

T{ -1. -1. D= -> TRUE  }T
T{ -1.  0. D= -> FALSE }T
T{ -1.  1. D= -> FALSE }T
T{  0. -1. D= -> FALSE }T
T{  0.  0. D= -> TRUE  }T
T{  0.  1. D= -> FALSE }T
T{  1. -1. D= -> FALSE }T
T{  1.  0. D= -> FALSE }T
T{  1.  1. D= -> TRUE  }T

T{ 0 -1 0 -1 D= -> TRUE  }T
T{ 0 -1 0  0 D= -> FALSE }T
T{ 0 -1 0  1 D= -> FALSE }T
T{ 0  0 0 -1 D= -> FALSE }T
T{ 0  0 0  0 D= -> TRUE  }T
T{ 0  0 0  1 D= -> FALSE }T
T{ 0  1 0 -1 D= -> FALSE }T
T{ 0  1 0  0 D= -> FALSE }T
T{ 0  1 0  1 D= -> TRUE  }T

T{ max-2int min-2int D= -> FALSE }T
T{ max-2int 0. D= -> FALSE }T
T{ max-2int max-2int D= -> TRUE }T
T{ max-2int hi-2int  D= -> FALSE }T
T{ max-2int min-2int D= -> FALSE }T
T{ min-2int min-2int D= -> TRUE }T
T{ min-2int lo-2int  D=  -> FALSE }T
T{ min-2int max-2int D= -> FALSE }T

\ ------------------------------------------------------------------------------
TESTING 2LITERAL 2VARIABLE

T{ : cd3 [ max-2int ] 2LITERAL ; -> }T
T{ cd3 -> max-2int }T
T{ 2VARIABLE 2v1 -> }T
T{ 0. 2v1 2! -> }T
T{ 2v1 2@ -> 0. }T
T{ -1 -2 2v1 2! -> }T
T{ 2v1 2@ -> -1 -2 }T
T{ : cd4 2VARIABLE ; -> }T
T{ cd4 2v2 -> }T
T{ : cd5 2v2 2! ; -> }T
T{ -2 -1 cd5 -> }T
T{ 2v2 2@ -> -2 -1 }T
T{ 2VARIABLE 2v3 IMMEDIATE 5 6 2v3 2! -> }T
T{ 2v3 2@ -> 5 6 }T
T{ : cd7 2v3 [ 2@ ] 2LITERAL ; cd7 -> 5 6 }T
T{ : cd8 [ 6 7 ] 2v3 [ 2! ] ; 2v3 2@ -> 6 7 }T

\ ------------------------------------------------------------------------------
TESTING DMAX DMIN

T{  1.  2. DMAX -> 2. }T
T{  1.  0. DMAX -> 1. }T
T{  1. -1. DMAX -> 1. }T
T{  1.  1. DMAX -> 1. }T
T{  0.  1. DMAX -> 1. }T
T{  0. -1. DMAX -> 0. }T
T{ -1.  1. DMAX -> 1. }T
T{ -1. -2. DMAX -> -1. }T

T{ max-2int hi-2int  DMAX -> max-2int }T
T{ max-2int min-2int DMAX -> max-2int }T
T{ min-2int max-2int DMAX -> max-2int }T
T{ min-2int lo-2int  DMAX -> lo-2int  }T

T{ max-2int  1. DMAX -> max-2int }T
T{ max-2int -1. DMAX -> max-2int }T
T{ min-2int  1. DMAX ->  1. }T
T{ min-2int -1. DMAX -> -1. }T


T{  1.  2. DMIN ->  1. }T
T{  1.  0. DMIN ->  0. }T
T{  1. -1. DMIN -> -1. }T
T{  1.  1. DMIN ->  1. }T
T{  0.  1. DMIN ->  0. }T
T{  0. -1. DMIN -> -1. }T
T{ -1.  1. DMIN -> -1. }T
T{ -1. -2. DMIN -> -2. }T

T{ max-2int hi-2int  DMIN -> hi-2int  }T
T{ max-2int min-2int DMIN -> min-2int }T
T{ min-2int max-2int DMIN -> min-2int }T
T{ min-2int lo-2int  DMIN -> min-2int }T

T{ max-2int  1. DMIN ->  1. }T
T{ max-2int -1. DMIN -> -1. }T
T{ min-2int  1. DMIN -> min-2int }T
T{ min-2int -1. DMIN -> min-2int }T

\ ------------------------------------------------------------------------------
TESTING D>S DABS

T{  1234  0 D>S ->  1234 }T
T{ -1234 -1 D>S -> -1234 }T
T{ max-intd  0 D>S -> max-intd }T
T{ min-intd -1 D>S -> min-intd }T

T{  1. DABS -> 1. }T
T{ -1. DABS -> 1. }T
T{ max-2int DABS -> max-2int }T
T{ min-2int 1. D+ DABS -> max-2int }T

\ ------------------------------------------------------------------------------
TESTING M+ M*/

T{ hi-2int   1 M+ -> hi-2int   1. D+ }T
T{ max-2int -1 M+ -> max-2int -1. D+ }T
T{ min-2int  1 M+ -> min-2int  1. D+ }T
T{ lo-2int  -1 M+ -> lo-2int  -1. D+ }T

\ To correct the result if the division is floored, only used when
\ necessary i.e. negative quotient and remainder <> 0

: ?floored [ -3 2 / -2 = ] LITERAL IF 1. D- THEN ;

T{  5.  7 11 M*/ ->  3. }T
T{  5. -7 11 M*/ -> -3. ?floored }T    \ floored -4.
T{ -5.  7 11 M*/ -> -3. ?floored }T    \ floored -4.
T{ -5. -7 11 M*/ ->  3. }T
T{ max-2int  8 16 M*/ -> hi-2int }T
T{ max-2int -8 16 M*/ -> hi-2int DNEGATE ?floored }T  \ floored subtract 1
T{ min-2int  8 16 M*/ -> lo-2int }T
T{ min-2int -8 16 M*/ -> lo-2int DNEGATE }T
T{ max-2int max-intd max-intd M*/ -> max-2int }T
T{ max-2int max-intd 2/ max-intd M*/ -> max-intd 1- hi-2int NIP }T
T{ min-2int lo-2int NIP DUP NEGATE M*/ -> min-2int }T
T{ min-2int lo-2int NIP 1- max-intd M*/ -> min-intd 3 + hi-2int NIP 2 + }T
T{ max-2int lo-2int NIP DUP NEGATE M*/ -> max-2int DNEGATE }T
T{ min-2int max-intd DUP M*/ -> min-2int }T

\ ------------------------------------------------------------------------------
TESTING D. D.R

\ Create some large double numbers
max-2int 71 73 M*/ 2CONSTANT dbl1
min-2int 73 79 M*/ 2CONSTANT dbl2

: d>ascii  ( d -- caddr u )
   DUP >R <# DABS #S R> SIGN #>    ( -- caddr1 u )
   HERE SWAP 2DUP 2>R CHARS DUP ALLOT MOVE 2R>
;

dbl1 d>ascii 2CONSTANT "dbl1"
dbl2 d>ascii 2CONSTANT "dbl2"

: DoubleOutput
   CR ." You should see lines duplicated:" CR
   5 SPACES "dbl1" TYPE CR
   5 SPACES dbl1 D. CR
   8 SPACES "dbl1" DUP >R TYPE CR
   5 SPACES dbl1 R> 3 + D.R CR
   5 SPACES "dbl2" TYPE CR
   5 SPACES dbl2 D. CR
   10 SPACES "dbl2" DUP >R TYPE CR
   5 SPACES dbl2 R> 5 + D.R CR
;

T{ DoubleOutput -> }T

\ ------------------------------------------------------------------------------
TESTING 2ROT DU< (Double Number extension words)

T{ 1. 2. 3. 2ROT -> 2. 3. 1. }T
T{ max-2int min-2int 1. 2ROT -> min-2int 1. max-2int }T

T{  1.  1. DU< -> FALSE }T
T{  1. -1. DU< -> TRUE  }T
T{ -1.  1. DU< -> FALSE }T
T{ -1. -2. DU< -> FALSE }T

T{ max-2int hi-2int  DU< -> FALSE }T
T{ hi-2int  max-2int DU< -> TRUE  }T
T{ max-2int min-2int DU< -> TRUE }T
T{ min-2int max-2int DU< -> FALSE }T
T{ min-2int lo-2int  DU< -> TRUE }T

\ ------------------------------------------------------------------------------

CR .( End of Double-Number word tests) CR

