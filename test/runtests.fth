\ ANS Forth tests - run all tests

\ Adjust the file paths as appropriate to your system
\ Select the appropriate test harness, either the simple tester.fr
\ or the more complex tester.fs 

CR .( Running ANS Forth test programs, version 0.9) CR

	S" test/tester.fr" INCLUDED
\   S" ttester.fs" INCLUDED
	S" test/core.fr" INCLUDED
   S" coreplustest.fth" INCLUDED
\	S" coreexttest.fth" INCLUDED
\	S" doubletest.fth" INCLUDED
\	S" exceptiontest.fth" INCLUDED
\	S" filetest.fth" INCLUDED
\	S" memorytest.fth" INCLUDED
\	S" toolstest.fth" INCLUDED
\	S" searchordertest.fth" INCLUDED
\	S" stringtest.fth" INCLUDED

CR CR .( Forth tests completed ) CR CR


