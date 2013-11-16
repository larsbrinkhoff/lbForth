\ ANS Forth tests - run all tests

\ Adjust the file paths as appropriate to your system
\ Select the appropriate test harness, either the simple tester.fth
\ or the more complex ttester.fth 

CR .( Running ANS Forth test programs, version 0.9) CR

	S" test/hayes/tester.fth" INCLUDED
\   S" ttester.fth" INCLUDED
	S" test/hayes/core.fth" INCLUDED
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
