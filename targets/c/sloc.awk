#!/usr/bin/awk -f

# Count Forth and C source code lines.
# Typical usage: sloc.awk kernel.fth

BEGIN		{ state=0; forth=0; c=0 }
$1=="end-code"	{ state=0  }
		{ if (state) c++; else forth++; }
$1=="code"	{ state=1 }
END		{ print "Forth:", forth; print "C:", c }
