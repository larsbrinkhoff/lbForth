#!/bin/sh

FORM="$1"

try() {
    if type $1 > /dev/null 2>&1; then
	$1 $2 "$FORM"
	exit 0
    fi
}

try sbcl "--noinform --eval"
try clisp "-q -x"
try ecl "-eval"
try ccl "--eval"

echo No Lisp found.
exit 1
