#!/bin/sh

ARCH=`uname -m`
SYSTEM=`uname`

case "$ARCH-$SYSTEM" in
x86*Linux)   FMT=elf;;
*CYGWIN*)    FMT=pe;;
*)           echo 42; exit 0;;
esac

echo "include test/test-$FMT.fth" | $FORTH | tail -n+3 > TMP
chmod a+x TMP
./TMP
echo $?
exit 0
