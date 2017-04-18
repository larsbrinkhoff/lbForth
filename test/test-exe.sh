#!/bin/sh

ARCH=`uname -m`
SYSTEM=`uname`

case "$ARCH-$SYSTEM-$TARGET" in
x86*Linux*x86)   FMT=elf;;
*CYGWIN*x86)     FMT=pe;;
*)               echo 42; exit 0;;
esac

echo "include test/test-$FMT.fth" | $FORTH | tail -n+3 > TMP
chmod a+x TMP
./TMP
echo $?
exit 0
