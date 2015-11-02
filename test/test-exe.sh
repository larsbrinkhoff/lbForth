#!/bin/sh

case `uname` in
Linux)    FMT=elf;;
CYGWIN*)  FMT=pe;;
*)        echo 42; exit 0;;
esac

echo "include test/test-$FMT.fth" | ./forth | tail -n+3 > TMP
chmod a+x TMP
./TMP
echo $?
exit 0
