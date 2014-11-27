#!/bin/sh

case `uname` in
Linux)    FMT=elf;;
CYGWIN*)  FMT=pe;;
*)        echo "Unknown system"; exit 1;;
esac

echo "include experiments/test-$FMT.fth" | ./forth | tail -n+3 > TMP
chmod a+x TMP
./TMP
echo $?
