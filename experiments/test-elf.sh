#!/bin/sh
echo "include experiments/test-elf.fth" | ./forth | tail -n+3 > TMP
chmod a+x TMP
./TMP
echo $?
