#!/bin/sh
echo "include experiments/test-elf.fth bye" | ./forth | tail -n+3 > TMP
chmod a+x TMP
./TMP
echo $?
