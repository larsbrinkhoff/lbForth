#!/bin/sh

if test -z "$JS"; then
    JS=js
    type js24 > /dev/null 2>&1 && JS="js24 -W"
fi

$JS "$@"
