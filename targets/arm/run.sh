#!/bin/sh

case `uname -m` in
    arm*) "$@" ;;
    *)    qemu-arm "$@" ;;
esac
