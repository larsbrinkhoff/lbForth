#!/bin/sh

case "$OS" in
    tos)
	# http://github.com/e8johan/tosemu
	../tosemu/bin/tosemu "$@" ;;
    linux)
	qemu-m68k "$@" ;;
esac
