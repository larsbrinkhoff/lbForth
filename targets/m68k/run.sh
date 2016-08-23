#!/bin/sh

case "$OS" in
    tos)
	tosemu "$@" ;;
    linux)
	qemu-m68k "$@" ;;
esac
