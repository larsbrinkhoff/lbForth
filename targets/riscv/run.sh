#!/bin/sh

QEMU=riscv-qemu/riscv32-linux-user/qemu-riscv32

case `uname -m` in
    riscv*) "$@" ;;
    *) $QEMU "$@" ;;
esac
