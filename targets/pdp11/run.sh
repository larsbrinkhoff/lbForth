#!/bin/sh

export APOUT_ROOT=/

case `uname -m` in
    pdp*) "$@" ;;
    *)    apout "$@" ;;
esac
