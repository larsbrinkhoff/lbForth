#!/bin/sh

case "$CC" in
  i686*cygwin*)
    CYGWIN=`echo $PWD/packages/Cygwin.*`
    TPATH=$CYGWIN/tools/usr/i686-pc-cygwin/sys-root/usr/bin ;;
  *)
    TPATH="$PATH" ;;
esac

PATH="$TPATH" "$@"
