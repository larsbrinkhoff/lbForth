#!/usr/bin/env bash

# This is, obviously, not the real Gradle wrapper.  It's only used as
# the build entry point by Ship.io.

sh install-deps.sh install_osx
gnumake check M32=
zip outputs forth
exit 0
