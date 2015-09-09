#!/usr/bin/env bash

# This is, obviously, not the real Gradle wrapper.  It's only used as
# the build entry point by Ship.io and BuildHive.

build_shipio() {
    sh install-deps.sh install_osx
    gnumake check M32=
}

build_buildhive() {
    sh install-deps.sh download_sbcl
    export SBCL_HOME=$PWD/sbcl/lib/sbcl
    export PATH=$PATH:$PWD/sbcl/bin
    make all M32=
    make check
}

test "$SHIPIO" = "1" && build_shipio
test "$JENKINS_URL" = "https://buildhive.cloudbees.com/" && build_buildhive

exit 0
