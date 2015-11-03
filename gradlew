#!/usr/bin/env bash

# This is, obviously, not the real Gradle wrapper.  It's only used as
# the build entry point by BuildHive.

build_buildhive() {
    sh install-deps.sh download_sbcl
    export SBCL_HOME=$PWD/sbcl/lib/sbcl
    export PATH=$PATH:$PWD/sbcl/bin
    make all M32=
    make check
}

test "$JENKINS_URL" = "https://buildhive.cloudbees.com/" && build_buildhive

exit 0
