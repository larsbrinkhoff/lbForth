This program is built and tested in the cloud-based continuous
integration services listed below.  For each service, there has to be
a way to install or download a Lisp compiler.  Also, on Windows,
Cygwin is used to provide a Unix build environment with make, gcc,
etc.

This documents the configuration for all services.  Some services use
files checked into the repository, others require information entered
into their web interface.

## [AppVeyor](http://appveyor.com/)
[![AppVeyor](https://ci.appveyor.com/api/projects/status/r8wuvf0n0obp3n14)](https://ci.appveyor.com/project/larsbrinkhoff/lbforth/history)  
OS: Windows  
File: [`appveyor.yml`](https://github.com/larsbrinkhoff/lbForth/blob/master/appveyor.yml)

## [Bitrise](http://bitrise.io/)
[![Bitrise](https://www.bitrise.io/app/663938c8cb3bee14.svg?token=34FFBj3CLaI1yWXqou5JEg&branch=master)](https://www.bitrise.io/app/663938c8cb3bee14)  
OS: MacOS X  
Install: `install-deps.sh install_osx`  
Build: `gnumake M32=`  
Test: `gnumake check M32=`

## [CircleCI](http://circleci.com/)
[![CircleCI](https://circleci.com/gh/larsbrinkhoff/lbForth.svg?style=svg)](https://circleci.com/gh/larsbrinkhoff/lbForth)  
OS: Linux: Ubuntu 12  
File: [`circle.yml`](https://github.com/larsbrinkhoff/lbForth/blob/master/circle.yml)

## [GitLab CI](http://gitlab.com/)
[![GitLab CI](https://gitlab.com/larsbrinkhoff/lbForth/badges/master/build.svg)](https://gitlab.com/larsbrinkhoff/lbForth/commits/master)  
OS: Linux: Ubuntu 12  
File: [`.gitlab-ci.yml`](https://github.com/larsbrinkhoff/lbForth/blob/master/.gitlab-ci.yml)

## [Scrutinizer](http://scrutinizer-ci.com/)
[![Scrutinizer](https://scrutinizer-ci.com/g/larsbrinkhoff/lbForth/badges/build.png)](https://scrutinizer-ci.com/g/larsbrinkhoff/lbForth/)  
OS: Linux: Ubuntu 14  
File: [`.scrutinizer.yml`](https://github.com/larsbrinkhoff/lbForth/blob/master/.scrutinizer.yml)

## [Semaphore](http://semaphoreci.com/)
[![Semaphore](https://semaphoreci.com/api/v1/projects/726d1f9e-ae3a-4ef6-b109-39b2eeef14b1/531496/badge.svg)](https://semaphoreci.com/larsbrinkhoff/lbforth)  
OS: Linux: Ubuntu 14  
Setup:

    git submodule update --init
    install-deps.sh install_linux

Build:

    make check M32=

Post thread:

    git submodule deinit --force .

## [Travis CI](http://travis-ci.org/)
[![Travis CI](https://travis-ci.org/larsbrinkhoff/lbForth.svg?branch=master)](https://travis-ci.org/larsbrinkhoff/lbForth)  
OS: Linux: Ubuntu 12, MacOS X  
File: [`.travis.yml`](https://github.com/larsbrinkhoff/lbForth/blob/master/.travis.yml)
