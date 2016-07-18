which lsb_release && lsb_release -ds

install_linux() {
  sudo apt-get update -yqqm
  sudo apt-get install -ym ${LISP:-sbcl}
  test -z "$M32" || sudo apt-get install -y gcc-multilib
  case "$TARGET" in
    arm|m68k) sudo apt-get install qemu-user;;
  esac
}

install_yum() {
  sudo yum install -y ${LISP:-sbcl}
}

download_sbcl() {
  test -d sbcl && return
  sbcl=sbcl-1.2.4-x86-64-linux
  wget http://prdownloads.sourceforge.net/sbcl/$sbcl-binary.tar.bz2
  tar xjf $sbcl-binary.tar.bz2
  (export INSTALL_ROOT=$PWD/sbcl && cd $sbcl && sh install.sh)
}

install_osx() {
  brew update > /dev/null
  brew install ${LISP:-clozure-cl}
}

$1
