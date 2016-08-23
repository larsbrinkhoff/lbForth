which lsb_release && lsb_release -ds

install_linux() {
  sudo apt-get update -yqqm
  sudo apt-get install -ym ${LISP:-sbcl}
  test -z "$M32" || sudo apt-get install -y gcc-multilib
  case "$TARGET" in
    arm|m68k) sudo apt-get install qemu-user;;
    pdp11) download_apout;;
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

download_apout() {
  apout=apout2.3beta1
  wget http://www.tuhs.org/Archive/PDP-11/Emulators/Apout/$apout.tar.gz
  tar xzf $apout.tar.gz
  (cd $apout && make && sudo make install) || echo error ignored
}

install_osx() {
  brew update > /dev/null
  brew install ${LISP:-clozure-cl}
}

$1
