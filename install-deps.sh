which lsb_release && lsb_release -ds

install_linux() {
  sudo apt-get update -yqqm
  sudo apt-get install -ym ${LISP:-sbcl}
  test -z "$M32" || sudo apt-get install -y gcc-multilib
  case "$TARGET-$OS" in
    x86-linux) ;;
    *-linux) sudo apt-get install qemu-user;;
    m68k-tos) download_tosemu;;
    pdp11-unix) download_apout;;
    avr-*) sudo apt-get install simulavr;;
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

download_tosemu() {
  git clone http://github.com/e8johan/tosemu
  (cd tosemu && make && sudo install bin/tosemu /usr/local/bin)
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
