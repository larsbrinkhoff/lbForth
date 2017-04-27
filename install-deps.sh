which lsb_release && lsb_release -ds

if test -n "$GITLAB_CI"; then
  sudo() {
    "$@"
  }
fi

install_linux() {
  sudo apt-get update -yqqm
  sudo apt-get install -ym git make ${LISP:-sbcl}
  test -z "$M32" || sudo apt-get install -y gcc-multilib
  case "$TARGET-$OS" in
    x86-linux) ;;
    riscv-*) download_riscv_qemu;;
    *-linux) sudo apt-get install qemu-user;;
    m68k-tos) download_tosemu;;
    pdp11-unix) download_apout;;
    z80-cpm) download_cpm;;
    avr-*) sudo apt-get install simulavr;;
  esac
  case "$TARGET-$JS" in
    asmjs-js24) sudo apt-get install libmozjs-24-bin;;
    asmjs-nodejs) 
      wget -qO- https://deb.nodesource.com/setup_7.x | sudo -E bash -
      sudo apt-get install nodejs;;
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
  apout=http://github.com/DoctorWkt/Apout
  git clone $apout
  (cd Apout && make && sudo make install) || echo error ignored
}

download_riscv_qemu() {
  qemu=http://github.com/riscv/riscv-qemu
  git clone $qemu
  (cd riscv-qemu && git submodule update --init pixman &&
   ./configure --target-list=riscv32-linux-user && make)
}

download_cpm() {
  git clone https://github.com/jhallen/cpm
  (cd cpm && make && sudo cp cpm /usr/local/bin)
}

install_osx() {
  brew update > /dev/null
  brew install ${LISP:-clozure-cl}
}

$1
