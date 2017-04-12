which lsb_release && lsb_release -ds

fix_repos() {
  which add-apt-repository || return 0

  # This server responds with an overly long Via header line.
  bad=http://us-central1.gce.archive.ubuntu.com/ubuntu/
  for i in 'main restricted' 'universe' 'multiverse'; do
    sudo add-apt-repository --remove "$bad $i"
  done
  sudo add-apt-repository http://archive.ubuntu.com/ubuntu/
  sudo add-apt-repository 'http://archive.ubuntu.com/ubuntu/ universe'
  sudo add-apt-repository 'http://archive.ubuntu.com/ubuntu/ multiverse'
}

if test -n "$GITLAB_CI"; then
  sudo() {
    "$@"
  }
fi

install_linux() {
  fix_repos

  sudo apt-get update -yqqm
  sudo apt-get install -ym git make ${LISP:-sbcl}
  test -z "$M32" || sudo apt-get install -y gcc-multilib
  case "$TARGET-$OS" in
    x86-linux) ;;
    *-linux) sudo apt-get install qemu-user;;
    m68k-tos) download_tosemu;;
    pdp11-unix) download_apout;;
    avr-*) sudo apt-get install simulavr;;
    asmjs-*) sudo apt-get install libmozjs-24-bin;;
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

install_osx() {
  brew update > /dev/null
  brew install ${LISP:-clozure-cl}
}

$1
