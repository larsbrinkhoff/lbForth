install_linux() {
  lsb_release -ds
  sudo apt-get update -yqq
  sudo apt-get install -y sbcl
  test -z "$M32" || sudo apt-get install -y gcc-multilib
}

install_osx() {
  brew update
  brew install clozure-cl
}

$1
