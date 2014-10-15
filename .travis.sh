install_linux() {
  sudo apt-get update
  sudo apt-get install sbcl
  test -z "$M32" || sudo apt-get install gcc-multilib
}

install_osx() {
  brew update
  brew install clozure-cl
}

$1
