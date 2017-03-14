( Subset of Forth94 )

This is a self-hosted implementation of Forth, which can regenerate
itself from Forth source code.  The bootstrapping process uses a
[metacompiler written in Lisp]
(https://github.com/larsbrinkhoff/forth-metacompiler) to target a
small inner interpreter and a handful of code words written in C.  A
new [metacompiler written in Forth](lib/meta.fth) generates an x86
executable using using [assembly language code words]
(targets/x86/nucleus.fth).

There are also ARM, Motorola 68000, and PDP-11 targets, and target
assemblers for 6502, AVR, MSP430, and Emacs bytecodes.

( Continuous integration )

The code is continuously built and tested in Linux, MacOS X, and
Windows using several cloud-based continuous integration services.
This is documented in [build.md](build.md).

( Further reading )

[INSTALL](INSTALL) \ How to build.  
[doc](doc) \ Classic (and recent) texts not related to this project.  
[lib/README](lib/README) \ Information about libraries.  
[targets/README.md](targets/README.md) \ Information about current and possibly future targets.

( Implementation guide )

The Forth kernel contains everything needed to read and compile the
rest of the system from source code, and not much else.  It's composed
of two parts: a target-specific file nucleus.fth containing all
primitive CODE words, and a [target-independent
kernel.fth](src/kernel.fth).  These two are compiled by the
metacompiler.

The [C target nucleus](targets/c/nucleus.fth) used for bootstrapping
has only twelve proper primitives.  There is also the COLD word which
compiles to main(), and four I/O words.

When the kernel starts, it jumps to the word called WARM.  This is
responsible for loading the rest of the system and entering the text
interpreter.  The first file loaded by WARM is [core.fth](src/core.fth),
which implements the CORE wordset.  Because the kernel only has a bare
minimum of words, the start of core.fth looks a little strange.
