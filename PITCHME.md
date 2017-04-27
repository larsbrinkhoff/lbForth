# lbForth

A self-hosting portable Forth

http://github.com/larsbrinkhoff/lbForth

---

# Self-hosting

- Rebuilds itself using a compiler written in Forth
- Primitives written in assembly language
- Assemblers written in Forth
- Outputs a self-contained executable file

---

# Portable

- 64-bit, 32-bit, 16-bit
- Little, big endian
- x86, ARM, RISC-V, 68000, PDP-11, asm.js
- Linux, Windows, Unix, TOS

---

# Simplified

- Based on 12 required primitives
- Minimal kernel
- Most of the system is loaded and compiled on startup

---

# Small

- Kernel: 800 lines
- System: 600 lines
- Compiler: 400 lines
- Assembler: 300 lines

---

# Bootstrapped

- Bootstraps without any Forth
- Compiler written in Lisp
- Primitives written in C
- Builds with C compiler

---

# CONTEMPORARY

- Subset of 1994 and 2012 standards
- Builds with common tools
- Available from GitHub
- Built and tested in continuous integration services

---

# RETRO

- Atari ST
- PDP-11 Unix
- Block editor

---

## UPCOMING

- Microcontrollers:  
  AVR, PIC, MSP430, Thumb, Xtensa, STM8, 8051
- Retro: PDP-10, C64, Z80 CP/M, 6809, 1802
- Optimizing compiler
- Run inside Emacs

---

# TRY IT

- ./configure && make && ./forth
- http://larsbrinkhoff.github.io/lbForth/forth.html
