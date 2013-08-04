CC = gcc
M32 = -m32
CFLAGS = $(M32) -O2 -fomit-frame-pointer -fno-unit-at-a-time
CPPFLAGS = -I$(TARGET)
LDFLAGS = $(M32)

TARGET = targets/c
meta = $(TARGET)/meta.lisp
nucleus = $(TARGET)/nucleus.fth

# Bootstrap metacompiler, written in Lisp.
%.c: %.fth
	./lisp.sh '(progn (load "$(meta)") (compile-forth "$(nucleus)" "$<"))'

all: forth

forth: kernel.o
	$(CC) $(LDFLAGS) $^ -o $@
	touch .bootstrap $(TARGET)/forth.h

kernel.o: kernel.c $(TARGET)/forth.h

kernel.c: kernel.fth c.fth $(nucleus) $(meta) params.lisp

params.lisp: params
	./$< -lisp > $@

params: $(TARGET)/params.c $(TARGET)/forth.h Makefile
	$(CC) $(CFLAGS) $(CPPFLAGS) $< -o $@
