CC = gcc
M32 = -m32
CFLAGS = -g $(M32) -O2 -fomit-frame-pointer -fno-unit-at-a-time -I. -Itargets/c
LDFLAGS = -g $(M32)

meta = targets/c/meta.lisp
nucleus = targets/c/nucleus.fth

all: forth

forth: kernel.o
	$(CC) $(LDFLAGS) $^ -o $@

kernel.o: kernel.c kernel.h targets/c/forth.h

%.c %.h: %.fth c.fth $(nucleus) $(meta) params.lisp
	./lisp.sh '(progn (load "$(meta)") (compile-forth "$(nucleus)" "$<"))'

params.lisp: params
	./$< -lisp > $@

params: targets/c/params.c Makefile
	$(CC) $(CFLAGS) -I. $< -o $@

clean:
	rm -f forth *.o kernel.c kernel.h params params.lisp
