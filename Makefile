CC = gcc
M32 = -m32
CFLAGS = -g $(M32) -O2 -fomit-frame-pointer -fno-unit-at-a-time -I. -Itargets/c
LDFLAGS = -g $(M32)

boot = targets/c/meta.lisp
meta = targets/c/meta.fth
nucleus = targets/c/nucleus.fth

all: forth

forth: kernel.o
	$(CC) $(LDFLAGS) $^ -o $@

kernel.o: kernel.c kernel.h targets/c/forth.h

%.c %.h: %.fth c.fth $(nucleus) $(boot) params.lisp
	./lisp.sh '(progn (load "$(boot)") (compile-forth "$(nucleus)" "$<"))'

params.lisp: params
	./$< -lisp > $@

params.fth: params
	./$< -forth > $@

params: targets/c/params.c Makefile
	$(CC) $(CFLAGS) $< -o $@

clean:
	rm -f forth *.o kernel.c kernel2.c kernel.h params*

kernel2.c: forth kernel.fth c.fth params.fth $(nucleus) $(meta)
	echo 'include $(meta) bye' | ./forth > $@
