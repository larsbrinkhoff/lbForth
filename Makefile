CC = gcc
CFLAGS = -g -m32 -fomit-frame-pointer -O2
LISP = sbcl
LDFLAGS = -g -m32 

all: forth

forth: kernel.o
	$(CC) $(LDFLAGS) $^ -o $@

kernel.o: kernel.c kernel.h forth.h

%.c %.h: %.fth meta.lisp params.lisp
	$(LISP) --load meta.lisp \
	        --eval '(progn (compile-forth "$<") (quit))'

params.lisp: params
	./$< > $@

params: params.c

clean:
	rm -f forth *.o kernel.c kernel.h params params.lisp
